#!/usr/bin/env bash
# monitor_errores_ultima_hora.sh
# Diagnóstico de monitores/GPU con hallazgos claros y acciones sugeridas.

if [ -z "${BASH_VERSION:-}" ]; then
  exec /usr/bin/env bash "$0" "$@"
fi
set -euo pipefail

usage() {
  cat <<'EOF'
Uso:
  ./monitor_errores_ultima_hora.sh [--detallado] [SINCE] [TOPN] [TAILN]

Parámetros:
  --detallado  Muestra logs técnicos completos (por defecto: resumen claro)
  SINCE  Rango para journalctl (default: "1 hour ago")
  TOPN   Cantidad de mensajes únicos por sección (default: 15)
  TAILN  Máximo de líneas a considerar por sección (default: 400)

Ejemplos:
  ./monitor_errores_ultima_hora.sh
  ./monitor_errores_ultima_hora.sh --detallado
  ./monitor_errores_ultima_hora.sh "2 hours ago" 20 600
  ./monitor_errores_ultima_hora.sh "today"
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

VERBOSE=0
POSITIONAL=()
for arg in "$@"; do
  case "$arg" in
    --detallado|--verbose) VERBOSE=1 ;;
    *) POSITIONAL+=("$arg") ;;
  esac
done

SINCE="${POSITIONAL[0]:-1 hour ago}"
TOPN="${POSITIONAL[1]:-15}"
TAILN="${POSITIONAL[2]:-400}"

RED=$'\033[31m'; YEL=$'\033[33m'; GRN=$'\033[32m'; BLU=$'\033[34m'; DIM=$'\033[2m'; RST=$'\033[0m'
HOST="$(hostname)"
NOW="$(date '+%Y-%m-%d %H:%M:%S')"

# Señales GPU/monitor
PAT_GPU='drm|i915|amdgpu|radeon|nouveau|nvidia|nvidia-modeset|nvidia-drm|NVRM|Xid'
PAT_DISP='edid|ddc|hpd|hotplug|displayport|dp-|hdmi|connector|link training|modeset|mode setting|atomic|crtc|vblank|flip|pageflip|underrun|overrun|framebuffer|kms|panel|backlight|eDP'
PAT_BAD='error|failed|failure|timeout|timed out|hang|reset|stuck|BUG|oops|panic|segfault|assert|invalid|corrupt'
PAT_FREEZE='freeze|frozen|hung|stuck|soft lockup|hard lockup|watchdog|blocked for more than|gpu reset|ring.*stuck|recover'

# “Críticos” (si salen, subir severidad)
PAT_CRIT='Xid|GPU HANG|flip_done timed out|EDID.*failed|link training.*failed|modeset.*failed|NVRM: .*error|rm error|rpcSendMessage failed'

# Ruido
NOISE='xkbcomp|Could not resolve keysym|keycodes above 255|search provider org\.gnome\.Terminal|No entry for geolocation|/dev/input/event|containerd|NetworkManager|gvfs-|bluetooth|hci0|sof-hda-dsp|hda_dsp_hdmi|topology|typec port|pciehp|permission-dialog-launcher\.py|snapd-prompting'

hr() { echo "${DIM}------------------------------------------------------------${RST}"; }

# Normaliza una línea de journalctl removiendo:
# - timestamp inicial
# - hostname
# - unidad[pid]:
normalize_line() {
  sed -E \
    -e 's/^[0-9]{4}-[0-9]{2}-[0-9]{2}T[^ ]+ [^ ]+ //' \
    -e 's/^[^ ]+ //' \
    -e 's/^[^:]+: //'
}

# Top únicos: cuenta por mensaje normalizado (sin timestamp/host/pid)
top_unique() {
  normalize_line | awk '{c[$0]++} END{for (m in c) printf "%6d  %s\n", c[m], m}' | sort -nr | head -n "$TOPN"
}

collect_cmd_output() {
  local cmd=( "$@" )
  set +e
  local out
  out="$("${cmd[@]}" 2>/dev/null | tail -n "$TAILN")"
  local rc=$?
  set -e
  if [[ $rc -ne 0 ]]; then
    echo ""
  else
    echo "$out"
  fi
}

max_sev() {
  local a="$1"
  local b="$2"
  case "$a:$b" in
    CRIT:*|*:CRIT) echo "CRIT" ;;
    WARN:*|*:WARN) echo "WARN" ;;
    *) echo "OK" ;;
  esac
}

FINAL_SEV="OK"
FINDINGS=""
SOLUTIONS=""
FINDINGS_COUNT=0
FIRST_TITLE=""
FIRST_EVIDENCE=""
FIRST_ACTIONS=""

add_finding() {
  local sev="$1"
  local title="$2"
  local evidence="$3"
  local actions="$4"

  FINAL_SEV="$(max_sev "$FINAL_SEV" "$sev")"
  FINDINGS_COUNT=$((FINDINGS_COUNT + 1))
  if [[ -z "$FIRST_TITLE" ]]; then
    FIRST_TITLE="$title"
    FIRST_EVIDENCE="$evidence"
    FIRST_ACTIONS="$actions"
  fi
  FINDINGS+="- [$sev] $title"$'\n'
  FINDINGS+="  Evidencia: $evidence"$'\n'
  SOLUTIONS+="- $title"$'\n'"  $actions"$'\n'
}

detect_issue() {
  local source_txt="$1"
  local regex="$2"
  local sev="$3"
  local title="$4"
  local actions="$5"
  local match

  if match="$(printf '%s\n' "$source_txt" | grep -Eai "$regex" | tail -n 1)"; then
    [[ -n "${match//[[:space:]]/}" ]] || return 0
    add_finding "$sev" "$title" "$match" "$actions"
  fi
}

section() {
  local title="$1"
  local raw="$2"

  [[ "$VERBOSE" -eq 1 ]] || return 0

  echo "${GRN}== $title ==${RST}"

  if [[ -z "${raw//[[:space:]]/}" ]]; then
    echo "${DIM}Sin eventos relevantes.${RST}"
    echo
    return 0
  fi

  echo "${BLU}Top mensajes (únicos)${RST}"
  echo "$raw" | top_unique || true
  echo
  echo "${BLU}Muestras recientes${RST}"
  echo "$raw" | tail -n 20
  echo
}

echo "${BLU}== Diagnóstico monitores/GPU (desde \"$SINCE\") ==${RST}"
echo "${DIM}Host: $HOST | Ahora: $NOW${RST}"
echo

# Conectores
CONNECT_SUMMARY=""
CONNECTED_COUNT=0
if ls /sys/class/drm/card*-*/status >/dev/null 2>&1; then
  for f in /sys/class/drm/card*-*/status; do
    [[ -f "$f" ]] || continue
    connector="${f%/status}"
    status="$(cat "$f")"
    CONNECT_SUMMARY+="$connector: $status"$'\n'
    if [[ "$status" == "connected" ]]; then
      CONNECTED_COUNT=$((CONNECTED_COUNT + 1))
    fi
  done
else
  CONNECT_SUMMARY="No se pudo leer /sys/class/drm/*/status"
fi

if [[ "$VERBOSE" -eq 1 ]]; then
  echo "${GRN}== Conectores actuales (DRM) ==${RST}"
  printf '%s\n' "$CONNECT_SUMMARY"
  echo
  hr
fi

# 1) NVIDIA (servicio + kernel NVRM/Xid)
NVIDIA_RAW="$(collect_cmd_output bash -lc "
  { journalctl -u nvidia-powerd --since \"$SINCE\" --no-pager -o short-iso 2>/dev/null || true; }
  { journalctl -k --since \"$SINCE\" --no-pager -o short-iso 2>/dev/null | grep -Eai 'NVRM|Xid|nvidia' || true; }
")"
NVIDIA_FILT="$(printf '%s\n' "$NVIDIA_RAW" | grep -Eai 'ERROR|Failed|NVRM|Xid|rpc|rm error|timeout|hang' | grep -Evai "($NOISE)" || true)"

section "NVIDIA (nvidia-powerd / NVRM / Xid)" "$NVIDIA_FILT"

[[ "$VERBOSE" -eq 1 ]] && hr

# 2) Kernel display real (DRM + errores)
KERNEL_FILT="$(collect_cmd_output bash -lc "
  journalctl -k --since \"$SINCE\" --no-pager -o short-iso -p info..alert 2>/dev/null \
  | grep -Eai \"($PAT_GPU)\" \
  | grep -Eai \"($PAT_DISP|$PAT_BAD)\" \
  | grep -Evai \"($NOISE)\"
")"
section "Kernel (display real: DRM/EDID/HPD/link training/modeset)" "$KERNEL_FILT"

[[ "$VERBOSE" -eq 1 ]] && hr

# 3) GNOME/Mutter (solo display-ish)
SESSION_FILT="$(collect_cmd_output bash -lc "
  journalctl --since \"$SINCE\" --no-pager -o short-iso 2>/dev/null \
  | grep -Eai 'gnome-shell|mutter|wayland|xwayland' \
  | grep -Eai 'vsync|frame|monitor|output|connector|kms|drm|modeset|invalid|assert|failed|timeout' \
  | grep -Evai \"($NOISE)\"
")"
section "Sesión (GNOME/Mutter/Wayland) relevante" "$SESSION_FILT"

[[ "$VERBOSE" -eq 1 ]] && hr

# 4) Señales de congelamiento (fuentes amplias)
FREEZE_FILT="$(collect_cmd_output bash -lc "
  journalctl --since \"$SINCE\" --no-pager -o short-iso 2>/dev/null \
  | grep -Eai 'kernel|gnome-shell|mutter|nvidia|amdgpu|i915|drm|Xorg|gdm' \
  | grep -Eai 'drm|nvidia|i915|amdgpu|radeon|nouveau|gnome-shell|mutter|wayland|xorg|display|monitor|kms|modeset|vsync|pageflip|crtc|edid|hpd|gpu' \
  | grep -Eai \"($PAT_FREEZE|$PAT_CRIT|$PAT_BAD|$PAT_DISP)\" \
  | grep -Evai 'audit:|apparmor|systemd-detect-' \
  | grep -Evai \"($NOISE)\"
")"
section "Congelamientos / cuelgues (kernel + sesion)" "$FREEZE_FILT"

[[ "$VERBOSE" -eq 1 ]] && hr

# Hallazgos + posibles soluciones
ALL_TXT="$NVIDIA_FILT"$'\n'"$KERNEL_FILT"$'\n'"$SESSION_FILT"$'\n'"$FREEZE_FILT"

detect_issue "$ALL_TXT" 'Xid|NVRM: .*error|GPU HANG|rm error' "CRIT" \
  "Fallo de GPU NVIDIA (Xid/NVRM/GPU Hang)" \
  "Actualizar driver NVIDIA y kernel al ultimo estable; revisar temperatura/energia; probar con otro cable/puerto y sin dock; si persiste, capturar reporte con nvidia-bug-report.sh."

detect_issue "$ALL_TXT" 'EDID.*failed|failed to read edid|no edid' "WARN" \
  "Problema leyendo EDID del monitor" \
  "Cambiar cable (preferir corto y certificado), probar otro puerto (HDMI/DP), apagar/encender monitor y revisar adaptadores; validar firmware del monitor/dock."

detect_issue "$ALL_TXT" 'link training.*failed|clock recovery failed|channel equalization failed' "CRIT" \
  "Fallo de DisplayPort Link Training" \
  "Suele ser capa física: cambiar cable/puerto, evitar adaptadores pasivos, bajar temporalmente resolución/Hz, actualizar firmware de dock/monitor/GPU."

detect_issue "$ALL_TXT" 'flip_done timed out|pageflip.*timeout|vblank.*timeout|modeset.*failed|crtc.*failed' "WARN" \
  "Timeout/fallo de modeset o pageflip" \
  "Probar sesión Xorg vs Wayland para aislar; desactivar VRR/FreeSync/G-Sync; reducir Hz temporalmente; actualizar Mesa/driver/kernel."

detect_issue "$ALL_TXT" 'hotplug|hpd' "WARN" \
  "Reconexiones frecuentes (hotplug/HPD)" \
  "Revisar conectores flojos, dock/cables y energía del monitor; probar conexión directa sin dock/hub para descartar inestabilidad."

detect_issue "$ALL_TXT" 'mutter.*(assert|failed|invalid|timeout)|gnome-shell.*(assert|failed|timeout)' "WARN" \
  "Errores en GNOME/Mutter relacionados con display" \
  "Actualizar GNOME/Mutter, desactivar extensiones temporalmente y comparar con usuario limpio; si falla solo en Wayland, probar Xorg."

detect_issue "$ALL_TXT" 'nvidia-powerd.*(error|failed|rpc)' "WARN" \
  "Errores en servicio nvidia-powerd" \
  "Si no usas gestion dinamica de energia NVIDIA, deshabilitar nvidia-powerd; si lo necesitas, reinstalar driver y revisar version compatible."

detect_issue "$ALL_TXT" 'freeze|frozen|hung|soft lockup|hard lockup|blocked for more than|watchdog|gpu reset|ring.*stuck' "CRIT" \
  "Congelamiento detectado por logs (hang/lockup/watchdog)" \
  "Registrar hora exacta del freeze y revisar 2-5 minutos antes/despues en logs; probar sin dock, bajar Hz temporalmente y actualizar kernel/driver GPU."

meaning_from_title() {
  local title="$1"
  case "$title" in
    *"GNOME/Mutter"*)
      echo "El problema parece estar en la capa grafica de la sesion (no en dano fisico del monitor)."
      ;;
    *"GPU NVIDIA"*)
      echo "Hay senales de fallo del driver/GPU NVIDIA que si pueden causar congelamientos o pantallazos."
      ;;
    *"EDID"*)
      echo "La laptop/GPU no esta leyendo bien la informacion del monitor."
      ;;
    *"DisplayPort Link Training"*)
      echo "La comunicacion de video por DisplayPort esta fallando (normalmente cable/puerto/dock)."
      ;;
    *"modeset"*|*"pageflip"*)
      echo "Hubo tiempos de espera en el cambio de imagen del monitor."
      ;;
    *"hotplug/HPD"*)
      echo "El monitor parece conectarse y desconectarse solo por momentos."
      ;;
    *)
      echo "Se detectaron eventos de display que vale la pena corregir."
      ;;
  esac
}

extract_error_time() {
  local evidence="$1"
  local ts
  ts="$(printf '%s\n' "$evidence" | awk '{print $1}')"
  if [[ "$ts" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T ]]; then
    echo "$ts"
  else
    echo "No disponible"
  fi
}

build_timeline() {
  local txt="$1"
  printf '%s\n' "$txt" \
    | grep -Eai "$PAT_FREEZE|$PAT_CRIT|$PAT_BAD|$PAT_DISP" \
    | grep -Eai 'drm|nvidia|i915|amdgpu|radeon|nouveau|gnome-shell|mutter|wayland|xorg|display|monitor|kms|modeset|vsync|pageflip|crtc|edid|hpd|gpu' \
    | grep -Evai 'audit:|apparmor|systemd-detect-' \
    | awk '!seen[$0]++' \
    | sort \
    | tail -n 50
}

LATEST_EVENT="$(printf '%s\n' "$ALL_TXT" | grep -Eai "$PAT_FREEZE|$PAT_CRIT|$PAT_BAD|$PAT_DISP" | tail -n 1 || true)"
LATEST_EVENT_TS="$(extract_error_time "$LATEST_EVENT")"
TIMELINE_TXT="$(build_timeline "$ALL_TXT" || true)"

echo "${GRN}== Resumen claro ==${RST}"
echo "Monitores conectados detectados: $CONNECTED_COUNT"
if [[ "$CONNECTED_COUNT" -ge 2 ]]; then
  echo "Estado fisico basico: ambos monitores parecen conectados."
elif [[ "$CONNECTED_COUNT" -eq 1 ]]; then
  echo "Estado fisico basico: solo 1 monitor conectado en este momento."
else
  echo "Estado fisico basico: no se detectaron monitores conectados en DRM."
fi
echo

echo "${GRN}== Posibles soluciones (priorizadas) ==${RST}"
if [[ "$FINDINGS_COUNT" -eq 0 ]]; then
  echo "1) No hay errores fuertes en este rango."
  echo "2) Si el problema es intermitente, ejecuta: ./monitor_errores_ultima_hora.sh \"24 hours ago\""
  echo "3) Si vuelve a pasar, usa --detallado para ver los logs tecnicos."
else
  echo "Problema principal detectado: $FIRST_TITLE"
  echo "Hora exacta del error principal: $(extract_error_time "$FIRST_EVIDENCE")"
  if [[ -n "${LATEST_EVENT//[[:space:]]/}" ]]; then
    echo "Hora del evento relevante mas reciente: $LATEST_EVENT_TS"
    echo "Evento reciente: $LATEST_EVENT"
  fi
  echo "Que significa: $(meaning_from_title "$FIRST_TITLE")"
  echo "Evidencia: $FIRST_EVIDENCE"
  echo "Que hacer ahora: $FIRST_ACTIONS"
  if [[ "$FINDINGS_COUNT" -gt 1 ]]; then
    echo "Nota: tambien se detectaron $((FINDINGS_COUNT - 1)) hallazgo(s) adicional(es). Usa --detallado para verlos."
  fi
fi
echo

case "$FINAL_SEV" in
  CRIT) echo "${RED}Estado: CRIT${RST} (grave: requiere correccion prioritaria)";;
  WARN) echo "${YEL}Estado: WARN${RST} (hay errores, pero no se ve fallo critico de GPU)";;
  *)    echo "${GRN}Estado: OK${RST} (no hay senales fuertes en este rango)";;
esac

echo
echo "${YEL}Siguiente paso recomendado:${RST}"
if [[ "$FINAL_SEV" == "WARN" ]]; then
  echo "1) Prueba sesion Xorg (pantalla de login) y compara."
  echo "2) Desactiva extensiones GNOME y reinicia sesion."
  echo "3) Repite diagnostico: ./monitor_errores_ultima_hora.sh \"2 hours ago\""
elif [[ "$FINAL_SEV" == "CRIT" ]]; then
  echo "1) Cambia cable/puerto/dock primero."
  echo "2) Actualiza driver GPU + kernel."
  echo "3) Repite diagnostico con --detallado y comparte salida."
else
  echo "1) Mantener drivers/sistema actualizados."
  echo "2) Si el fallo reaparece, corre con --detallado."
fi

echo
echo "${GRN}== Linea de tiempo (ultimos 20 errores) ==${RST}"
if [[ -z "${TIMELINE_TXT//[[:space:]]/}" ]]; then
  echo "Sin errores relevantes en el rango analizado."
else
  printf '%s\n' "$TIMELINE_TXT"
fi

if [[ "$VERBOSE" -eq 1 ]]; then
  echo
  echo "${GRN}== Hallazgos tecnicos completos ==${RST}"
  if [[ -z "${FINDINGS//[[:space:]]/}" ]]; then
    echo "- Sin hallazgos tecnicos fuertes."
  else
    printf '%s' "$FINDINGS"
  fi
  echo
  echo "${GRN}== Soluciones tecnicas completas ==${RST}"
  if [[ -z "${SOLUTIONS//[[:space:]]/}" ]]; then
    echo "- Mantener drivers/kernel actualizados."
  else
    printf '%s' "$SOLUTIONS"
  fi
fi
