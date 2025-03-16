#!/bin/bash

# Script para configurar reglas udev para dispositivos Ledger
# Este script debe ejecutarse con privilegios de superusuario (sudo)

# Colores para mensajes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # Sin color

# Verificar si se está ejecutando como root
if [ "$EUID" -ne 0 ]; then
  echo -e "${RED}Este script debe ejecutarse como root (usa sudo).${NC}"
  echo "Intenta: sudo $0"
  exit 1
fi

echo -e "${YELLOW}Configurando reglas udev para dispositivos Ledger...${NC}"

# Crear el archivo de reglas udev
RULES_FILE="/etc/udev/rules.d/20-ledger.rules"

# Hacer una copia de seguridad si el archivo ya existe
if [ -f "$RULES_FILE" ]; then
  BACKUP_FILE="${RULES_FILE}.backup.$(date +%Y%m%d%H%M%S)"
  echo -e "${YELLOW}Se encontró un archivo de reglas existente. Haciendo copia de seguridad en ${BACKUP_FILE}${NC}"
  cp "$RULES_FILE" "$BACKUP_FILE"
fi

# Crear el archivo de reglas con los permisos adecuados
cat > "$RULES_FILE" << 'EOL'
# Reglas para dispositivos Ledger Nano S, Nano X, Nano S Plus
# https://github.com/LedgerHQ/udev-rules/blob/master/add_udev_rules.sh

# Ledger Nano S
SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001|1000|1001|1002|1003|1004|1005|1006|1007|1008|1009|100a|100b|100c|100d|100e|100f|1010|1011|1012|1013|1014|1015|1016|1017|1018|1019|101a|101b|101c|101d|101e|101f", TAG+="uaccess", TAG+="udev-acl"
# Ledger Nano X
SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0004|4000|4001|4002|4003|4004|4005|4006|4007|4008|4009|400a|400b|400c|400d|400e|400f|4010|4011|4012|4013|4014|4015|4016|4017|4018|4019|401a|401b|401c|401d|401e|401f", TAG+="uaccess", TAG+="udev-acl"
# Ledger Nano S Plus
SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0005|5000|5001|5002|5003|5004|5005|5006|5007|5008|5009|500a|500b|500c|500d|500e|500f|5010|5011|5012|5013|5014|5015|5016|5017|5018|5019|501a|501b|501c|501d|501e|501f", TAG+="uaccess", TAG+="udev-acl"
# Ledger Blue
SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0000|0002|0003|0015|1015", TAG+="uaccess", TAG+="udev-acl"
EOL

# Verificar que el archivo se creó correctamente
if [ -f "$RULES_FILE" ]; then
  echo -e "${GREEN}Archivo de reglas creado correctamente en ${RULES_FILE}${NC}"
else
  echo -e "${RED}Error al crear el archivo de reglas.${NC}"
  exit 1
fi

# Establecer los permisos adecuados
chmod 644 "$RULES_FILE"

# Recargar las reglas udev
echo -e "${YELLOW}Recargando reglas udev...${NC}"
udevadm control --reload-rules
udevadm trigger

echo -e "${GREEN}¡Configuración completada con éxito!${NC}"
echo -e "${YELLOW}Si tu dispositivo Ledger está conectado, desconéctalo y vuelve a conectarlo.${NC}"
echo -e "${GREEN}Ahora deberías poder utilizar Ledger Live con tu dispositivo.${NC}"

# Información adicional
echo -e "\n${YELLOW}Información adicional:${NC}"
echo -e "- Si experimentas problemas, asegúrate de que tu usuario esté en el grupo 'plugdev':"
echo -e "  ${GREEN}sudo usermod -aG plugdev \$USER${NC}"
echo -e "- Puede ser necesario reiniciar la sesión para que los cambios surtan efecto."
echo -e "- Para probar si el dispositivo es reconocido correctamente, puedes usar:"
echo -e "  ${GREEN}lsusb | grep -i ledger${NC}"
