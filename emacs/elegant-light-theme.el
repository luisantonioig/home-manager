;; ;;; elegant-light-theme.el --- Tema elegante y claro para Emacs -*- lexical-binding: t; -*-

;; ;; Autor: Tu Nombre
;; ;; Descripción: Un tema claro y elegante, perfecto para complementar el borde de tu laptop y brindar una experiencia visual amigable.

;; (deftheme elegant-light "Tema claro y elegante, diseñado para ser amable con la vista y avanzar con paso firme.")

;; (let ((class '((class color) (min-colors 89)))
;;       ;; Definición de la paleta de colores:
;;       (fg "#34302D")        ;; Color principal del texto (marrón oscuro)
;;       (bg "#F5F2E0")        ;; Fondo principal (papel amarillento)
;;       (bg-alt "#EAE6D1")    ;; Fondo alternativo para regiones y barras
;;       (cursor "#D0803A")    ;; Color del cursor para resaltarlo (naranja elegante)
;;       (red "#D34F4D")       ;; Versión más suave del rojo
;;       (orange "#E67C45")    ;; Naranja más elegante
;;       (yellow "#D19A45")    ;; Amarillo más elegante
;;       (green "#689A74")     ;; Verde más suave
;;       (blue "#4F89CC")      ;; Azul sofisticado
;;       (magenta "#AA77AA")   ;; Magenta más suave
;;       (cyan "#4C9999")      ;; Cyan elegante
;;       (grey "#A0A0A0"))     ;; Gris medio para comentarios
;;   (custom-theme-set-faces
;;    'elegant-light

;;    ;; Configuración básica de colores:
;;    `(default ((,class (:background ,bg :foreground ,fg))))
;;    `(cursor ((,class (:background ,cursor))))
;;    `(region ((,class (:background "#D9D9D9" :foreground ,fg))))
;;    `(fringe ((,class (:background ,bg))))
;;    `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))

;;    ;; Resaltado de sintaxis:
;;    `(font-lock-builtin-face ((,class (:foreground "#8C5383"))))
;;    `(font-lock-comment-face ((,class (:foreground "#7D7165" :slant italic))))
;;    `(font-lock-constant-face ((,class (:foreground "#3D7B7B"))))
;;    `(font-lock-function-name-face ((,class (:foreground "#3B649A" :weight bold))))
;;    `(font-lock-keyword-face ((,class (:foreground "#A44035" :weight bold))))
;;    `(font-lock-string-face ((,class (:foreground "#4E6F49"))))
;;    `(font-lock-type-face ((,class (:foreground "#8D6C28"))))
;;    `(font-lock-variable-name-face ((,class (:foreground "#A9622A"))))
;;    `(font-lock-warning-face ((,class (:foreground ,red :background ,bg-alt :weight bold))))

;;    ;; Configuración de la barra de modo (mode-line):
;;    `(mode-line ((,class (:foreground ,fg :background ,bg-alt
;;                           :box (:line-width -1 :style released-button)))))
;;    `(mode-line-inactive ((,class (:foreground "#9A9283" :background ,bg
;;                                    :box (:line-width -1 :style released-button)))))
;;    `(header-line ((,class (:foreground ,fg :background ,bg-alt))))

;;    ;; Otros elementos de la interfaz:
;;    `(link ((,class (:foreground ,blue :underline t))))
;;    `(show-paren-match ((,class (:background "#BBDFFF" :foreground ,fg :weight bold))))
;;    `(show-paren-mismatch ((,class (:background ,red :foreground "#FFFFFF" :weight bold))))
;;    ))

;; ;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; ;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))
;; (set-frame-font "IBM Plex Mono" nil t)

;; ;; Cursor configuration
;; (blink-cursor-mode 1)
;; (setq-default cursor-type '(bar . 2))
;; (setq blink-cursor-blinks 0)
;; (setq idle-update-delay 1.0)

;; (defun my-cursor-update ()
;;   "Cambia el color del cursor según el estado del buffer."
;;   (if (buffer-modified-p)
;;       (set-cursor-color "#A44035")  ;; Rojo antiguo si hay cambios sin guardar
;;     (set-cursor-color "#A9622A"))) ;; Naranja antiguo si todo está guardado

;; (add-hook 'post-command-hook #'my-cursor-update)

;; (use-package beacon
;;   :ensure t
;;   :config
;;   (setq beacon-color "#3B649A")
;;   (setq beacon-size 40)
;;   (setq beacon-blink-duration 0.2)
;;   (beacon-mode 1))

;; (defvar my-cursor-timer nil)
;; (defvar my-cursor-speed-threshold 0.1) ;; Tiempo en segundos para detectar rapidez

;; (defun my-adjust-cursor-size ()
;;   "Ajusta el tamaño del cursor según la velocidad de escritura."
;;   (when my-cursor-timer
;;     (cancel-timer my-cursor-timer))
;;   (setq my-cursor-timer
;;         (run-with-timer my-cursor-speed-threshold nil
;;                         (lambda ()
;;                           (if (input-pending-p)
;;                               (setq cursor-type '(bar . 5)) ;; Cursor grueso si escribes rápido
;;                             (setq cursor-type '(bar . 2))))))) ;; Vuelve a delgado

;; (add-hook 'post-command-hook #'my-adjust-cursor-size)

;; ;; Configuration for modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 25)                 ;; Altura de la barra
;;   (setq doom-modeline-bar-width 3)               ;; Grosor de la barra de estado
;;   (setq doom-modeline-hud t)                     ;; Activar efecto de carga visual

;;   ;; Mostrar información útil
;;   (setq doom-modeline-buffer-encoding nil)       ;; Oculta la codificación del buffer (menos ruido)
;;   (setq doom-modeline-time t)                    ;; Mostrar la hora actual
;;   (setq doom-modeline-vcs t)                     ;; Mostrar rama de Git
;;   (setq doom-modeline-lsp t)                     ;; Mostrar estado de LSP (para Haskell, TypeScript, etc.)
;;   (setq doom-modeline-env-version t)             ;; Mostrar versión de entorno (ej: Node.js, Haskell)
;;   (setq doom-modeline-buffer-state-icon t)       ;; Mostrar iconos de estado del buffer
;;   (setq doom-modeline-modal-icon t))             ;; Mostrar el icono del modo actual (Evil-mode))

;; (custom-set-faces
;;  '(mode-line ((t (:background "#EAE6D1" :foreground "#34302D" :box nil))))  ;; Fondo papel, texto oscuro
;;  '(mode-line-inactive ((t (:background "#F5F2E0" :foreground "#9A9283" :box nil))))  ;; Versión atenuada
;;  '(doom-modeline-bar ((t (:background "#A9622A")))))  ;; Naranja antiguo para el indicador de estado

;; ;; Configuration for the line numbers
;; (setq display-line-numbers-type 'visual)
;; (custom-set-faces
;;  '(line-number ((t (:foreground "#9A9283" :background "#F5F2E0"))))  ;; Números en marrón claro
;;  '(line-number-current-line ((t (:foreground "#A9622A" :weight bold))))) ;; Línea actual en naranja antiguo

;; (set-face-attribute 'line-number nil :height 100) ;; Ajusta el tamaño
;; (set-face-attribute 'line-number-current-line nil :height 110)

;; (dolist (mode '(eshell-mode-hook
;;                 shell-mode-hook
;;                 term-mode-hook
;;                 dashboard-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (when (> (buffer-size) 500000)
;;               (display-line-numbers-mode -1))))


(deftheme elegant-light
  "Tema claro, elegante y sobrio, con fondo blanco y resaltado sutil sin subrayado.")

(let ((class '((class color) (min-colors 89)))
      (fg "#1C1C1C")
      (bg "#FFFFFF")
      (keyword "#2C5F8A")
      (string "#4E8A59")
      (comment "#8FA98F")
      (constant "#B27D12")
      (type "#8E6FBD")
      (builtin "#3C5E77")
      (variable "#5A5A5A")
      (warning "#A23C3C")
      (highlight "#F2F2F2")
      (mode-line-bg "#E6E6E6")
      (mode-line-inactive-bg "#F5F5F5")
      (mode-line-fg "#1C1C1C")
      (mode-line-inactive-fg "#888888"))

  (custom-theme-set-faces
   'elegant-light

   ;; Básicos
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,keyword))))
   `(region ((,class (:background ,highlight))))
   `(fringe ((,class (:background ,bg))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :weight bold))))

   ;; Sintaxis
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-function-name-face ((,class (:foreground ,fg :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-warning-face ((,class (:foreground ,warning :weight bold))))

   ;; Otros elementos
   `(link ((,class (:foreground ,keyword :underline nil))))
   `(highlight ((,class (:background ,highlight))))
   `(mode-line ((,class (:background ,mode-line-bg :foreground ,mode-line-fg))))
   `(mode-line-inactive ((,class (:background ,mode-line-inactive-bg :foreground ,mode-line-inactive-fg))))
   ))

(provide-theme 'elegant-light)
