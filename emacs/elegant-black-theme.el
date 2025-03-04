;;; elegant-black-theme.el --- Tema elegante y oscuro para Emacs -*- lexical-binding: t; -*-

;; Autor: Tu Nombre
;; Descripción: Un tema oscuro y elegante, perfecto para complementar el borde negro de tu laptop y brindar una experiencia visual amigable.

(deftheme elegant-black "Tema oscuro y elegante, diseñado para ser amable con la vista y avanzar con paso firme.")

(let ((class '((class color) (min-colors 89)))
      ;; Definición de la paleta de colores:
      (fg "#DCDCCC")        ;; Color principal del texto.
      (bg "#1E1E1E")        ;; Fondo principal (oscuro y elegante).
      (bg-alt "#262626")    ;; Fondo alternativo para regiones y barras.
      (cursor "#FFCC66")    ;; Color del cursor para resaltarlo.
      (red "#F2777A")
      (orange "#F99157")
      (yellow "#FFCC66")
      (green "#99CC99")
      (blue "#6699CC")
      (magenta "#CC99CC")
      (cyan "#66CCCC")
      (grey "#7F7F7F"))
  (custom-theme-set-faces
   'elegant-black

   ;; Configuración básica de colores:
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background "#3A3A3A" :foreground ,fg))))
   `(fringe ((,class (:background ,bg))))
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))

   ;; Resaltado de sintaxis:
   `(font-lock-builtin-face ((,class (:foreground ,magenta))))
   `(font-lock-comment-face ((,class (:foreground ,grey :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,red :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-warning-face ((,class (:foreground ,red :background ,bg-alt :weight bold))))

   ;; Configuración de la barra de modo (mode-line):
   `(mode-line ((,class (:foreground ,fg :background ,bg-alt
                          :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((,class (:foreground ,grey :background ,bg
                                   :box (:line-width -1 :style released-button)))))
   `(header-line ((,class (:foreground ,fg :background ,bg-alt))))

   ;; Otros elementos de la interfaz:
   `(link ((,class (:foreground ,blue :underline t))))
   `(show-paren-match ((,class (:background ,blue :foreground ,bg :weight bold))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,bg :weight bold))))
   ))
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alfpha . (90 . 90)))
(set-frame-font "IBM Plex Mono" nil t)

;; Cursor configuration
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 2))
(setq blink-cursor-blinks 0)
(setq idle-update-delay 1.0)

(defun my-cursor-update ()
  "Cambia el color del cursor según el estado del buffer."
  (if (buffer-modified-p)
      (set-cursor-color "#F2777A")  ;; Rojo si hay cambios sin guardar
    (set-cursor-color "#FFCC66"))) ;; Blanco si todo está guardado

(add-hook 'post-command-hook #'my-cursor-update)

(use-package beacon
  :ensure t
  :config
  (setq beacon-color "#6699CC")
  (setq beacon-size 40)
  (setq beacon-blink-duration 0.2)
  (beacon-mode 1))

;; TODO: this does not work (yet)
;; (use-package smooth-cursor
;;   :ensure t
;;   :config
;;   (setq smooth-cursor-type 'bar)
;;   (setq smooth-cursor-speed 0.88)
;;   (setq smooth-cursor-duration 0.3)
;;   (smooth-cursor-mode 1))

;; NOTE: this does not work because cursor is white if the file is saved and red if it is not
;; (defun my-highlight-empty-line ()
;;   "Cambia el color del cursor si está en una línea vacía o al final del archivo."
;;   (if (or (looking-at-p "^[ \t]*$")  ;; Línea vacía
;;           (eobp))                    ;; Fin del archivo
;;       (set-cursor-color "magenta")    ;; Magenta si está en línea vacía o EOF
;;     (set-cursor-color "white")))      ;; Blanco en otras líneas

;; (add-hook 'post-command-hook #'my-highlight-empty-line)

(defvar my-cursor-timer nil)
(defvar my-cursor-speed-threshold 0.1) ;; Tiempo en segundos para detectar rapidez

(defun my-adjust-cursor-size ()
  "Ajusta el tamaño del cursor según la velocidad de escritura."
  (when my-cursor-timer
    (cancel-timer my-cursor-timer))
  (setq my-cursor-timer
        (run-with-timer my-cursor-speed-threshold nil
                        (lambda ()
                          (if (input-pending-p)
                              (setq cursor-type '(bar . 5)) ;; Cursor grueso si escribes rápido
                            (setq cursor-type '(bar . 2))))))) ;; Vuelve a delgado

(add-hook 'post-command-hook #'my-adjust-cursor-size)

;; Configuration for modeline

;; NOTE: I have to run (nerd-icons-install-fonts) to install all the icons needed
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)                 ;; Altura de la barra
  (setq doom-modeline-bar-width 3)               ;; Grosor de la barra de estado
  (setq doom-modeline-hud t)                     ;; Activar efecto de carga visual

  ;; Mostrar información útil
  (setq doom-modeline-buffer-encoding nil)       ;; Oculta la codificación del buffer (menos ruido)
  (setq doom-modeline-time t)                    ;; Mostrar la hora actual
  (setq doom-modeline-vcs t)                     ;; Mostrar rama de Git
  (setq doom-modeline-lsp t)                     ;; Mostrar estado de LSP (para Haskell, TypeScript, etc.)
  (setq doom-modeline-env-version t)             ;; Mostrar versión de entorno (ej: Node.js, Haskell)
  (setq doom-modeline-buffer-state-icon t)       ;; Mostrar iconos de estado del buffer
  (setq doom-modeline-modal-icon t))             ;; Mostrar el icono del modo actual (Evil-mode))

(custom-set-faces
 '(mode-line ((t (:background "#262626" :foreground "#DCDCCC" :box nil))))  ;; Fondo oscuro, texto claro
 '(mode-line-inactive ((t (:background "#1E1E1E" :foreground "#7F7F7F" :box nil))))  ;; Versión atenuada
 '(doom-modeline-bar ((t (:background "#FFCC66")))))  ;; Amarillo elegante para el indicador de estado

;; Configuration for the line numbers

(setq display-line-numbers-type 'visual)
(custom-set-faces
 '(line-number ((t (:foreground "#7F7F7F" :background "#1E1E1E"))))  ;; Números en gris
 '(line-number-current-line ((t (:foreground "#FFCC66" :weight bold))))) ;; Línea actual en amarillo

(set-face-attribute 'line-number nil :height 140) ;; Ajusta el tamaño
(set-face-attribute 'line-number-current-line nil :height 110)

(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'find-file-hook
          (lambda ()
            (when (> (buffer-size) 500000)
              (display-line-numbers-mode -1))))



(provide-theme 'elegant-black)
;;; elegant-black-theme.el ends here
