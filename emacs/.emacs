(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; ;; NOTE: change theme using load-theme
(load-theme 'elegant-black t)
; TODO @luisantonioig: Make a cheat sheet of any available command in my configuration
(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(setq-default header-line-format nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; NOTE @luisantonioig: La siguiente línea muestra la comunicación entre el servidor lsp y emacs, descomentar para debuguear ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; (setq lsp-log-io t)

;; ;; Graphical user interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Disable intant-tabs-mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


(global-display-line-numbers-mode 1)
(global-visual-line-mode t)


(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq inhibit-startup-echo-area-message t)


(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(setq js-indent-level 2)


;; ;; typescript-mode and tsx-mode configuration
;; (use-package typescript-mode
;;   :ensure t
;;   :mode ("\\.ts\\'" "\\.tsx\\'")
;;   :custom
;;   (typescript-indent-level 2))

;; (use-package web-mode
;;   :mode ("\\.js\\'" "\\.jsx\\'")
;;   :hook ((web-mode . (lambda ()
;;                        (setq indent-tabs-mode nil)
;;                        (setq web-mode-markup-indent-offset 2)
;;                        (setq web-mode-css-indent-offset 2)
;;                        (setq web-mode-code-indent-offset 2))))
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)))

;; (use-package lsp-mode
;;   :hook ((typescript-mode . lsp))
;;   :commands lsp
;;   :config
;;   (setq lsp-clients-typescript-init-options '(hostInfo "Emacs"))
;;   (setq lsp-eslint-auto-fix-on-save t)
;;   (setq lsp-disabled-clients '()))

;; ;; lsp-mode configuration
;; (require 'lsp-mode)
;; (add-hook 'typescript-mode-hook #'lsp)
;; (add-hook 'tsx-mode-hook #'lsp)

;; ;; lsp-ui-mode configuration
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-position 'at-point
;;         lsp-ui-doc-show-with-cursor t
;;         lsp-ui-doc-show-with-mouse t
;;         lsp-ui-peek-enable t
;;         lsp-ui-sideline-enable t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-code-actions t))
;; ;; (require 'lsp-ui)
;; ;; (add-hook 'lsp-mode-hook #'lsp-ui-mode)


;; Configuration for haskell-mode
;; (use-package haskell-mode
;;   :ensure t
;;   ;; :hook (haskell-mode . interactive-haskell-mode)
;;   :config
;;   (setq haskell-process-type 'ghci)
;;   (setq haskell-process-log t))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-server-path "haskell-language-server")
  (setq lsp-haskell-server-args '("-d")))

(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Configuration for projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/iog" "~/personal")) ;; Ajusta a tus rutas
  (setq projectule-auto-discover t)
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)
  (setq projectile-sort-order 'recentf)
  (setq projectile-generic-command "rg --files --hidden --glob '!**node_modules**'")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; Configuración básica del dashboard
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo
      dashboard-center-content t
      dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (todos . 10)))  ;; Sección personalizada para TODOs

(defun is-weekday-p ()
  "Devuelve t si hoy es un día entre semana (lunes a viernes), nil en caso contrario."
  (let* ((dow-num (string-to-number (format-time-string "%w"))))
    ;; %w devuelve 0 para domingo, 1-5 para lunes-viernes, 6 para sábado
    (and (> dow-num 0) (< dow-num 6))))

;; Función para obtener TODOs usando ripgrep
(defun dashboard-get-todos ()
  "Obtener lista de TODOs usando ripgrep."
  (let* ((default-directory (if (is-weekday-p)
               "~/iog"
               "~/personal"))
         ;; (default-directory (or default-directory "~/"))
         ;; TODO @luisantonioig: Maybe change my github handle for a environment variable so that any persona can use it
         (cmd (format "rg --line-number \"TODO @luisantonioig: \" %s" default-directory))
         (output (shell-command-to-string cmd))
         (todos '()))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\(.*\\):\\([0-9]+\\):.*TODO @luisantonioig: \\(.*\\)" line)
        (let ((file (match-string 1 line))
              (line-num (string-to-number (match-string 2 line)))
              (todo-text (match-string 3 line)))
          (push (list file line-num todo-text) todos))))
    (nreverse todos)))

;; Función para insertar TODOs en el dashboard
(defun dashboard-insert-todos (list-size)
  (dashboard-insert-heading "TODOs")
  (let ((todos (dashboard-get-todos)))
    (if (not todos)
        (insert "\n 🎉 Congratulations! You've successfully completed all your tasks! ✅")
      (dolist (todo (seq-take todos list-size))
        (let ((file (nth 0 todo))
              (line (nth 1 todo))
              (text (nth 2 todo))
              )
          (insert "\n    ")
          (widget-create 'push-button
                        :action `(lambda (&rest _)
                                  (find-file ,file)
                                  (goto-char (point-min))
                                  (forward-line (1- ,line)))
                        :mouse-face 'highlight
                        :follow-link "\C-m"
                        :button-prefix ""
                        :button-suffix ""
                        :format "%[%t%]"
                        (format "%s:%d - \t%s" (file-name-nondirectory file) line text)))))))

;; Registrar la función para la sección personalizada
(add-to-list 'dashboard-item-generators '(todos . dashboard-insert-todos))

;; Función para refrescar el dashboard
(defun dashboard-refresh ()
  "Refrescar el buffer del dashboard."
  (interactive)
  (when (get-buffer dashboard-buffer-name)
    (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name))

;; Atajo de teclado para refrescar
(global-set-key (kbd "C-c d") 'dashboard-refresh)

;; Función para saltar a la sección de TODOs en el dashboard
(defun dashboard-jump-to-todos ()
  "Saltar a la sección de TODOs en el dashboard."
  (interactive)
  (let ((buffer (get-buffer dashboard-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "TODOs" nil t)
          (beginning-of-line)
          (forward-line 1))))))

;; Agregar la tecla "t" al mapa de teclas del dashboard
(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "t") 'dashboard-jump-to-todos))

;; Agregar la tecla a la lista de atajos mostrados en el dashboard
(add-to-list 'dashboard-item-shortcuts '(todos . "t"))

;; ;; (use-package dashboard
;; ;;   :ensure t
;; ;;   :config
;; ;;   (dashboard-setup-startup-hook)
;; ;;   (setq dashboard-items '((recents   . 10)  ;; Mostrar 10 archivos recientes
;; ;;                           (bookmarks . 5)   ;; Mostrar 5 marcadores
;; ;;                           (projects  . 5)   ;; Mostrar 5 proyectos recientes
;; ;;                           (agenda    . 5))) ;; Mostrar 5 eventos del calendario
;; ;;   (setq dashboard-projects-backend 'projectile)
;; ;;   (setq dashboard-banner-logo-title "Bienvenido a Emacs 🚀")
;; ;;   (setq dashboard-startup-banner 'official)  ;; Usa el logo oficial de Emacs
;; ;;   (setq dashboard-center-content t)          ;; Centrar contenido
;; ;;   (setq dashboard-set-heading-icons t)       ;; Agrega iconos a las secciones
;; ;;   (setq dashboard-set-file-icons t))         ;; Usa iconos en la lista de archivos recientes

;; ;; (defun dashboard-insert-todo-files (list-size)
;; ;;   "Insertar una lista de archivos con 'TODO' en el Dashboard usando ripgrep (rg)."
;; ;;   (let* ((default-directory (or (ignore-errors (projectile-project-root)) default-directory))
;; ;;          (command "rg --files-with-matches --no-messages TODO . -g '!.git' -g '!node_modules' -g '!.cache'")
;; ;;          (output (shell-command-to-string command))
;; ;;          (todo-files (split-string output "\n" t))) ;; Dividir la salida en líneas

;; ;;     ;; 🔹 Mensajes de depuración
;; ;;     (message "=== DEBUG: Output de rg ===\n%s" output)
;; ;;     (message "=== DEBUG: Archivos encontrados ===\n%S" todo-files)

;; ;;     (when todo-files
;; ;;       (dashboard-insert-section
;; ;;        "Archivos con TODOs:"
;; ;;        (cl-subseq todo-files 0 (min (length todo-files) list-size))
;; ;;        list-size
;; ;;        'todos
;; ;;        "t"
;; ;;        'identity)))) ;; 🔹 Cambiado de 'dashboard-insert-file' a 'identity'





;; ;; ;; Agregar la nueva sección después de que `dashboard` se haya cargado
;; ;; (with-eval-after-load 'dashboard
;; ;;   (add-to-list 'dashboard-item-generators '(todos . dashboard-insert-todo-files))
;; ;;   (add-to-list 'dashboard-items '(todos . 5))) ;; Muestra hasta 5 archivos con TODOs


;; Search stack
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
	("C-x b" . consult-buffer)
	("M-y" . consult-yank-pop))
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-preview)
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Command to assign tasks to myself
(defun insert-todo-luisantonioig ()
  "Inserts a TODO comment using the format of the current mode"
  (interactive)
  (let ((todo-text (concat (or comment-start "//") " TODO @luisantonioig:")))
    (insert todo-text)))

(global-set-key (kbd "C-c t") 'insert-todo-luisantonioig)

;; magit projects same as projectile projects,
(setq magit-repository-directories
      (mapcar (lambda (dir) (cons dir 0)) projectile-known-projects))

;; doom-modeline configuration
(defun my/nerd-icons-fonts-installed-p ()
  "This function checks if the icons are already installed."
  (let ((font-dir (expand-file-name "fonts" (expand-file-name "nerd-icons" user-emacs-directory))))
    (and (file-directory-p font-dir)
         (> (length (directory-files font-dir nil "\\.ttf$")) 0))))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))
  ;; Install only if they aren't installed yet
  ; TODO @luisantonioig: This takes so much, maybe look for a better solution to run nerd-icons-install-fonts
  ;; (unless (my/nerd-icons-fonts-installed-p)
  ;;   (nerd-icons-install-fonts t))) ;; `t` para instalar sin preguntar

(setq doom-modeline-height 25)  ;; Ajustar altura
(setq doom-modeline-bar-width 4) ;; Ajustar ancho de la barra de estado
(setq doom-modeline-icon t)      ;; Habilitar iconos si están disponibles
(setq doom-modeline-major-mode-icon t) ;; Mostrar icono del modo actual
(setq doom-modeline-buffer-file-name-style 'relative-to-project) ;; Nombre del archivo relativo al proyecto
(setq doom-modeline-enable-word-count t) ;; Mostrar conteo de palabras en modos como org-mode
(setq doom-modeline-buffer-encoding nil) ;; Ocultar codificación del buffer
(setq doom-modeline-env-version nil) ;; Ocultar versiones de entorno como Python o Node

;; Aiken support
(require 'aiken-mode)

;; lsp-mode for aiken
(require 'lsp-mode)

;; Definición simplificada del cliente
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ();
                     (list "aiken" "lsp" "--stdio")))
  :major-modes '(aiken-mode)
  :server-id 'aiken-ls))

;; Creación del modo si no existe
(unless (fboundp 'aiken-mode)
  (define-derived-mode aiken-mode prog-mode "Aiken"
    "Major mode for editing Aiken files."
    (setq-local comment-start "--")
    (setq-local comment-end "")))

;; Asociar extensiones de archivo
(add-to-list 'auto-mode-alist '("\\.ak\\'" . aiken-mode))

;; Activar LSP al abrir archivos Aiken
; TODO @luisantonioig: Make tree-sitter to work (again)
;; (add-hook 'aiken-mode-hook #'lsp)

;; (add-to-list 'lsp-language-id-configuration '(aiken-mode . "aiken"))


;; ;; (require 'tree-sitter)
;; ;; (require 'tree-sitter-langs)

;; ;; (global-tree-sitter-mode)

;; ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; ;; (add-hook 'typescript-mode-hook #'tree-sitter-mode)


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; (require 'compact-mode)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)
;; ;; Configuración de tree-sitter
(setq tree-sitter-load-path '("~/.emacs.d/tree-sitter"))
(setq tree-sitter-langs-grammar-dir "~/.emacs.d/tree-sitter")

(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))



;;;;;;;;;;;;;;;;;
;; Experimento ;;
;;;;;;;;;;;;;;;;;

(use-package treesit
  :ensure nil  ;; Integrado en Emacs 29+
  :config
  ;; Instalar gramáticas para TypeScript
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src")))
  
  ;; Remapeo de modos
  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (tsx-mode . tsx-ts-mode)))

  ;; Configurar extensiones de archivo
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))


(defun my/typescript-treesit-fontlock ()
  "Configurar resaltado sintáctico para TypeScript con tree-sitter."
  ;; Nivel máximo de resaltado
  (setq-local treesit-font-lock-level 4)
  
  ;; Personalizar caras de resaltado
  (face-remap-add-relative 'treesit-font-lock-type-face '(:foreground "sea green" :weight bold))
  (face-remap-add-relative 'treesit-font-lock-function-name-face '(:foreground "dodger blue"))
  (face-remap-add-relative 'treesit-font-lock-variable-name-face '(:foreground "sandy brown"))
  (face-remap-add-relative 'treesit-font-lock-property-face '(:foreground "light sea green"))
  (face-remap-add-relative 'treesit-font-lock-constructor-face '(:foreground "magenta" :weight bold)))

;; 3. Configuración para indentación basada en tree-sitter
(defun my/typescript-treesit-indent ()
  "Configurar indentación para TypeScript con tree-sitter."
  (setq-local typescript-indent-level 2)  ;; Ajustar según preferencia
  
  ;; Reglas de indentación personalizadas
  (setq-local treesit-simple-indent-rules
              `((typescript
                 ;; Nivel principal
                 ((node-is "program") parent-bol 0)
                 ((node-is "export_statement") parent-bol 0)
                 ((node-is "import_statement") parent-bol 0)
                 
                 ;; Bloques
                 ((parent-is "statement_block") parent-bol typescript-indent-level)
                 ((parent-is "class_body") parent-bol typescript-indent-level)
                 ((parent-is "object") parent-bol typescript-indent-level)
                 ((parent-is "array") parent-bol typescript-indent-level)
                 
                 ;; Expresiones
                 ((parent-is "arguments") parent-bol typescript-indent-level)
                 ((parent-is "formal_parameters") parent-bol typescript-indent-level)
                 ((parent-is "binary_expression") parent-bol typescript-indent-level)
                 
                 ;; Cerrar bloques
                 ((or (node-is "}") (node-is ")") (node-is "]")) parent-bol 0)))))

;; 4. Configuración de lsp-mode para TypeScript
(use-package lsp-mode
  :ensure t
  :hook ((typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp))
  :commands lsp
  :config
  ;; Configuración básica
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-typescript-format-enable t)
  
  ;; Configuración de performance
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-keep-workspace-alive nil)
  
  ;; Configuración específica para TypeScript
  (setq lsp-typescript-preferences-import-module-specifier 'relative)
  (setq lsp-typescript-surveys-enabled nil))

(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1))

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (my/typescript-treesit-fontlock)
            (my/typescript-treesit-indent)))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (my/typescript-treesit-fontlock)
            (my/typescript-treesit-indent)))


(defun my/ts-next-definition ()
  (interactive)
  (let* ((query-text "(function_declaration) @func (class_declaration) @class")
         (query (treesit-query-compile 'typescript query-text))
         (root-node (treesit-buffer-root-node))
         (pos (point))
         (matches (treesit-query-capture root-node query))
         (next-pos nil))
    
    (dolist (match matches)
      (let ((start (treesit-node-start (cdr match))))
        (when (and (> start pos)
                  (or (not next-pos) (< start next-pos)))
          (setq next-pos start))))
    
    (if next-pos
        (goto-char next-pos)
      (message "No hay más definiciones"))))

(defvar my-treesit-map (make-sparse-keymap)
  "Mapa de teclado para comandos de tree-sitter.")

;;;;;;;;;;;;;;;;;;;
;; From my theme ;;
;;;;;;;;;;;;;;;;;;;

;; ;; ;; Estas configuraciones deberían estar en init.el, no en el tema

;; ;; Configuración de transparencia
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; ;; Configuración de fuente
;; ;; (set-frame-font "IBM Plex Mono" nil t)

;; ;; Configuración del cursor
;; (blink-cursor-mode 1)
;; (setq-default cursor-type '(bar . 2))
;; (setq blink-cursor-blinks 0)
;; (setq idle-update-delay 1.0)

;; (defun my-cursor-update ()
;;   "Cambia el color del cursor según el estado del buffer."
;;   (if (buffer-modified-p)
;;       (set-cursor-color "#F2777A") ;; Rojo si hay cambios sin guardar
;;     (set-cursor-color "#FFCC66"))) ;; Amarillo si todo está guardado

;; (add-hook 'post-command-hook #'my-cursor-update)

;; ;; Paquete beacon
;; (use-package beacon
;;   :ensure t
;;   :config
;;   (setq beacon-color "#6699CC")
;;   (setq beacon-size 40)
;;   (setq beacon-blink-duration 0.2)
;;   (beacon-mode 1))

;; ;; Ajuste de cursor según velocidad de escritura
;; (defvar my-cursor-timer nil)
;; (defvar my-cursor-speed-threshold 0.1)

;; (defun my-adjust-cursor-size ()
;;   "Ajusta el tamaño del cursor según la velocidad de escritura."
;;   (when my-cursor-timer
;;     (cancel-timer my-cursor-timer))
;;   (setq my-cursor-timer
;;         (run-with-timer my-cursor-speed-threshold nil
;;                         (lambda ()
;;                           (if (input-pending-p)
;;                               (setq cursor-type '(bar . 5))
;;                             (setq cursor-type '(bar . 2)))))))

;; (add-hook 'post-command-hook #'my-adjust-cursor-size)

;; ;; Configuración de doom-modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 35)
;;   (setq doom-modeline-bar-width 3)
;;   (setq doom-modeline-hud t)
;;   (setq doom-modeline-buffer-encoding nil)
;;   (setq doom-modeline-time t)
;;   (setq doom-modeline-vcs t)
;;   (setq doom-modeline-lsp t)
;;   (setq doom-modeline-env-version t)
;;   (setq doom-modeline-buffer-state-icon t)
;;   (setq doom-modeline-modal-icon t))

;; ;; Configuración de números de línea
;; (setq display-line-numbers-type 'visual)
;; (custom-set-faces
;;  '(line-number ((t (:foreground "#7F7F7F" :background "#1E1E1E"))))
;;  '(line-number-current-line ((t (:foreground "#FFCC66" :weight bold)))))

;; (set-face-attribute 'line-number nil :height 200)
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

;; (set-frame-font "IBM Plex Mono-12" nil t)

;; ;; Solución específica para IBM Plex Mono
;; (defun fix-ibm-plex-mono (&optional frame)
;;   "Configura IBM Plex Mono correctamente."
;;   (interactive)
;;   (with-selected-frame (or frame (selected-frame))
;;     (when (display-graphic-p)
;;       ;; Usar nombres de fuente específicos con diagnóstico
;;       (let ((method-used "ninguno"))
;;         ;; Primer intento: sintaxis normal
;;         (condition-case nil
;;             (progn
;;               (set-frame-font "IBM Plex Mono-12" nil t)
;;               (setq method-used "sintaxis normal"))
;;           (error
;;            ;; Segundo intento: sintaxis XLFD
;;            (condition-case nil
;;                (progn
;;                  (set-frame-font "-*-IBM Plex Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1" nil t)
;;                  (setq method-used "sintaxis XLFD"))
;;              (error
;;               ;; Último recurso: font-spec
;;               (let ((fs (font-spec :family "IBM Plex Mono" :size 12)))
;;                 (set-frame-parameter nil 'font fs)
;;                 (set-face-attribute 'default nil :font fs)
;;                 (setq method-used "font-spec"))))))
        
;;         ;; Mostrar qué método funcionó
;;         (message "Fuente configurada usando método: %s" method-used)
;;         (with-current-buffer (get-buffer-create "*Font Method*")
;;           (erase-buffer)
;;           (insert (format "Fuente configurada usando método: %s\n" method-used))
;;           (insert (format "Hora: %s\n" (current-time-string)))
;;           (insert (format "Frame: %s\n" (frame-parameter nil 'name)))
;;           (insert (format "Daemon: %s\n" (if (daemonp) "sí" "no")))))
      
;;       ;; También configurar la fuente para la modeline
;;       (set-face-attribute 'mode-line nil :height 200)
;;       (set-face-attribute 'mode-line-inactive nil :height 200))))

;; ;; Ejecutar al inicio y para cada nuevo frame
;; (add-hook 'after-init-hook 'fix-ibm-plex-mono)
;; (add-hook 'server-after-make-frame-hook 'fix-ibm-plex-mono)
;; (add-hook 'after-make-frame-functions 'fix-ibm-plex-mono)

;; ;; Para el daemon, también usar un timer
;; (when (daemonp)
;;   (run-with-timer 1 nil 'fix-ibm-plex-mono))

;; Añade esto a tu init.el
(defun my-setup-fonts (&optional frame)
  "Configurar fuente para el frame actual o especificado."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      ;; Configurar fuente
      (set-frame-font "IBM Plex Mono-12" nil t)
      ;; Actualizar también default-frame-alist para futuros frames
      (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-12")))))

;; Aplicar a todos los escenarios
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my-setup-fonts)
  (my-setup-fonts))


;; Configuración para dashboard en daemon/client
(defun setup-dashboard-for-frame (&optional frame)
  "Configurar dashboard para el frame actual o especificado."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      ;; Reiniciar y configurar dashboard para este frame
      (setq dashboard-startup-banner 'logo
            dashboard-center-content t
            dashboard-items '((recents  . 5)
                              (bookmarks . 5)
                              (todos . 10)))
      
      ;; Regenerar dashboard si ya existe un buffer
      (when (get-buffer dashboard-buffer-name)
        (with-current-buffer dashboard-buffer-name
          (dashboard-refresh-buffer))))))

;; Aplicar al iniciar y cuando se crean nuevos frames cliente
(if (daemonp)
    (add-hook 'after-make-frame-functions #'setup-dashboard-for-frame)
  (add-hook 'after-init-hook 'dashboard-setup-startup-hook))

;; Asegurar que el dashboard se regenere al abrir un cliente
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (daemonp) (get-buffer "*dashboard*"))
              (dashboard-refresh-buffer))))
