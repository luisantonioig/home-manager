;;; .emacs --- Configuracion personal de Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Archivo organizado por secciones para facilitar lectura y mantenimiento.
;; Objetivo: mantener el comportamiento actual con mejor estructura y comentarios.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0) Rendimiento de arranque ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Acelera startup reduciendo GC y desactivando handlers costosos temporalmente.
(defvar my/default-file-name-handler-alist file-name-handler-alist
  "Valor original de `file-name-handler-alist' para restaurar tras startup.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t
      package-quickstart t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/default-file-name-handler-alist)
            (garbage-collect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) Rutas y carga base    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) Apariencia global     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ventana inicial y tema.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(load-theme 'elegant-black2 t)

;; Transparencia ligera (efecto sobrio tipo glass).
(set-frame-parameter (selected-frame) 'alpha-background 95)
(add-to-list 'default-frame-alist '(alpha-background . 95))

;; Fuente para GUI (tambien en nuevos frames cuando corres Emacs como daemon).
(defun my/setup-fonts (&optional frame)
  "Configurar fuente para el FRAME actual o especificado."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-frame-font "IBM Plex Mono-12" nil t)
      (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-12")))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/setup-fonts)
  (my/setup-fonts))

;; UI minima: sin menu/tool/scroll bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Inicio limpio.
(setq inhibit-startup-screen t
      initial-buffer-choice nil
      inhibit-startup-echo-area-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3) Edicion y comportamiento
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Espacios en lugar de tabs y ancho base de 2.
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Ajuste de lineas visual.
(global-visual-line-mode t)

;; Numeros de linea en general (modo visual para archivos con wrap).
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode 1)

;; Desactivar numeros donde molestan o no aportan.
(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; En archivos muy grandes, quitar numeros para mejorar rendimiento.
(add-hook 'find-file-hook
          (lambda ()
            (when (> (buffer-size) 500000)
              (display-line-numbers-mode -1))))

;; Directorios de backup/autosave para no ensuciar proyectos.
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; Evitar header-line por defecto.
(setq-default header-line-format nil)

;; Integrar PATH del shell (util con Nix/entornos no interactivos).
(use-package exec-path-from-shell
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . exec-path-from-shell-initialize))

;; Integracion con direnv/nix-direnv: entorno por proyecto (clave para Haskell multi-version).
(use-package envrc
  :ensure t
  :defer t
  :hook (after-init . envrc-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4) Cursor y feedback visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cursor base.
(setq indicate-buffer-boundaries 'left
      blink-cursor-blinks 0
      which-func-update-delay 1.0)
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 2))

(defvar my/cursor-timer nil)
(defvar my/cursor-speed-threshold 0.1
  "Segundos para considerar escritura rapida.")
(defvar my/cursor-last-modified-state nil
  "Estado previo de `buffer-modified-p` para evitar repintado innecesario.")

(defun my/adjust-cursor-size ()
  "Ajustar grosor del cursor temporalmente al escribir."
  (setq cursor-type '(bar . 5))
  (when my/cursor-timer
    (cancel-timer my/cursor-timer))
  (setq my/cursor-timer
        (run-with-idle-timer
         my/cursor-speed-threshold nil
         (lambda ()
           (setq cursor-type '(bar . 2))))))

(defun my/cursor-update-color ()
  "Cambiar color del cursor solo cuando cambia el estado del buffer."
  (let ((modified (buffer-modified-p)))
    (unless (eq modified my/cursor-last-modified-state)
      (setq my/cursor-last-modified-state modified)
      (set-cursor-color (if modified "#E87A7A" "#FFD479")))))

(add-hook 'post-command-hook #'my/cursor-update-color)
(add-hook 'post-self-insert-hook #'my/adjust-cursor-size)

;; Flash suave para ubicar cursor tras saltos grandes.
(use-package beacon
  :ensure t
  :defer t
  :hook (after-init . beacon-mode)
  :config
  (setq beacon-color "#82A7DD"
        beacon-size 40
        beacon-blink-duration 0.2))

;; Barra visual de scroll en fringe derecho.
(use-package yascroll
  :ensure t
  :defer t
  :hook (after-init . global-yascroll-bar-mode)
  :config
  (setq yascroll:scroll-bar 'right-fringe
        yascroll:delay-to-hide 0.5)
  (set-face-attribute 'yascroll:thumb-fringe nil
                      :background "#FFD479"
                      :foreground "#FFD479"))

;; Modeline.
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-buffer-encoding nil
        doom-modeline-time t
        doom-modeline-vcs t
        doom-modeline-lsp t
        doom-modeline-env-version t
        doom-modeline-buffer-state-icon t
        doom-modeline-modal-icon t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5) Busqueda y completado  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6) Proyectos y Git        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :defer t
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-project-search-path '("~/iog" "~/personal")
        projectile-auto-discover t
        projectile-enable-caching t)
  :config
  (setq projectile-switch-project-action #'projectile-dired
        projectile-completion-system 'default
        projectile-sort-order 'recentf
        projectile-generic-command "rg --files --hidden --glob '!**node_modules**'")
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

;; Intentar mantener directorios de magit alineados con proyectos conocidos.
(with-eval-after-load 'projectile
  (setq magit-repository-directories
        (mapcar (lambda (dir) (cons dir 0)) projectile-known-projects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7) Dashboard + TODOs      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dashboard-setup-startup-hook)
(defvar my/dashboard-show-todos-on-startup nil
  "Si es nil, difiere TODOs para que Emacs abra mas rapido.")

(setq dashboard-startup-banner 'logo
      dashboard-center-content t
      dashboard-items (if my/dashboard-show-todos-on-startup
                          '((recents . 5)
                            (bookmarks . 5)
                            (todos . 10))
                        '((recents . 5)
                          (bookmarks . 5))))

(defun my/weekday-p ()
  "Devolver t si hoy es lunes-viernes."
  (let ((dow-num (string-to-number (format-time-string "%w"))))
    (and (> dow-num 0) (< dow-num 6))))

(defun dashboard-get-todos ()
  "Obtener TODOs con formato TODO @luisantonioig usando ripgrep."
  (let* ((default-directory (if (my/weekday-p) "~/iog" "~/personal"))
         (cmd (format "rg --line-number --hidden --no-messages --glob '!.git' --glob '!node_modules' \"TODO @luisantonioig: \" %s"
                      default-directory))
         (output (shell-command-to-string cmd))
         (todos '()))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\(.*\\):\\([0-9]+\\):.*TODO @luisantonioig: \\(.*\\)" line)
        (let ((file (match-string 1 line))
              (line-num (string-to-number (match-string 2 line)))
              (todo-text (match-string 3 line)))
          (push (list file line-num todo-text) todos))))
    (nreverse todos)))

(defun dashboard-insert-todos (list-size)
  "Insertar seccion de TODOs en dashboard con LIST-SIZE elementos."
  (dashboard-insert-heading "TODOs")
  (let ((todos (dashboard-get-todos)))
    (if (not todos)
        (insert "\n Congratulations! Has completado todas tus tareas.\n")
      (dolist (todo (seq-take todos list-size))
        (let ((file (nth 0 todo))
              (line (nth 1 todo))
              (text (nth 2 todo)))
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
                         (format "%s:%d - %s"
                                 (file-name-nondirectory file)
                                 line
                                 text)))))))

(add-to-list 'dashboard-item-generators '(todos . dashboard-insert-todos))
(add-to-list 'dashboard-item-shortcuts '(todos . "t"))

(defun dashboard-refresh ()
  "Refrescar el buffer del dashboard."
  (interactive)
  (when (get-buffer dashboard-buffer-name)
    (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name))

(defun dashboard-jump-to-todos ()
  "Saltar a la seccion de TODOs en dashboard."
  (interactive)
  (let ((buffer (get-buffer dashboard-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "TODOs" nil t)
          (beginning-of-line)
          (forward-line 1))))))

(global-set-key (kbd "C-c d") #'dashboard-refresh)
(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "t") #'dashboard-jump-to-todos))

;; Ajustes de dashboard para daemon/client.
(defun my/setup-dashboard-for-frame (&optional frame)
  "Ajustar dashboard para FRAME actual o especificado."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (setq dashboard-startup-banner 'logo
            dashboard-center-content t
            dashboard-items '((recents . 5)
                              (bookmarks . 5)
                              (todos . 10)))
      (when (get-buffer dashboard-buffer-name)
        (with-current-buffer dashboard-buffer-name
          (dashboard-refresh-buffer))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/setup-dashboard-for-frame))

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (daemonp) (get-buffer "*dashboard*"))
              (dashboard-refresh-buffer))))

;; Habilitar TODOs despues del arranque para no bloquear la apertura inicial.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             1 nil
             (lambda ()
               (setq dashboard-items '((recents . 5)
                                       (bookmarks . 5)
                                       (todos . 10)))
               (when (get-buffer dashboard-buffer-name)
                 (dashboard-refresh-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8) Utilidades personales  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-todo-luisantonioig ()
  "Insertar TODO con formato TODO @luisantonioig del lenguaje actual."
  (interactive)
  (let ((todo-text (concat (or comment-start "//") " TODO @luisantonioig:")))
    (insert todo-text)))

(global-set-key (kbd "C-c t") #'insert-todo-luisantonioig)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9) LSP base              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nota de debug opcional:
;; (setq lsp-log-io t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred lsp-describe-session)
  :init
  (setq lsp-keymap-prefix "C-c l"
        ;; Mejora throughput en servidores LSP (TS/Nix/Haskell).
        read-process-output-max (* 1024 1024)
        process-adaptive-read-buffering nil)
  :config
  ;; Mantener snippets desactivados por defecto.
  (setq lsp-enable-snippet nil

        ;; Preferir imports no-relativos (evita rutas largas en proyectos con symlinks/Nix).
        lsp-typescript-preferences-import-module-specifier 'non-relative
        lsp-javascript-preferences-import-module-specifier 'non-relative

        ;; Balance de UX/rendimiento.
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.5
        lsp-log-io nil

        ;; Mantener organize imports manual.
        lsp-disabled-code-actions '("source.organizeImports")))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10) Lenguajes            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; JSON/JS.
(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(setq js-indent-level 2)

;; Haskell.
(defun my/haskell-format-provider ()
  "Elegir formatter Haskell disponible en PATH."
  (cond
   ((executable-find "fourmolu") "fourmolu")
   ((executable-find "ormolu") "ormolu")
   (t "none")))

(defun my/haskell-lsp-server-path ()
  "Elegir el ejecutable de HLS disponible en PATH."
  (or (executable-find "haskell-language-server-wrapper")
      (executable-find "haskell-language-server")
      "haskell-language-server-wrapper"))

(defun my/haskell-mode-setup ()
  "Ajustes de productividad para Haskell."
  ;; Comando de build por defecto para `M-x compile`.
  (setq-local compile-command "cabal build")
  ;; Si hay formatter disponible via HLS, formatear al guardar.
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (bound-and-true-p lsp-mode)
                         (boundp 'lsp-haskell-formatting-provider)
                         (not (string= lsp-haskell-formatting-provider "none")))
                (lsp-format-buffer)))
            nil t))

(use-package haskell-mode
  :if (locate-library "haskell-mode")
  :ensure nil
  :hook (haskell-mode . my/haskell-mode-setup)
  :config
  (setq haskell-stylish-on-save nil))

(use-package lsp-haskell
  :if (locate-library "lsp-haskell")
  :ensure nil
  :after lsp-mode
  :hook (haskell-mode . lsp-deferred)
  :config
  (setq lsp-haskell-server-path (my/haskell-lsp-server-path)
        lsp-haskell-server-args '("-d")
        lsp-haskell-formatting-provider (my/haskell-format-provider)))

;; Fallback robusto: asegurar autoinicio de LSP en buffers Haskell.
(with-eval-after-load 'lsp-mode
  (require 'lsp-haskell nil t))

(dolist (hook '(haskell-mode-hook haskell-literate-mode-hook))
  (add-hook hook #'my/haskell-mode-setup)
  (add-hook hook #'lsp-deferred))

;; Aiken.
(add-to-list 'auto-mode-alist '("\\.ak\\'" . aiken-mode))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (lambda () (list "aiken" "lsp" "--stdio")))
    :major-modes '(aiken-mode)
    :server-id 'aiken-ls))
  (add-to-list 'lsp-language-id-configuration '(aiken-mode . "aiken")))

(add-hook 'aiken-mode-hook #'lsp)

(use-package reformatter
  :ensure t)

(reformatter-define aiken-format
  :program "aiken"
  :args '("fmt" "--stdin")
  :stdin t)

(add-hook 'aiken-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'aiken-format-buffer nil t)))

;; Markdown.
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Shell scripts.
(use-package sh-script
  :ensure nil
  :mode (("\\.\\(sh\\|bash\\|zsh\\)$" . sh-mode))
  :hook ((sh-mode . lsp)
         (sh-mode . flycheck-mode)
         (sh-mode . shfmt-on-save-mode)
         (sh-mode . (lambda ()
                      (setq indent-tabs-mode nil
                            sh-basic-offset 2
                            sh-indentation 2)))))

(use-package shfmt
  :ensure t
  :commands (shfmt-on-save-mode))

(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; Nix.
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp))

;; TypeScript/TSX + Tree-sitter nativo (Emacs 29+).
(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src")))

  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (tsx-mode . tsx-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(use-package tree-sitter
  :ensure t
  :defer t)

(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after tree-sitter)

(setq tree-sitter-load-path '("~/.emacs.d/tree-sitter")
      tree-sitter-langs-grammar-dir "~/.emacs.d/tree-sitter"
      treesit-extra-load-path '("~/.emacs.d/tree-sitter"))

(use-package prettier-js
  :ensure nil
  :hook ((typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)))

(defun my/typescript-treesit-fontlock ()
  "Personalizar resaltado para TypeScript con tree-sitter."
  (setq-local treesit-font-lock-level 4)
  (face-remap-add-relative 'treesit-font-lock-type-face '(:foreground "sea green" :weight bold))
  (face-remap-add-relative 'treesit-font-lock-function-name-face '(:foreground "dodger blue"))
  (face-remap-add-relative 'treesit-font-lock-variable-name-face '(:foreground "sandy brown"))
  (face-remap-add-relative 'treesit-font-lock-property-face '(:foreground "light sea green"))
  (face-remap-add-relative 'treesit-font-lock-constructor-face '(:foreground "magenta" :weight bold)))

(defun my/typescript-treesit-indent ()
  "Configurar indentacion para TypeScript con reglas tree-sitter."
  (setq-local typescript-indent-level 2)
  (setq-local treesit-simple-indent-rules
              `((typescript
                 ((node-is "program") parent-bol 0)
                 ((node-is "export_statement") parent-bol 0)
                 ((node-is "import_statement") parent-bol 0)
                 ((parent-is "statement_block") parent-bol typescript-indent-level)
                 ((parent-is "class_body") parent-bol typescript-indent-level)
                 ((parent-is "object") parent-bol typescript-indent-level)
                 ((parent-is "array") parent-bol typescript-indent-level)
                 ((parent-is "arguments") parent-bol typescript-indent-level)
                 ((parent-is "formal_parameters") parent-bol typescript-indent-level)
                 ((parent-is "binary_expression") parent-bol typescript-indent-level)
                 ((or (node-is "}") (node-is ")") (node-is "]")) parent-bol 0)))))

(defun my/ts-next-definition ()
  "Ir a la siguiente definicion de funcion/clase en el buffer TypeScript."
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
      (message "No hay mas definiciones"))))

(defvar my-treesit-map (make-sparse-keymap)
  "Mapa de teclado para comandos personalizados de tree-sitter.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11) Fin del archivo       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '.emacs)
;;; .emacs ends here
