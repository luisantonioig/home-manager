(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'elegant-black t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; TODO @luisantonioig: Quitar esto despues de provar las capacidades de aiken lsp
(setq lsp-log-io t)

;; Graphical user interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable intant-tabs-mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; typescript-mode and tsx-mode configuration
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :custom
  (typescript-indent-level 2))

;; TODO @luisantonioig: What can I do with tree-sitter??
;; tree-sitter-mode configuration
;; (add-hook 'typescript-mode-hook #'tree-sitter-mode)
;; (add-hook 'tsx-mode-hook #'tree-sitter-mode)
;; (setq major-mode-remap-alist
;;       '((typescript-mode . typescript-ts-mode)
;;         (tsx-mode . tsx-ts-mode)))

(use-package web-mode
  :mode ("\\.js\\'" "\\.jsx\\'")
  :hook ((web-mode . (lambda ()
                       (setq indent-tabs-mode nil)
                       (setq web-mode-markup-indent-offset 2)
                       (setq web-mode-css-indent-offset 2)
                       (setq web-mode-code-indent-offset 2))))
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)))

(use-package lsp-mode
  :hook ((typescript-mode . lsp))
  :commands lsp
  :config
  (setq lsp-clients-typescript-init-options '(hostInfo "Emacs"))
  (setq lsp-eslint-auto-fix-on-save t)
  (setq lsp-disabled-clients '()))

;; lsp-mode configuration
(require 'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'tsx-mode-hook #'lsp)

;; lsp-ui-mode configuration
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t))
;; (require 'lsp-ui)
;; (add-hook 'lsp-mode-hook #'lsp-ui-mode)

(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(setq js-indent-level 2)


;; TODO @luisantonioig: make aiken-mode to work
;; (use-package aiken-mode
;;   :load-path "~/personal/aiken-mode/")

(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq inhibit-startup-echo-area-message t)

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

;; Función para obtener TODOs usando ripgrep
(defun dashboard-get-todos ()
  "Obtener lista de TODOs usando ripgrep."
  (let* ((default-directory (or default-directory "~/"))
         (cmd (format "rg --line-number \"TODO\" %s" default-directory))
         (output (shell-command-to-string cmd))
         (todos '()))
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\(.*\\):\\([0-9]+\\):.*TODO.*" line)
        (let ((file (match-string 1 line))
              (line-num (string-to-number (match-string 2 line))))
          (push (cons file line-num) todos))))
    (nreverse todos)))

;; Función para insertar TODOs en el dashboard
(defun dashboard-insert-todos (list-size)
  (dashboard-insert-heading "TODOs")
  (let ((todos (dashboard-get-todos)))
    (if (not todos)
        (insert "\n    No se encontraron TODOs")
      (dolist (todo (seq-take todos list-size))
        (let ((file (car todo))
              (line (cdr todo)))
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
                        (format "%s:%d" (file-name-nondirectory file) line)))))))

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



;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-items '((recents   . 10)  ;; Mostrar 10 archivos recientes
;;                           (bookmarks . 5)   ;; Mostrar 5 marcadores
;;                           (projects  . 5)   ;; Mostrar 5 proyectos recientes
;;                           (agenda    . 5))) ;; Mostrar 5 eventos del calendario
;;   (setq dashboard-projects-backend 'projectile)
;;   (setq dashboard-banner-logo-title "Bienvenido a Emacs 🚀")
;;   (setq dashboard-startup-banner 'official)  ;; Usa el logo oficial de Emacs
;;   (setq dashboard-center-content t)          ;; Centrar contenido
;;   (setq dashboard-set-heading-icons t)       ;; Agrega iconos a las secciones
;;   (setq dashboard-set-file-icons t))         ;; Usa iconos en la lista de archivos recientes

;; (defun dashboard-insert-todo-files (list-size)
;;   "Insertar una lista de archivos con 'TODO' en el Dashboard usando ripgrep (rg)."
;;   (let* ((default-directory (or (ignore-errors (projectile-project-root)) default-directory))
;;          (command "rg --files-with-matches --no-messages TODO . -g '!.git' -g '!node_modules' -g '!.cache'")
;;          (output (shell-command-to-string command))
;;          (todo-files (split-string output "\n" t))) ;; Dividir la salida en líneas

;;     ;; 🔹 Mensajes de depuración
;;     (message "=== DEBUG: Output de rg ===\n%s" output)
;;     (message "=== DEBUG: Archivos encontrados ===\n%S" todo-files)

;;     (when todo-files
;;       (dashboard-insert-section
;;        "Archivos con TODOs:"
;;        (cl-subseq todo-files 0 (min (length todo-files) list-size))
;;        list-size
;;        'todos
;;        "t"
;;        'identity)))) ;; 🔹 Cambiado de 'dashboard-insert-file' a 'identity'





;; ;; Agregar la nueva sección después de que `dashboard` se haya cargado
;; (with-eval-after-load 'dashboard
;;   (add-to-list 'dashboard-item-generators '(todos . dashboard-insert-todo-files))
;;   (add-to-list 'dashboard-items '(todos . 5))) ;; Muestra hasta 5 archivos con TODOs


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
  ; TODO @luisantonioig: This takes so much, maybe look for a better solution
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
