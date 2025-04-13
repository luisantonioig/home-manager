;;; compact-mode.el --- Major mode for editing Compact contract code

;; Copyright (C) 2025 Your Name Here
;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Este modo proporciona resaltado de sintaxis para código de contratos de Compact.

;;; Code:

(defvar compact-mode-hook nil)

(defvar compact-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Compact mode.")

(defconst compact-font-lock-keywords
  (list
   ;; Palabras clave de control
   '("\\<\\(pragma\\|import\\|export\\|constructor\\|circuit\\|pure\\|witness\\|ledger\\)\\>" . font-lock-keyword-face)
   
   ;; Tipos de control de flujo
   '("\\<\\(if\\|else\\|for\\|of\\|return\\|while\\|assert\\)\\>" . font-lock-control-face)
   
   ;; Tipos de datos
   '("\\<\\(const\\|let\\|var\\|function\\|struct\\|enum\\)\\>" . font-lock-type-face)
   
   ;; Tipos primitivos
   '("\\<\\(Boolean\\|Bytes\\|Uint\\|Vector\\|Counter\\|Maybe\\|GAME_STATE\\|SHOT_RESULT\\|SHIP\\)\\>" . font-lock-type-face)
   
   ;; Funciones y métodos
   '("\\<\\(calculate_shot_result\\|check_winner\\|create_ship_state\\|get_shot_result\\|update_ship_state\\|get_ships\\|get_ship_state\\|update_hit_cell\\|update_hit_cell_state\\|contains\\|occupied_cells\\|unique_vector\\|ship2_cells\\|ship3_cells\\|ship4_cells\\|ship5_cells\\)\\>" . font-lock-function-name-face)
   
   ;; Constantes y valores
   '("\\<\\(true\\|false\\|null\\|undefined\\)\\>" . font-lock-constant-face)
   
   ;; Nombres de variables y funciones que comienzan con p1, p2, game, shot, etc.
   '("\\<\\(p1\\|p2\\|game_state\\|shot_attempt\\|last_shot_result\\)\\>" . font-lock-variable-name-face)
   
   ;; Comentarios
   '("//.*" . font-lock-comment-face)
   '("/\\*\\(.\\|\n\\)*?\\*/" . font-lock-comment-face)
   
   ;; Strings
   '("\".*?\"" . font-lock-string-face)
   
   ;; Símbolos especiales
   '("\\(=>\\|==\\|!=\\|<=\\|>=\\|&&\\|||\\)" . font-lock-builtin-face))
  "Resaltado para modo Compact.")

(defvar compact-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Tabla de sintaxis para modo Compact.")

(defun compact-mode ()
  "Major mode para editar archivos de Compact contract."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table compact-mode-syntax-table)
  (use-local-map compact-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(compact-font-lock-keywords nil nil nil nil))
  (setq major-mode 'compact-mode)
  (setq mode-name "Compact")
  (run-hooks 'compact-mode-hook))

(add-to-list 'auto-mode-alist '("\\.compact\\'" . compact-mode))

;; Añadir soporte para indentación
(defun compact-indent-line ()
  "Indentar línea actual según el código Compact."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*}") ; Si la línea contiene un } cerrado
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) 2)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}") ; Si la línea anterior tiene un } cerrado
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^.*{") ; Si la línea anterior tiene un { abierto
                  (progn
                    (setq cur-indent (+ (current-indentation) 2))
                    (setq not-indented nil))
                (if (bobp) ; Si estamos al principio del buffer
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(provide 'compact-mode)

;;; compact-mode.el ends here
