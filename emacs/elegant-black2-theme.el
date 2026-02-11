;;; elegant-black2-theme.el --- Tema elegante y oscuro para Emacs -*- lexical-binding: t; -*-

;; Author: Tu Nombre
;; Description: Tema oscuro, minimalista y moderno.

(deftheme elegant-black2
  "Tema oscuro y elegante, amable con la vista y sobrio en los colores.")

(let* ((class '((class color) (min-colors 89)))
       (fg "#D7D7D9")
       (fg-alt "#B8B8BB")
       (bg "#0E0E10")
       (bg-alt "#151518")
       (bg-alt2 "#1C1C20")
       (cursor "#FFD479")
       (red "#E87A7A")
       (orange "#F0A96B")
       (yellow "#FFD479")
       (green "#9CD6A3")
       (blue "#82A7DD")
       (magenta "#C7A1E6")
       (cyan "#71D4C3")
       (grey "#6E6E75")
       (grey-alt "#555555")
       (border "#232327")
       (hl-line-bg "#151518"))

  (custom-theme-set-faces
   'elegant-black2

   ;; Básico
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(fringe ((,class (:background ,bg))))
   `(region ((,class (:background ,bg-alt2 :foreground ,fg))))
   `(highlight ((,class (:background ,bg-alt2 :foreground ,fg))))
   `(shadow ((,class (:foreground ,grey))))
   `(minibuffer-prompt ((,class (:foreground ,blue :weight semi-bold))))
   `(vertical-border ((,class (:foreground ,border))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:foreground ,border))))
   `(window-divider-last-pixel ((,class (:foreground ,border))))
   `(hl-line ((,class (:background ,hl-line-bg))))
   `(link ((,class (:foreground ,blue :underline t))))

   ;; Resaltado de sintaxis
   `(font-lock-builtin-face       ((,class (:foreground ,magenta))))
   `(font-lock-comment-face       ((,class (:foreground ,grey :slant italic))))
   `(font-lock-doc-face           ((,class (:foreground ,grey))))
   `(font-lock-constant-face      ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,blue :weight semi-bold))))
   `(font-lock-keyword-face       ((,class (:foreground ,red :weight semi-bold))))
   `(font-lock-string-face        ((,class (:foreground ,green))))
   `(font-lock-type-face          ((,class (:foreground ,yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange))))
   `(font-lock-warning-face       ((,class (:foreground ,red :weight bold))))

   ;; Estados genéricos
   `(success ((,class (:foreground ,green :weight bold))))
   `(warning ((,class (:foreground ,yellow :weight bold))))
   `(error   ((,class (:foreground ,red :weight bold))))

   ;; Mode-line (doom-modeline se apoya en esto también)
   `(mode-line
     ((,class (:foreground ,fg
               :background ,bg-alt2
               :box nil
               :overline ,border
               :underline ,border))))
   `(mode-line-inactive
     ((,class (:foreground ,grey
               :background ,bg-alt
               :box nil
               :overline ,border))))
   `(header-line
     ((,class (:foreground ,fg-alt
               :background ,bg-alt
               :box nil
               :overline ,border))))

   ;; Números de línea
   `(line-number
     ((,class (:foreground ,grey :background ,bg))))
   `(line-number-current-line
     ((,class (:foreground ,yellow :weight bold :background ,bg))))

   ;; Parent match
   `(show-paren-match
     ((,class (:background ,blue :foreground ,bg :weight bold))))
   `(show-paren-mismatch
     ((,class (:background ,red :foreground ,bg :weight bold))))

   ;; Búsqueda
   `(isearch
     ((,class (:background ,yellow :foreground ,bg :weight bold))))
   `(lazy-highlight
     ((,class (:background ,bg-alt2 :foreground ,yellow :weight semi-bold))))

   ;; Tooltips
   `(tooltip
     ((,class (:background ,bg-alt2 :foreground ,fg :inherit variable-pitch))))

   ;; Org-mode básico (para que no se vea “crudo”)
   `(org-level-1 ((,class (:foreground ,blue   :weight semi-bold :height 1.1))))
   `(org-level-2 ((,class (:foreground ,yellow :weight semi-bold))))
   `(org-level-3 ((,class (:foreground ,green))))
   `(org-code    ((,class (:foreground ,cyan))))
   `(org-block   ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line ((,class (:foreground ,grey :background ,bg-alt :extend t))))
   `(org-block-end-line   ((,class (:foreground ,grey :background ,bg-alt :extend t))))

   ;; Company (autocompletado), por si lo usas
   `(company-tooltip            ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-selection  ((,class (:background ,bg-alt2 :foreground ,fg))))
   `(company-scrollbar-bg       ((,class (:background ,bg-alt))))
   `(company-scrollbar-fg       ((,class (:background ,grey))))
   `(company-tooltip-annotation ((,class (:foreground ,cyan))))

   ;; doom-modeline bar (para integrarlo con tu color de acento)
   `(doom-modeline-bar ((,class (:background ,yellow))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'elegant-black2)
;;; elegant-black2-theme.el ends here
