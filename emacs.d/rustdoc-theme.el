;;; rustdoc-theme.el --- A theme based on the Rustdoc Dark theme  -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 0.1.0
;; Keywords: faces theme
;; URL: https://github.com/rust-lang/rust
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; A color theme for Emacs based on the Rustdoc Dark theme (which is essentially
;; Base16 Tomorrow Night).
;;
;;; Code:

(deftheme rustdoc
  "A theme based on the Rustdoc Dark theme.")

(defconst rustdoc-colors-alist
  '(("class"   . '((class color) (min-colors 89)))
    ("bg"      . "#1d1f21")
    ("fg"      . "#c5c8c6")
    ("comment" . "#969896") ;; Standard Tomorrow Night comment, slightly lighter than #707880 for better readability
    ("red"     . "#cc6666")
    ("orange"  . "#de935f")
    ("yellow"  . "#f0c674")
    ("green"   . "#b5bd68")
    ("cyan"    . "#8abeb7")
    ("blue"    . "#81a2be")
    ("purple"  . "#b294bb")
    ("hl-line" . "#282a2e")
    ("selection" . "#373b41")
    ("dark"    . "#1d1f21")
    ("light"   . "#e0e0e0"))
  "Palette for the rustdoc theme.")

(defmacro rustdoc-with-colors (&rest body)
  "Execute BODY with colors from `rustdoc-colors-alist' bound."
  (declare (indent 0))
  (let ((colors (mapcar (lambda (x) (list (intern (car x)) (cdr x)))
                        rustdoc-colors-alist)))
    `(let ,colors
       ,@body)))

(rustdoc-with-colors
  (custom-theme-set-faces
   'rustdoc
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,fg))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:background ,selection))))
   `(hl-line ((,class (:background ,hl-line))))
   `(fringe ((,class (:background ,bg))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(vertical-border ((,class (:foreground ,selection))))

   ;; Font Lock
   `(font-lock-builtin-face ((,class (:foreground ,red))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,orange))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,purple))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,red))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))

   ;; Mode Line
   `(mode-line ((,class (:background ,selection :foreground ,fg :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,comment :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold))))

   ;; Search
   `(isearch ((,class (:background ,yellow :foreground ,bg))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg))))
   `(lazy-highlight ((,class (:background ,blue :foreground ,bg))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,comment :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,hl-line))))

   ;; Org Mode
   `(org-level-1 ((,class (:foreground ,blue :weight bold))))
   `(org-level-2 ((,class (:foreground ,yellow :weight bold))))
   `(org-level-3 ((,class (:foreground ,purple :weight bold))))
   `(org-level-4 ((,class (:foreground ,red :weight bold))))
   `(org-code ((,class (:foreground ,green))))
   `(org-hide ((,class (:foreground ,bg))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-date ((,class (:foreground ,cyan))))
   `(org-block ((,class (:background ,hl-line))))

   ;; Markdown
   `(markdown-header-face-1 ((,class (:foreground ,blue :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,yellow :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,purple :weight bold))))
   `(markdown-code-face ((,class (:foreground ,green))))

   ;; Rainbow Delimiters (optional support)
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,red))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rustdoc)

;;; rustdoc-theme.el ends here