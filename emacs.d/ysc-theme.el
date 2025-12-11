;;; Adapated from Tomorrow Theme  -*- lexical-binding: t; -*-
;;
;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles
;; Rewritten by Steve Purcell <steve@sanityinc.com> for compatibility
;; Update to match master by Donald Curtis <dcurtis@milkbox.net>

(deftheme ysc
  "A theme based on the Tomorrow Night theme.")

(defconst ysc-colors-alist
  '(("class"   . '((class color) (min-colors 89)))
    ("bg"      . "#1d1f21")
    ("fg"      . "#c5c8c6")
    ("comment" . "#969896")
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
  "Palette for the ysc theme.")

(defmacro ysc-theme-with-colors (&rest body)
  "Execute BODY with colors from `ysc-colors-alist' bound."
  (declare (indent 0))
  (let ((colors (mapcar (lambda (x) (list (intern (car x)) (cdr x)))
                        ysc-colors-alist)))
    `(let ,colors
       ,@body)))

(ysc-theme-with-colors
  (custom-theme-set-faces
   'ysc
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
   `(mode-line ((,class (:background ,selection :foreground ,fg :box (:style released-button)))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,comment :box (:style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold))))

   ;; Search
   `(isearch ((,class (:background ,yellow :foreground ,bg))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg))))
   `(lazy-highlight ((,class (:background ,blue :foreground ,bg))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,comment :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,hl-line))))

   ;; Markdown
   `(markdown-header-face-1 ((,class (:foreground ,blue :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,yellow :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,purple :weight bold))))
   `(markdown-code-face ((,class (:foreground ,green))))

   ;; Flymake
   `(flymake-error ((,class (:underline nil))))
   `(flymake-warning ((,class (:underline nil))))
   `(flymake-note ((,class (:underline nil))))
   `(flymake-error-echo-at-eol ((,class (:foreground ,red :slant italic :height 130))))
   `(flymake-warning-echo-at-eol ((,class (:foreground ,orange :slant italic :height 130))))
   `(flymake-note-echo-at-eol ((,class (:foreground ,blue :slant italic :height 130))))

   ;; LaTeX
   `(font-latex-sectioning-1-face ((,class (:foreground ,fg :height 150 :weight bold))))
   `(font-latex-sectioning-2-face ((,class (:foreground ,fg :height 150 :weight bold))))
   `(font-latex-sectioning-3-face ((,class (:foreground ,fg :height 150 :weight bold))))
   `(font-latex-sectioning-4-face ((,class (:foreground ,fg :height 150 :weight bold))))
   `(font-latex-sectioning-5-face ((,class (:foreground ,fg :height 150 :weight bold))))
   `(font-latex-italic-face ((,class (:foreground ,fg :height 150 :slant italic))))
   `(font-latex-bold-face ((,class (:foreground ,fg :height 150 :weight bold))))

   ;; General
   `(success ((,class (:foreground ,blue :weight bold))))

   ;; Tab bars
   `(tab-bar ((,class (:background ,bg
					   :box (:line-width (2 . 8) :color ,bg)))))
   `(tab-bar-tab ((,class (:background ,bg
						   :box nil
						   :underline (:color ,fg :style double-line :position 5)))))
   `(tab-bar-tab-inactive ((,class (:box nil :background ,bg))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ysc)

;;; ysc-theme.el ends here
