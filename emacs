(require 'package)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; see remark below
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-coq-initial-fold-state 'bullets)
 '(coq-double-hit-enable t)
 '(default-frame-alist '((fullscreen . maximized)))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(exec-path-from-shell proof-general color-theme-modern company-coq dracula-theme evil))
 '(proof-electric-terminator-enable nil)
 '(proof-three-window-mode-policy 'hybrid)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fira Mono")))))

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

; Evil initial setup (before loading Evil)
(setq evil-shift-width 2)

;; Enable Evil
(require 'evil)
(evil-mode 1)

(evil-select-search-module 'evil-search-module 'evil-search)

;(load-theme 'dracula t)
(add-hook 'coq-mode-hook #'company-coq-mode)

(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Emacs
; Maximized screen


;; Proof general
; dont show the arrow
(setq overlay-arrow-string "")
(setq mac-command-key-is-meta t)
(setq proof-next-command-insert-space nil)
(setq coq-one-command-per-line nil)
(setq scroll-step 1)
(setq scroll-conservatively  10000)

;; Input of unicode symbols
(require 'math-symbol-lists)
; Automatically use math input method for Coq files
(add-hook 'coq-mode-hook (lambda () (set-input-method "math")))
; Input method for the minibuffer
(defun my-inherit-input-method ()
  "Inherit input method from `minibuffer-selected-window'."
  (let* ((win (minibuffer-selected-window))
         (buf (and win (window-buffer win))))
    (when buf
      (activate-input-method (buffer-local-value 'current-input-method buf)))))
(add-hook 'minibuffer-setup-hook #'my-inherit-input-method)
; Define the actual input method
(quail-define-package "math" "UTF-8" "Ω" t)
(quail-define-rules ; add whatever extra rules you want to define here...
 ("\\fun"    ?λ)
 ("\\mult"   ?⋅)
 ("\\ent"    ?⊢)
 ("\\valid"  ?✓)
 ("\\diamond" ?◇)
 ("\\box"    ?□)
 ("\\bbox"   ?■)
 ("\\later"  ?▷)
 ("\\pred"   ?φ)
 ("\\and"    ?∧)
 ("\\or"     ?∨)
 ("\\comp"   ?∘)
 ("\\ccomp"  ?◎)
 ("\\all"    ?∀)
 ("\\ex"     ?∃)
 ("\\to"     ?→)
 ("\\sep"    ?∗)
 ("\\lc"     ?⌜)
 ("\\rc"     ?⌝)
 ("\\Lc"     ?⎡)
 ("\\Rc"     ?⎤)
 ("\\lam"    ?λ)
 ("\\empty"  ?∅)
 ("\\Lam"    ?Λ)
 ("\\Sig"    ?Σ)
 ("\\-"      ?∖)
 ("\\aa"     ?●)
 ("\\af"     ?◯)
 ("\\auth"   ?●)
 ("\\frag"   ?◯)
 ("\\iff"    ?↔)
 ("\\gname"  ?γ)
 ("\\incl"   ?≼)
 ("\\latert" ?▶)
 ("\\update" ?⇝)
 ("\\Ph"     ?Φ)
 ("\\gam"    ?γ)
 ("\\named"  ?∷)
 ("\\hto"    ?↪)
 ("\\tname"  ?τ)
 ("\\aname"  ?α)

 ;; accents (for iLöb)
 ("\\\"o" ?ö)

 ;; subscripts and superscripts
 ("^^+" ?⁺) ("__+" ?₊) ("^^-" ?⁻)
 ("__0" ?₀) ("__1" ?₁) ("__2" ?₂) ("__3" ?₃) ("__4" ?₄)
 ("__5" ?₅) ("__6" ?₆) ("__7" ?₇) ("__8" ?₈) ("__9" ?₉)

 ("__a" ?ₐ) ("__e" ?ₑ) ("__h" ?ₕ) ("__i" ?ᵢ) ("__k" ?ₖ)
 ("__l" ?ₗ) ("__m" ?ₘ) ("__n" ?ₙ) ("__o" ?ₒ) ("__p" ?ₚ)
 ("__r" ?ᵣ) ("__s" ?ₛ) ("__t" ?ₜ) ("__u" ?ᵤ) ("__v" ?ᵥ) ("__x" ?ₓ)
)
(mapc (lambda (x)
        (if (cddr x)
            (quail-defrule (cadr x) (car (cddr x)))))
      (append math-symbol-list-basic math-symbol-list-extended))

(setq coq-smie-user-tokens
   '(("," . ":=")
    ("∗" . "->")
    ("-∗" . "->")
    ("∗-∗" . "->")
    ("==∗" . "->")
    ("=∗" . "->")           ;; Hack to match ={E1,E2}=∗
    ("|==>" . ":=")
    ("⊢" . "->")
    ("⊣⊢" . "->")
    ("↔" . "->")
    ("←" . "<-")
    ("→" . "->")
    ("=" . "->")
    ("==" . "->")
    ("/\\" . "->")
    ("⋅" . "->")
    (":>" . ":=")
    ("by" . "now")
    ("forall" . "now")              ;; NB: this breaks current ∀ indentation.
   ))
