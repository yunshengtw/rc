;;; Packages and features
;; Enable installation of MELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; Disable Emacs customization feature
(setq custom-file "~/.emacs.d/custom.el")
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;;; Add homebrew binary to exec-path since emacs-plus is installed through it.
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")

;;; Add opam binary to exec-path to find coqtop.
(add-to-list 'exec-path "/Users/yunsheng/.opam/default/bin")

;;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
;; Center the dashboard content
(setq dashboard-center-content t)
;; Show 10 most recently open files
(setq dashboard-items '((recents  . 5)
						(bookmarks . 5)))
;; Show navigators below the banner
(setq dashboard-set-navigator t)
;; Create a navigator to open terminal
(setq dashboard-navigator-buttons
      `(((, nil "Terminal" "Open vterm" (lambda (&rest _) (vterm))))))
;; Disable footer
(setq dashboard-set-footer nil)

;;; VTerm
(require 'vterm)

;;; Themes
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)
(custom-set-faces
 ;; Set font size and family
 '(default ((t :family "Fira Mono" :height 150)))
 ;; Add "padding" to the tab bar
 ;; `(tab-bar ((t (:background ,(face-background 'default) :foreground ,(face-foreground 'default))))))
 `(tab-bar ((t (:background
				,(face-background 'default)
				:foreground
				,(face-foreground 'default)
				:box (:line-width 8 :color ,(face-background 'default))))))
 '(tab-bar-tab ((t (:underline nil))))
 '(tab-bar-tab-inactive ((t (:foreground "#4c566a")))))

;; Adds an empty header line to create some margin at the top
(setq-default header-line-format "")
(custom-set-faces
 `(header-line ((t (:background ,(face-background 'default) :height 0.5 :box nil)))))

;; (require 'modus-themes)
;; (defun ysc/modus-themes-custom-faces ()
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      ;; Set font size and family
;;      '(default ((t :family "Fira Mono" :height 150)))
;;      ;; Add "padding" to the tab bar
;;      `(tab-bar ((,c :box (:line-width 4 :color ,bg-tab-bar))))
;;      ;; Add "padding" to the mode lines
;;      ;; `(mode-line ((,c :box (:line-width 6 :color ,bg-mode-line-active))))
;;      ;; `(mode-line-inactive ((,c :box (:line-width 6 :color ,bg-mode-line-inactive))))
;;      )))
;; (add-hook 'modus-themes-after-load-theme-hook #'ysc/modus-themes-custom-faces)
;; (modus-themes-load-theme 'modus-vivendi)
;; (global-set-key (kbd "<f6>") 'modus-themes-toggle)

;;; Previous theme setting (to be deleted)
;; (load-theme 'dark-laptop t)
;; (set-face-attribute 'default nil :height 150 :family "Fira Mono")
;; Set face for mode line
;; (copied from https://github.com/gonsie/dotfiles/blob/master/emacs/theme.el)
;; (set-face-attribute 'mode-line nil
;;                     :background "#565063"
;;                     :foreground "white"
;;                     :box '(:line-width 8 :color "#565063")
;;                     :overline nil
;;                     :underline nil)
;; Set face for mode line for inactive (non-focused) window
;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#353644"
;;                     :foreground "white"
;;                     :box '(:line-width 8 :color "#353644")
;;                     :overline nil
;;                     :underline nil)
;; Set face for the tab bar, active/inactive tabs (somehow the `inherit' field
;; doesn't work)
;; (set-face-attribute 'tab-bar nil
;; 					:family "Fira Mono"
;;                     :background "#353644"
;;                     :foreground "white"
;;                     :box '(:line-width 6 :color "#353644")
;;                     :overline nil
;;                     :underline nil)
;; (set-face-attribute 'tab-bar-tab nil
;; 					:background "#565063"
;; 					:foreground "white")
;; (set-face-attribute 'tab-bar-tab-inactive nil
;; 					:background "#353644"
;; 					:foreground "white")

;;; Misc
;; Maximize frame on start-up
(setq default-frame-alist '((fullscreen . maximized)))
;; Open files in existing frame
;; This allows, on macOS, ``> open -a "Emacs" file'' to open in exiting frame,
;; rather than opening a new frame
(setq ns-pop-up-frames nil)
;; Show column number
(column-number-mode)
;; Interactive matching
;; (require 'ido)
;; (ido-mode)
;; Wrap around 
(setq-default isearch-wrap-pause 'no-ding)
;; Bind `occur'; overwriting `ns-open-file-using-panel'
(global-set-key (kbd "s-o") 'occur)
(global-set-key (kbd "s-r") 'undo-redo)
;; Put lock files in /var/tmp
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
;; Switch to the most recent previously selected buffer
(global-set-key (kbd "C-`") 'mode-line-other-buffer)

;;; Tab bar
;; Refresh on creating a new tab for dashboard
(defun ysc/tab-bar-new-dashboard ()
  (interactive)
  (tab-bar-new-tab)
  (dashboard-refresh-buffer))
;; (copied from http://www.gonsie.com/blorg/tab-bar.html)
(when (< 26 emacs-major-version)
  ;; hide bar if <= 1 tabs open
  (setq tab-bar-show 1)
  ;; hide tab close / X button
  (setq tab-bar-close-button-show nil)
  ;; buffer to show in new tabs
  (setq tab-bar-new-tab-choice "*dashboard*")
  ;; show tab numbers
  (setq tab-bar-tab-hints t)
  (setq tab-bar-auto-width nil)
  ;; elements to include in bar
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (setq tab-bar-close-last-tab-choice
	(lambda (tab)
	  (delete-other-windows)
	  (dashboard-open))))

;; Set up key bindings for selecting tab with tab numbers
;; (copied from https://github.com/gonsie/dotfiles/blob/master/emacs/my-keybindings.el)
(global-set-key (kbd "s-t") 'ysc/tab-bar-new-dashboard)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "s-1") 'tab-bar-select-tab)
(global-set-key (kbd "s-2") 'tab-bar-select-tab)
(global-set-key (kbd "s-3") 'tab-bar-select-tab)
(global-set-key (kbd "s-4") 'tab-bar-select-tab)
(global-set-key (kbd "s-5") 'tab-bar-select-tab)
(global-set-key (kbd "s-6") 'tab-bar-select-tab)
(global-set-key (kbd "s-7") 'tab-bar-select-tab)
(global-set-key (kbd "s-8") 'tab-bar-select-tab)
(global-set-key (kbd "s-9") 'tab-bar-select-tab)
(global-set-key (kbd "s-0") 'tab-bar-select-tab)

;;; Other bars
;; Disable tool bar
(tool-bar-mode -1)

;;; Frame management
(global-set-key (kbd "s-\"") 'toggle-frame-maximized)
(global-set-key (kbd "s-'") 'toggle-frame-fullscreen)

;;; Window management
(global-set-key (kbd "s-X") 'delete-window)
(global-set-key (kbd "s-|") 'split-window-right)
(global-set-key (kbd "s-S") 'split-window-below)
(global-set-key (kbd "s-A") 'delete-other-windows)
(global-set-key (kbd "M-<tab>") 'other-window)

;;; Build
;; Bind to `recompile' which directly invokes `compile-command'
(global-set-key (kbd "s-b") 'recompile)
(global-set-key (kbd "s-B") 'compile)
(global-set-key (kbd "s-]") 'next-error)
(global-set-key (kbd "s-[") 'previous-error)
(global-set-key (kbd "C-c C-k") 'kill-compilation)

;;; Registers
(global-set-key (kbd "s-p") 'point-to-register)
(global-set-key (kbd "s-j") 'jump-to-register)

;;; Misc
(global-set-key (kbd "M-`") 'shell-command)
(global-set-key (kbd "s-/") 'set-mark-command)

;;; Flymake
(add-hook 'flymake-mode-hook
		  (lambda ()
			(global-set-key (kbd "s-e") 'flymake-goto-next-error)))

;;; Editor
;; Tab width = 4
(setq-default tab-width 4)
;; Maximum line width = 80
(setq-default fill-column 80)
;; Enable automatically breaking lines on typing `SPC' or `RET'
(auto-fill-mode)
;; Scroll one line at a time
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;;; Language server protocol (eglot)
(require 'eglot)

;;; Elisp
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(company-mode)))

;;; Proof General
;; Enable double-hit electric terminator mode
(setq coq-double-hit-enable t)
(setq proof-electric-terminator-enable nil)
;; Two-column display: script | goals + response
(setq proof-three-window-mode-policy 'hybrid)
;; Disable automatically inserting space/newlines after typing terminator
(setq proof-next-command-insert-space nil)
;; Disable formatting for newlines after each command
(setq coq-one-command-per-line nil)
;; Disable arrow pointing to the current line in script
;; (setq overlay-arrow-string "")
(defun ysc/proof-retract-buffer ()
  (interactive)
  ;; Retract the buffer without moving the point to the start
  (proof-retract-buffer nil))
(add-hook 'coq-mode-hook
		  (lambda ()
			(local-set-key (kbd "S-<return>") 'electric-indent-just-newline)
			(local-set-key (kbd "C-c C-o") 'proof-omit-proofs-option-toggle)
			(local-set-key (kbd "C-c C-r") 'ysc/proof-retract-buffer)))


;;; Company Coq
(add-hook 'coq-mode-hook
	  (lambda ()
		;; Incompatible with mixcode
		(setq company-coq-disabled-features '(smart-subscripts))
	    (company-coq-mode)
	    ;; Avoid the image lighter that increases the mode line height
	    (setcar (cdr (assq 'company-coq-mode minor-mode-alist)) " company-coq")))
(add-hook 'coq-mode-hook
	  (lambda ()
	    ;; Set the default compile command to "./vos.sh" for Coq mode
	    (setq-local compile-command "./vos.sh")
	    ;; Indent `mixcode' correctly
	    (setq tab-width 2)))
;; Fold the bullets when opening a file
(setq company-coq-initial-fold-state 'bullets)

;;; Cheat Sheet
(defun cheat-sheet ()
  (interactive)
  (with-output-to-temp-buffer "*cheat*"
    (switch-to-buffer-other-window "*cheat*")
    (insert-file-contents "~/.emacs.d/cheat-key.org")))
(global-set-key (kbd "s-h") 'cheat-sheet)

;;; Go
(require 'go-mode)
;; Update the PATH env var so that we can find `golps'
(add-to-list 'exec-path "~/go/bin")
(add-hook 'go-mode-hook
	  (lambda ()
	    ;; Set the default compile command to "go build" for Go mode
	    (setq-local compile-command "go build")
	    (eglot-ensure)
	    (company-mode)))

;; Iris (clean-up/comments required)
(add-hook 'coq-mode-hook
	  (lambda ()
	    (load "~/.emacs.d/iris.el")))

;;; Git
(require 'magit)
;; Bind to open magit status buffer
(global-set-key (kbd "s-G") 'magit-status)
;; Unbind to allow `tab-next'
(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "C-<tab>") nil))

;;; Markdown (with support for previewing in GitHub style)
;; (adapted from https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/)
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -t html5"))

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host "localhost"))

(use-package impatient-mode
  :ensure t
  :commands impatient-mode)

(defun markdown-github-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun markdown-github-preview ()
  "Preview markdown in Github style."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'markdown-github-filter)
  (imp-visit-buffer))

;; Mixing code
;; (load-file "~/Repos/mixcode/mixcode.el")
;; TODO: remove this after debug
(add-hook 'coq-mode-hook
		  (lambda ()
			(load-file "~/Repos/perennial/etc/mixcode/mixcode.el")
			(mixcode-mode)
			(setq-local mixcode-source-dir "~/Repos/tulip/backup/")
			(setq-local mixcode-source-file "backup.go")))
;; (add-hook 'text-mode-hook
;; 		  (lambda ()
;; 			(load-file "~/Repos/mixcode/mixcode.el")
;; 			(setq-local mixcode-source-dir "~/Repos/mixcode/")
;; 			(setq-local mixcode-source-file "hello.go")))

;; Displaying PDF files
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands pdf-view-mode
  :config
  (pdf-tools-install :no-query))
(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    ;; Auto-update the pdf contents 
	    (auto-revert-mode)
	    (setq auto-revert-interval 0.5)
	    (auto-revert-set-timer)))
(defun dedicate-current-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t))

;; Drawing TikZ figures (not a very mature package, consider rewrite)
;; (add-hook 'tex-mode-hook
;; 		  (lambda ()
;; 			(load-file "~/Repos/tikz-emacs/tikz.el")))

(add-hook 'org-mode-hook
	  (lambda ()
	    (company-mode)
	    (setq-local company-backends '(company-dabbrev)
			company-minimum-prefix-length 2
			company-dabbrev-minimum-length 2
			company-dabbrev-other-buffers nil
			company-dabbrev-ignore-case t
			company-dabbrev-downcase nil)))

;;; Python
(require 'elpy)
(defun ysc/elpy-shell-send-to-point ()
  (interactive)
  (push-mark (point-min) nil t)
  (elpy-shell-send-region-or-buffer)
  (deactivate-mark))
(setq python-indent-guess-indent-offset nil)
(add-hook 'python-mode-hook
		  (lambda ()
			(elpy-enable)
			(local-set-key (kbd "C-c C-<return>") 'ysc/elpy-shell-send-to-point)
			(local-set-key (kbd "C-c C-c") 'elpy-shell-send-codecell)
			;; TODO: restarting
			(local-set-key (kbd "C-c C-n") 'elpy-shell-send-statement-and-step)
			(local-set-key (kbd "C-<return>") 'elpy-shell-send-statement)
			))
;; Unbind elpy keys
(with-eval-after-load "elpy-shell"
  (define-key elpy-mode-map (kbd "C-<return>") nil)
  (define-key elpy-mode-map (kbd "C-c C-n") nil)
  (define-key elpy-mode-map (kbd "C-c C-c") nil))
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --classic")
;; This suppresses an unimportant warning
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "ipython3")
(add-to-list 'exec-path "~/Library/Python/3.9/bin")

;;; Inline image in the REPL buffer
(add-hook 'inferior-python-mode-hook
		  (lambda ()
			(load-file "~/Repos/comint-mime/comint-mime.el")
			(comint-mime-setup)
			(setq comint-move-point-for-output t)
			(setq comint-scroll-to-bottom-on-input t)
			(setq comint-scroll-show-maximum-output 'other)
			))
;; Potential bug of `comint-mime': Sometimes using `display' in ipython3 would
;; render `comint-scroll-show-maximum-output' ineffective.

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
