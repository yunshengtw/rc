;;; Packages and features  -*- lexical-binding: t; -*-
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
;; (add-to-list 'exec-path "/Users/yunsheng/.opam/default/bin")

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

;;; Themes and faces

(add-to-list 'custom-theme-load-path "~/.emacs.d/")
(load-theme 'ysc t)

(custom-set-faces
 ;; Set font size and family
 ;; Note: The `t` here means "for every display (GUI, terminal, etc.)"
 '(default ((t (:family "Fira Mono" :height 150))))
 ;; Disable slant comments
 '(font-lock-comment-face ((t (:slant normal))))
 '(font-lock-comment-delimiter-face ((t (:slant normal)))))

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
;; Find file under a project
(global-set-key (kbd "C-x C-p") 'project-find-file)
;; Open find-definition in another window
(global-set-key (kbd "M->") 'xref-find-definitions-other-window)
;; Find reference
(global-set-key (kbd "M-/") 'xref-find-references)

;;; Tab bar
;; Refresh on creating a new tab for dashboard
(defun ysc/tab-bar-new-dashboard ()
  (interactive)
  (tab-bar-new-tab)
  (dashboard-refresh-buffer))
;; (copied from http://www.gonsie.com/blorg/tab-bar.html)
(when (< 26 emacs-major-version)
  ;; hide bar if <= 1 tabs open
  ;; (setq tab-bar-show t)
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
(tab-bar-mode)

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
; (global-set-key (kbd "s-\"") 'toggle-frame-maximized)
; (global-set-key (kbd "s-'") 'toggle-frame-fullscreen)

;;; Bell
(setq ring-bell-function 'ignore)

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
(setq compilation-always-kill t)

;;; Registers
(global-set-key (kbd "s-p") 'point-to-register)
(global-set-key (kbd "s-j") 'jump-to-register)

;;; Dired
(add-hook 'dired-mode-hook 'dired-omit-mode)
(global-set-key (kbd "s-D") 'dired-sidebar-toggle-sidebar)

;;; Misc
(global-set-key (kbd "M-`") 'shell-command)
(global-set-key (kbd "s-/") 'set-mark-command)
(global-set-key (kbd "S-<return>") 'electric-indent-just-newline)

;;; Backup files
;; Disable creation of backup files (i.e., files end with "~")
(setq make-backup-files nil)

;;; Flymake
(use-package flymake
  :ensure t
  :custom
  ;; Show diagnostics in new lines (requires Emacs version >= 30)
  (flymake-show-diagnostics-at-end-of-line 'fancy))
(add-hook 'flymake-mode-hook
		  (lambda ()
			(global-set-key (kbd "s-e") 'flymake-goto-next-error)))
;; Disable the exclamation marks and question marks in the margin
(with-eval-after-load 'eglot
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (put type 'flymake-overlay-control '((before-string . nil) (after-string . nil)))
    (put type 'flymake-bitmap nil)))


;;; Editor
;; Tab width = 4
(setq-default tab-width 4)
;; Maximum line width = 100
(setq-default fill-column 100)
;; Enable automatically breaking lines on typing `SPC' or `RET'
(auto-fill-mode)
;; Scroll one line at a time
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(global-set-key (kbd "s-ㄋ") 'save-buffer)


;;; Language server protocol (eglot)
(require 'eglot)
;; Disable inlay parameter names for Rust
(add-hook 'eglot-managed-mode-hook
		  (lambda ()
			(when (derived-mode-p 'rust-mode)
			  (eglot-inlay-hints-mode -1))))
(defun toggle-eglot-inlay-hints-mode ()
  (interactive)
  (eglot-inlay-hints-mode 'toggle)
  (when eglot-inlay-hints-mode
	;; Force refresh the hints
	;;
	;; We use `fboundp` here because functions with `--` are internal, and thus the name might
	;; change in future release
	(if (fboundp 'eglot--update-hints)
		(eglot--update-hints (point-min) (point-max))
	  (message "eglot--update-hints not available"))))
(global-set-key (kbd "s-h") 'toggle-eglot-inlay-hints-mode)
;; Disable advertising of eglot actions
(setq eglot-code-action-indications nil)

;;; Elisp
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(company-mode)))

;;; Proof General
(require 'opam-switch-mode)
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
			(local-set-key (kbd "C-c C-o") 'proof-omit-proofs-option-toggle)
			(local-set-key (kbd "C-c C-r") 'ysc/proof-retract-buffer)
			(opam-switch-mode)))

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
(global-set-key (kbd "s-H") 'cheat-sheet)

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

;;; Rust
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda ()
			;; Set the default compile command to "cargo run" for Rust mode
			(setq-local compile-command "cargo build")
			;; Set the max line width to 100
			(setq-local fill-column 100)
			;; Indent with space is encouraged in Rust
			(setq indent-tabs-mode nil)
			;; Prettify symbols
			(prettify-symbols-mode)
			;; Enable company mode
			(company-mode)
			;; Enable language server protocol
			(eglot-ensure)))
(with-eval-after-load 'eglot
  ;; Disable showing immediate diagnostics for Rust to avoid redundant information generated with
  ;; rust-analyzer.
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer . (:diagnostics (:enable :json-false)
									 :inlayHints (:bindingModeHints (:enable t)
												  :typeHints (:enable t)
												  :chainingHints (:enable t)
												  :parameterHints (:enable :json-false)))))))

;;; Iris (clean-up/comments required)
(add-hook 'coq-mode-hook
		  (lambda ()
			(load "~/.emacs.d/iris.el")))

;;; Latex
(use-package latex
  :custom
  (font-latex-user-keyword-classes
   '(("additional-keywords"
	  ;; The "{" means that this command takes one mandatory argument
      (("autoref" "{")
	   ("cc" "{"))
	  ;; Face to use for the argument (the command itself will be using the keyword face)
      font-lock-constant-face
	  ;; Type command is for `\cmd{arg}`; type declaration is for {\decl body}
      command))))

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
  (setq markdown-command "pandoc -t html5 --mathml"))

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

;;; Deft
(use-package deft
  ;; :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Repos/notes/"
                deft-extensions '("md")
				deft-recursive t))


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

;; https://emacs.stackexchange.com/a/77387
(setq frame-resize-pixelwise t)
