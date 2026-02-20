;;; magit-review.el --- Local file review tracking for Magit -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'magit-section)
(require 'subr-x)

(defgroup ysc-magit-review nil
  "Track review state of changed files in a local file."
  :group 'magit)

(defcustom ysc/magit-review-tracking-file "REVIEW.md"
  "Review tracking file relative to the repository root."
  :type 'string
  :group 'ysc-magit-review)

(defcustom ysc/magit-review-target-branch nil
  "Branch used as the review base.
When nil, fallback to `ysc/magit-review-default-target-branches'."
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "Branch"))
  :group 'ysc-magit-review)

(defcustom ysc/magit-review-default-target-branches
  '("master" "main" "origin/master" "origin/main")
  "Fallback base branches used when `ysc/magit-review-target-branch' is nil."
  :type '(repeat string)
  :group 'ysc-magit-review)

(defvar-local ysc/magit-review--range nil)
(defvar-local ysc/magit-review--target nil)
(defvar-local ysc/magit-review--latest-commits nil)

(defun ysc/magit-review--tracking-file-path ()
  "Return absolute path of the tracking file."
  (let ((root (or (magit-toplevel)
                  (user-error "Not inside a Git repository"))))
    (if (file-name-absolute-p ysc/magit-review-tracking-file)
        ysc/magit-review-tracking-file
      (expand-file-name ysc/magit-review-tracking-file root))))

(defun ysc/magit-review--resolve-target-branch ()
  "Return the target branch used for comparison."
  (cond
   ((and ysc/magit-review-target-branch
         (magit-rev-verify ysc/magit-review-target-branch))
    ysc/magit-review-target-branch)
   (ysc/magit-review-target-branch
    (user-error "Target branch `%s` not found" ysc/magit-review-target-branch))
   ((cl-find-if #'magit-rev-verify ysc/magit-review-default-target-branches))
   (t
    (user-error "No target branch found (tried: %s)"
                (string-join ysc/magit-review-default-target-branches ", ")))))

(defun ysc/magit-review--diff-range (target)
  "Return diff range from TARGET to HEAD."
  (format "%s...HEAD" target))

(defun ysc/magit-review--changed-files (range)
  "Return files changed in RANGE."
  (magit-git-lines "diff" "--name-only" "--diff-filter=ACDMRTUXB" range))

(defun ysc/magit-review--normalize-path (path)
  "Normalize PATH to repository-relative style."
  (string-remove-prefix "./" (string-trim path)))

(defun ysc/magit-review--parse-tracking-line (line)
  "Parse LINE into a cons cell of (FILE . COMMIT), or nil."
  (let ((line (string-trim line)))
    (cond
     ((or (string-empty-p line) (string-prefix-p "#" line))
      nil)
     ((string-match "\\`\\(.+?\\)[ \t]+\\([[:xdigit:]]\\{7,40\\}\\)\\'" line)
      (cons (ysc/magit-review--normalize-path (match-string 1 line))
            (downcase (match-string 2 line))))
     ((string-match "\\`\\([[:xdigit:]]\\{7,40\\}\\)[ \t]+\\(.+\\)\\'" line)
      (cons (ysc/magit-review--normalize-path (match-string 2 line))
            (downcase (match-string 1 line))))
     (t nil))))

(defun ysc/magit-review--read-tracking-table ()
  "Read tracking entries from `ysc/magit-review-tracking-file'."
  (let ((table (make-hash-table :test 'equal))
        (tracking-file (ysc/magit-review--tracking-file-path)))
    (when (file-readable-p tracking-file)
      (with-temp-buffer
        (insert-file-contents tracking-file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (when-let ((entry (ysc/magit-review--parse-tracking-line line)))
            (puthash (car entry) (cdr entry) table)))))
    table))

(defun ysc/magit-review--table-entries (table)
  "Return TABLE as sorted alist."
  (let (entries)
    (maphash (lambda (file commit)
               (push (cons file commit) entries))
             table)
    (sort entries (lambda (a b) (string< (car a) (car b))))))

(defun ysc/magit-review--write-tracking-table (table)
  "Persist TABLE to `ysc/magit-review-tracking-file'."
  (let ((tracking-file (ysc/magit-review--tracking-file-path)))
    (with-temp-file tracking-file
      (dolist (entry (ysc/magit-review--table-entries table))
        (insert (car entry) "\t" (cdr entry) "\n")))))

(defun ysc/magit-review--latest-commit-for-file (range file)
  "Return latest commit in RANGE that modified FILE."
  (magit-git-string "log" "-n" "1" "--format=%H" range "--" file))

(defun ysc/magit-review--latest-commit-table (range files)
  "Return a hash table mapping FILES to their latest commit in RANGE."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (file files)
      (when-let ((commit (ysc/magit-review--latest-commit-for-file range file)))
        (puthash file (downcase commit) table)))
    table))

(defun ysc/magit-review--commit-equal-p (a b)
  "Return non-nil when commit A and B refer to the same revision."
  (and a
       b
       (or (string-equal a b)
           (string-prefix-p a b)
           (string-prefix-p b a)
           (and (fboundp 'magit-rev-equal)
                (magit-rev-equal a b)))))

(defun ysc/magit-review--short-oid (oid)
  "Return a readable short version of OID."
  (if (and oid (> (length oid) 12))
      (substring oid 0 12)
    oid))

(defun ysc/magit-review--insert-file (file latest tracked)
  "Insert FILE section with LATEST and TRACKED commit ids."
  (magit-insert-section (review-file file)
    (let ((start (point)))
      (insert (propertize file 'font-lock-face 'magit-filename))
      (put-text-property start (point) 'ysc/magit-review-file file))
    (when latest
      (insert " "
              (propertize (format "[%s]" (ysc/magit-review--short-oid latest))
                          'font-lock-face 'magit-hash)))
    (when (and tracked (not (ysc/magit-review--commit-equal-p tracked latest)))
      (insert " "
              (propertize (format "(tracked %s)"
                                  (ysc/magit-review--short-oid tracked))
                          'font-lock-face 'shadow)))
    (insert "\n")))

(defun ysc/magit-review--insert-subsection (heading files latest-table tracked-table)
  "Insert a foldable subsection with HEADING and FILES."
  (magit-insert-section (review-subsection heading)
    (magit-insert-heading
      (concat (magit--propertize-face (concat heading " ")
                                      'magit-section-heading)
              (magit--propertize-face (format "(%d)" (length files))
                                      'magit-section-child-count)))
    (magit-insert-section-body
      (if files
          (dolist (file files)
            (ysc/magit-review--insert-file
             file
             (gethash file latest-table)
             (gethash file tracked-table)))
        (insert "  (none)\n")))))

(defun ysc/magit-review-refresh-buffer ()
  "Refresh `ysc/magit-review-mode' buffer."
  (let* ((target (or ysc/magit-review--target
                     (ysc/magit-review--resolve-target-branch)))
         (range (ysc/magit-review--diff-range target))
         (tracking-file (ysc/magit-review--tracking-file-path))
         (tracked-table (ysc/magit-review--read-tracking-table))
         (changed-files (ysc/magit-review--changed-files range))
         (latest-commits (ysc/magit-review--latest-commit-table range changed-files))
         reviewed-files
         not-reviewed-files)
    (setq-local ysc/magit-review--range range)
    (setq-local ysc/magit-review--target target)
    (setq-local ysc/magit-review--latest-commits latest-commits)
    (dolist (file changed-files)
      (let ((latest (gethash file latest-commits))
            (tracked (gethash file tracked-table)))
        (if (ysc/magit-review--commit-equal-p tracked latest)
            (push file reviewed-files)
          (push file not-reviewed-files))))
    (setq reviewed-files (nreverse reviewed-files))
    (setq not-reviewed-files (nreverse not-reviewed-files))
    (magit-set-header-line-format
      (format "Review %s" range))
    (magit-insert-section (review-root)
      (magit-insert-heading
        (concat (magit--propertize-face "Files changed "
                                        'magit-section-heading)
                (magit--propertize-face (format "(%d)" (length changed-files))
                                        'magit-section-child-count)))
      (magit-insert-section-body
        (insert (format "Target branch: %s\n" target))
        (insert (format "Tracking file: %s\n\n"
                        (file-relative-name tracking-file (magit-toplevel))))
        (ysc/magit-review--insert-subsection
         "Not yet reviewed" not-reviewed-files latest-commits tracked-table)
        (ysc/magit-review--insert-subsection
         "Reviewed" reviewed-files latest-commits tracked-table)
        (insert "\nKeys: `RET` visit file, `r` mark file reviewed at latest commit.\n")))))

(defun ysc/magit-review--file-at-point ()
  "Return reviewed file at point, or nil."
  (or (get-text-property (point) 'ysc/magit-review-file)
      (get-text-property (line-beginning-position) 'ysc/magit-review-file)))

(defun ysc/magit-review-visit-file ()
  "Visit file at point."
  (interactive)
  (when-let ((file (ysc/magit-review--file-at-point)))
    (find-file (expand-file-name file (magit-toplevel)))))

(defun ysc/magit-review-mark-file-reviewed ()
  "Mark the file at point as reviewed at its latest commit."
  (interactive)
  (let* ((file (or (ysc/magit-review--file-at-point)
                   (user-error "No file at point")))
         (commit (or (gethash file ysc/magit-review--latest-commits)
                     (user-error "No commit found for file `%s`" file)))
         (tracking (ysc/magit-review--read-tracking-table)))
    (puthash file commit tracking)
    (ysc/magit-review--write-tracking-table tracking)
    (magit-refresh-buffer)
    (message "Marked %s reviewed at %s" file (ysc/magit-review--short-oid commit))))

(defvar ysc/magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map (kbd "RET") #'ysc/magit-review-visit-file)
    (define-key map (kbd "r") #'ysc/magit-review-mark-file-reviewed)
    map))

(define-derived-mode ysc/magit-review-mode magit-mode "Magit Review"
  "Major mode for local file review tracking."
  (setq-local truncate-lines t))

(defun ysc/magit-review ()
  "Open the review buffer for the current repository."
  (interactive)
  (unless (magit-toplevel)
    (user-error "Not inside a Git repository"))
  (magit-setup-buffer #'ysc/magit-review-mode nil
    :directory (magit-toplevel)
    (ysc/magit-review--target (ysc/magit-review--resolve-target-branch))))

(with-eval-after-load 'magit
  (ignore-errors
    (transient-remove-suffix 'magit-dispatch "R"))
  (transient-insert-suffix 'magit-dispatch "o"
    '("R" "Review files" ysc/magit-review)))

(provide 'magit-review)
;;; magit-review.el ends here
