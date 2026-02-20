;;; magit-review.el --- Local file review tracking for Magit -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'magit-section)
(require 'subr-x)

(defvar magit-review-file-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-file-section-map)
    (define-key map (kbd "RET") #'ysc/magit-review-visit-file)
    (define-key map (kbd "r") #'ysc/magit-review-mark-file-reviewed)
    (define-key map (kbd "k") #'ysc/magit-review-unmark-file-reviewed)
    (define-key map (kbd "u") #'ysc/magit-review-undo-file-review)
    map))

(defclass magit-review-file-section (magit-file-section)
  ((keymap :initform 'magit-review-file-section-map)))

(setf (alist-get 'review-file magit--section-type-alist)
      'magit-review-file-section)

(defgroup ysc-magit-review nil
  "Track review state of changed files in a local file."
  :group 'magit)

(defcustom ysc/magit-review-tracking-file "REVIEW.md"
  "Review tracking file relative to the repository root."
  :type 'string
  :group 'ysc-magit-review)

(defcustom ysc/magit-review-base-branch nil
  "Branch used as the review base.
When nil, fallback to `ysc/magit-review-default-base-branches'."
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "Branch"))
  :group 'ysc-magit-review)

(defcustom ysc/magit-review-default-base-branches
  '("master" "main" "origin/master" "origin/main")
  "Fallback base branches used when `ysc/magit-review-base-branch' is nil."
  :type '(repeat string)
  :group 'ysc-magit-review)

(define-obsolete-variable-alias
  'ysc/magit-review-target-branch
  'ysc/magit-review-base-branch
  "ysc/magit-review")

(define-obsolete-variable-alias
  'ysc/magit-review-default-target-branches
  'ysc/magit-review-default-base-branches
  "ysc/magit-review")

(defvar-local ysc/magit-review--range nil)
(defvar-local ysc/magit-review--commit-range nil)
(defvar-local ysc/magit-review--base nil)
(defvar-local ysc/magit-review--latest-commits nil)
(defvar-local ysc/magit-review--worktree-tracking nil)
(defvar-local ysc/magit-review--head-tracking nil)
(defvar-local ysc/magit-review--status-table nil)
(defvar-local ysc/magit-review--head-status-table nil)
(defvar-local ysc/magit-review--worktree-status-changes nil)
(defvar-local ysc/magit-review--numstat-table nil)
(defvar-local ysc/magit-review--stat-file-width nil)
(defvar-local ysc/magit-review--stat-count-width nil)

(defun ysc/magit-review--tracking-file-path ()
  "Return absolute path of the tracking file."
  (let ((root (or (magit-toplevel)
                  (user-error "Not inside a Git repository"))))
    (if (file-name-absolute-p ysc/magit-review-tracking-file)
        ysc/magit-review-tracking-file
      (expand-file-name ysc/magit-review-tracking-file root))))

(defun ysc/magit-review--tracking-file-repo-path ()
  "Return tracking file path relative to repository root, or nil."
  (let* ((root (magit-toplevel))
         (file (ysc/magit-review--tracking-file-path)))
    (when (and root (file-in-directory-p file root))
      (file-relative-name file root))))

(defun ysc/magit-review--resolve-base-branch ()
  "Return the base branch used for comparison."
  (cond
   ((and ysc/magit-review-base-branch
         (magit-rev-verify ysc/magit-review-base-branch))
    ysc/magit-review-base-branch)
   (ysc/magit-review-base-branch
    (user-error "Base branch `%s` not found" ysc/magit-review-base-branch))
   ((cl-find-if #'magit-rev-verify ysc/magit-review-default-base-branches))
   (t
    (user-error "No base branch found (tried: %s)"
                (string-join ysc/magit-review-default-base-branches ", ")))))

(defun ysc/magit-review--diff-range (base)
  "Return diff range from BASE to HEAD."
  (format "%s...HEAD" base))

(defun ysc/magit-review--commit-range (base)
  "Return commit range from BASE to HEAD."
  (format "%s..HEAD" base))

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

(defun ysc/magit-review--parse-tracking-content (content)
  "Parse CONTENT and return a tracking hash table."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (line (split-string content "\n" t))
      (when-let ((entry (ysc/magit-review--parse-tracking-line line)))
        (puthash (car entry) (cdr entry) table)))
    table))

(defun ysc/magit-review--read-tracking-table ()
  "Read tracking entries from `ysc/magit-review-tracking-file'."
  (let ((tracking-file (ysc/magit-review--tracking-file-path)))
    (if (file-readable-p tracking-file)
        (with-temp-buffer
          (insert-file-contents tracking-file)
          (ysc/magit-review--parse-tracking-content (buffer-string)))
      (make-hash-table :test 'equal))))

(defun ysc/magit-review--read-tracking-table-from-revision (rev)
  "Read tracking entries from REV's version of tracking file."
  (if-let ((path (ysc/magit-review--tracking-file-repo-path))
           (content (magit-git-string "show" (format "%s:%s" rev path))))
      (ysc/magit-review--parse-tracking-content content)
    (make-hash-table :test 'equal)))

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

(defun ysc/magit-review--file-numstat (range file)
  "Return plist with numstat for FILE in RANGE."
  (if-let* ((line (car (magit-git-lines "diff" "--numstat" range "--" file)))
            (_ (string-match "\\`\\([0-9-]+\\)\t\\([0-9-]+\\)\t" line)))
      (let* ((add-str (match-string 1 line))
             (del-str (match-string 2 line))
             (add (unless (string-equal add-str "-")
                    (string-to-number add-str)))
             (del (unless (string-equal del-str "-")
                    (string-to-number del-str))))
        (list :add add
              :del del
              :total (and add del (+ add del))))
    (list :add 0 :del 0 :total 0)))

(defun ysc/magit-review--numstat-table (range files)
  "Return hash table mapping FILES to numstat plists in RANGE."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (file files)
      (puthash file (ysc/magit-review--file-numstat range file) table))
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

(defun ysc/magit-review--review-status (tracked latest)
  "Return review status symbol for TRACKED and LATEST commits."
  (cond
   ((null tracked) 'not-reviewed)
   ((ysc/magit-review--commit-equal-p tracked latest) 'reviewed)
   (t 'changed-after-reviewed)))

(defun ysc/magit-review--status-label (status)
  "Return human-readable label for STATUS."
  (pcase status
    ('not-reviewed "Not reviewed")
    ('changed-after-reviewed "Changed after reviewed")
    ('reviewed "Reviewed")
    (_ "Unknown")))

(defun ysc/magit-review--insert-diffstat-graph (numstat)
  "Insert a magit-revision-style diffstat graph from NUMSTAT."
  (let* ((add (plist-get numstat :add))
         (del (plist-get numstat :del))
         (width 8))
    (if (or (null add) (null del))
        (insert (propertize (make-string width ?.) 'font-lock-face 'shadow))
      (let* ((total (max 1 (+ add del)))
             (add-len (if (> add 0)
                          (max 1 (round (* width (/ (float add) total))))
                        0))
             (del-len (if (> del 0)
                          (max 1 (round (* width (/ (float del) total))))
                        0)))
        (while (> (+ add-len del-len) width)
          (if (> add-len del-len)
              (setq add-len (1- add-len))
            (setq del-len (1- del-len))))
        (let ((dot-len (- width add-len del-len)))
          (when (> add-len 0)
            (insert (propertize (make-string add-len ?+)
                                'font-lock-face 'magit-diffstat-added)))
          (when (> del-len 0)
            (insert (propertize (make-string del-len ?-)
                                'font-lock-face 'magit-diffstat-removed)))
          (when (> dot-len 0)
            (insert (propertize (make-string dot-len ?.)
                                'font-lock-face 'shadow))))))))

(defun ysc/magit-review--diffstat-graph-string (numstat)
  "Return diffstat graph string for NUMSTAT."
  (with-temp-buffer
    (ysc/magit-review--insert-diffstat-graph numstat)
    (buffer-string)))

(defun ysc/magit-review--file-heading-string (file changed-in-worktree)
  "Return file heading string for FILE."
  (magit-format-file 'stat
                     (if changed-in-worktree
                         (concat file "*")
                       file)
                     'magit-filename))

(defun ysc/magit-review--compute-stat-widths (files)
  "Return cons of (FILE-WIDTH . COUNT-WIDTH) for FILES."
  (let ((file-width 0)
        (count-width 0))
    (dolist (file files)
      (let* ((changed-in-worktree (gethash file ysc/magit-review--worktree-status-changes))
             (name (ysc/magit-review--file-heading-string file changed-in-worktree))
             (numstat (or (gethash file ysc/magit-review--numstat-table)
                          (list :add 0 :del 0 :total 0)))
             (count (plist-get numstat :total))
             (count-str (if count (number-to-string count) "Bin")))
        (setq file-width (max file-width (string-width name)))
        (setq count-width (max count-width (string-width count-str)))))
    (cons file-width count-width)))

(defun ysc/magit-review--line-change-totals (files numstat-table)
  "Return cons cell (ADDED . REMOVED) for FILES using NUMSTAT-TABLE."
  (let ((added 0)
        (removed 0))
    (dolist (file files)
      (let* ((numstat (gethash file numstat-table))
             (add (plist-get numstat :add))
             (del (plist-get numstat :del)))
        (when add
          (setq added (+ added add)))
        (when del
          (setq removed (+ removed del)))))
    (cons added removed)))

(defun ysc/magit-review--line-change-summary (files numstat-table)
  "Return Git-style line-change summary string for FILES."
  (pcase-let ((`(,added . ,removed)
               (ysc/magit-review--line-change-totals files numstat-table)))
    (format "%d insertions(+), %d deletions(-)" added removed)))

(defun ysc/magit-review--insert-summary-fields (fields)
  "Insert aligned summary FIELDS.
FIELDS is a list of cons cells (LABEL . VALUE)."
  (let ((label-width (apply #'max (mapcar (lambda (field)
                                            (string-width (car field)))
                                          fields))))
    (dolist (field fields)
      (insert (format (format "%%-%ds %%s\n" label-width)
                      (car field)
                      (cdr field))))))

(defun ysc/magit-review--file-heading (file changed-in-worktree numstat)
  "Return heading line text for FILE."
  (let* ((name (ysc/magit-review--file-heading-string file changed-in-worktree))
         (file-width (or ysc/magit-review--stat-file-width (string-width name)))
         (total (plist-get numstat :total)))
    (concat
     name
     (make-string (max 1 (1+ (- file-width (string-width name)))) ?\s)
     "| "
     (format (format "%%%ds " (or ysc/magit-review--stat-count-width 3))
             (if total
                 (number-to-string total)
               "Bin"))
     (ysc/magit-review--diffstat-graph-string numstat))))

(defun ysc/magit-review--wash-hunk ()
  "Wash one diff hunk into a `magit-hunk-section'."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let* ((heading (match-string 0))
           (ranges (mapcar
                    (lambda (str)
                      (let ((range
                             (mapcar #'string-to-number
                                     (split-string (substring str 1) ","))))
                        (if (= (length range) 1)
                            (nconc range (list 1))
                          range)))
                    (split-string (match-string 1))))
           (about (match-string 2))
           (combined (= (length ranges) 3))
           (value (cons about ranges)))
      (magit-delete-line)
      (magit-insert-section
          ( hunk value nil
            :combined combined
            :from-range (if combined (butlast ranges) (car ranges))
            :to-range (car (last ranges))
            :about about)
        (magit-insert-heading
          (propertize (concat heading "\n")
                      'font-lock-face 'magit-diff-hunk-heading))
        (while (not (or (eobp) (looking-at "^[^-+ \\\\]")))
          (forward-line))))
    t))

(defun ysc/magit-review--wash-diff-into-hunks (beg end)
  "Turn raw diff in region BEG..END into hunk sections."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (cond
       ((re-search-forward "^@\\{2,\\} " end-marker t)
        (goto-char (line-beginning-position))
        (delete-region beg (point))
        (goto-char beg)
        (magit-wash-sequence #'ysc/magit-review--wash-hunk)
        (when (< (point) end-marker)
          (delete-region (point) end-marker)))
       (t
        (let ((raw (buffer-substring-no-properties beg end-marker)))
          (delete-region beg end-marker)
          (insert (if (string-match-p "^Binary files " raw)
                      "  (binary diff)\n"
                    "  (no textual diff)\n"))))))
    (set-marker end-marker nil)))

(defun ysc/magit-review--insert-file-diff (file status base tracked)
  "Insert expandable diff for FILE according to STATUS."
  (let ((comparison-base (pcase status
                           ('not-reviewed base)
                           ('reviewed base)
                           ('changed-after-reviewed tracked)
                           (_ nil))))
    (when comparison-base
      (magit-insert-section-body
        (insert (propertize (format "Diff against %s\n" comparison-base)
                            'font-lock-face 'shadow))
        (if (not (magit-rev-verify comparison-base))
            (insert (format "Cannot resolve base revision `%s`.\n\n" comparison-base))
          (let ((beg (point)))
            (magit-git-insert "diff" "-p" "--no-prefix" "--no-color" "--no-ext-diff"
                              (format "%s..HEAD" comparison-base)
                              "--" file)
            (when (= beg (point))
              (insert "  (no changes)\n"))
            (ysc/magit-review--wash-diff-into-hunks beg (point))
            (unless (bolp)
              (insert "\n"))
            (insert "\n")))))))

(defun ysc/magit-review--insert-file (file)
  "Insert FILE section."
  (let* ((status (gethash file ysc/magit-review--status-table))
         (changed-in-worktree (gethash file ysc/magit-review--worktree-status-changes))
         (numstat (or (gethash file ysc/magit-review--numstat-table)
                      (list :add 0 :del 0 :total 0)))
         (tracked (gethash file ysc/magit-review--worktree-tracking)))
    (magit-insert-section (review-file file t)
      (magit-insert-heading
        (ysc/magit-review--file-heading file changed-in-worktree numstat))
      (ysc/magit-review--insert-file-diff file status ysc/magit-review--base tracked))))

(defun ysc/magit-review--insert-subsection (heading files)
  "Insert a foldable subsection with HEADING and FILES."
  (magit-insert-section (review-subsection heading t)
    (magit-insert-heading
      (concat (magit--propertize-face (concat heading " ")
                                      'magit-section-heading)
              (magit--propertize-face (format "(%d)" (length files))
                                      'magit-section-child-count)))
    (magit-insert-section-body
      (if files
          (dolist (file files)
            (ysc/magit-review--insert-file file))
        (insert "  (none)\n")))))

(defun ysc/magit-review-refresh-buffer ()
  "Refresh `ysc/magit-review-mode' buffer."
  (let* ((base (or ysc/magit-review--base
                   (ysc/magit-review--resolve-base-branch)))
         (range (ysc/magit-review--diff-range base))
         (commit-range (ysc/magit-review--commit-range base))
         (tracking-file (ysc/magit-review--tracking-file-path))
         (worktree-tracked-table (ysc/magit-review--read-tracking-table))
         (head-tracked-table (ysc/magit-review--read-tracking-table-from-revision "HEAD"))
         (changed-files (ysc/magit-review--changed-files range))
         (latest-commits (ysc/magit-review--latest-commit-table commit-range changed-files))
         (numstat-table (ysc/magit-review--numstat-table range changed-files))
         (status-table (make-hash-table :test 'equal))
         (head-status-table (make-hash-table :test 'equal))
         (worktree-status-changes (make-hash-table :test 'equal))
         (line-change-summary "")
         not-reviewed-files
         changed-after-reviewed-files
         reviewed-files)
    (setq-local ysc/magit-review--range range)
    (setq-local ysc/magit-review--commit-range commit-range)
    (setq-local ysc/magit-review--base base)
    (setq-local ysc/magit-review--latest-commits latest-commits)
    (setq-local ysc/magit-review--numstat-table numstat-table)
    (setq-local ysc/magit-review--worktree-tracking worktree-tracked-table)
    (setq-local ysc/magit-review--head-tracking head-tracked-table)
    (setq-local ysc/magit-review--status-table status-table)
    (setq-local ysc/magit-review--head-status-table head-status-table)
    (setq-local ysc/magit-review--worktree-status-changes worktree-status-changes)
    (dolist (file changed-files)
      (let* ((latest (gethash file latest-commits))
             (tracked (gethash file worktree-tracked-table))
             (head-tracked (gethash file head-tracked-table))
             (status (ysc/magit-review--review-status tracked latest))
             (head-status (ysc/magit-review--review-status head-tracked latest)))
        (puthash file status status-table)
        (puthash file head-status head-status-table)
        (when (not (eq status head-status))
          (puthash file t worktree-status-changes))
        (pcase status
          ('not-reviewed
           (push file not-reviewed-files))
          ('changed-after-reviewed
           (push file changed-after-reviewed-files))
          ('reviewed
           (push file reviewed-files)))))
    (pcase-let ((`(,file-width . ,count-width)
                 (ysc/magit-review--compute-stat-widths changed-files)))
      (setq-local ysc/magit-review--stat-file-width file-width)
      (setq-local ysc/magit-review--stat-count-width count-width))
    (setq line-change-summary
          (ysc/magit-review--line-change-summary changed-files numstat-table))
    (setq reviewed-files (nreverse reviewed-files))
    (setq changed-after-reviewed-files (nreverse changed-after-reviewed-files))
    (setq not-reviewed-files (nreverse not-reviewed-files))
    (magit-set-header-line-format
     (format "Review %s" range))
    (magit-insert-section (review-root)
      (magit-insert-section (review-summary nil t)
        (magit-insert-heading "Summary")
        (magit-insert-section-body
          (ysc/magit-review--insert-summary-fields
           (list (cons "Base branch:" base)
                 (cons "Tracking file:"
                       (file-relative-name tracking-file (magit-toplevel)))
                 (cons "Files changed:" (number-to-string (length changed-files)))
                 (cons "Lines changed:" line-change-summary)))))
      (insert "\n")
      (ysc/magit-review--insert-subsection
       "Not reviewed" not-reviewed-files)
      (ysc/magit-review--insert-subsection
       "Changed after reviewed" changed-after-reviewed-files)
      (ysc/magit-review--insert-subsection
       "Reviewed" reviewed-files)
      (insert "\n`*` means review status differs from HEAD in the worktree.\n")
      (insert "Keys: `TAB` toggle section, `RET` visit file, `r` mark reviewed, `k` unmark reviewed, `u` undo worktree status change.\n"))))

(defun ysc/magit-review--file-at-point ()
  "Return reviewed file at point, or nil."
  (when-let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'review-file)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun ysc/magit-review--status-at-point ()
  "Return review status for file at point."
  (when-let ((file (ysc/magit-review--file-at-point)))
    (gethash file ysc/magit-review--status-table)))

(defun ysc/magit-review-visit-file ()
  "Visit file at point."
  (interactive)
  (when-let* ((file (ysc/magit-review--file-at-point))
              (path (expand-file-name file (magit-toplevel))))
    (if (file-exists-p path)
        (find-file path)
      (message "File `%s` does not exist in worktree" file))))

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

(defun ysc/magit-review-unmark-file-reviewed ()
  "Convert file at point to `Not reviewed'."
  (interactive)
  (let* ((file (or (ysc/magit-review--file-at-point)
                   (user-error "No file at point")))
         (status (ysc/magit-review--status-at-point)))
    (unless (memq status '(reviewed changed-after-reviewed))
      (user-error "File `%s` is already Not reviewed" file))
    (let ((tracking (ysc/magit-review--read-tracking-table)))
      (remhash file tracking)
      (ysc/magit-review--write-tracking-table tracking)
      (magit-refresh-buffer)
      (message "Converted %s to Not reviewed" file))))

(defun ysc/magit-review-undo-file-review ()
  "Undo worktree review status changes for file at point by restoring HEAD status."
  (interactive)
  (let* ((file (or (ysc/magit-review--file-at-point)
                   (user-error "No file at point")))
         (status (gethash file ysc/magit-review--status-table))
         (head-status (gethash file ysc/magit-review--head-status-table)))
    (if (eq status head-status)
        (message "No worktree review-status change for %s" file)
      (let* ((tracking (ysc/magit-review--read-tracking-table))
             (head-tracking (ysc/magit-review--read-tracking-table-from-revision "HEAD"))
             (missing (make-symbol "missing"))
             (head-entry (gethash file head-tracking missing)))
        (if (eq head-entry missing)
            (remhash file tracking)
          (puthash file head-entry tracking))
        (ysc/magit-review--write-tracking-table tracking)
        (magit-refresh-buffer)
        (message "Restored %s to HEAD review status (%s)"
                 file
                 (ysc/magit-review--status-label head-status))))))

(defvar ysc/magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map (kbd "RET") #'ysc/magit-review-visit-file)
    (define-key map (kbd "r") #'ysc/magit-review-mark-file-reviewed)
    (define-key map (kbd "k") #'ysc/magit-review-unmark-file-reviewed)
    (define-key map (kbd "u") #'ysc/magit-review-undo-file-review)
    map))

(define-derived-mode ysc/magit-review-mode magit-mode "Magit Review"
  "Major mode for local file review tracking."
  (setq-local truncate-lines t)
  ;; Keep review actions available even when point is inside expanded hunks.
  (setq-local magit-hunk-section-map
              (let ((map (copy-keymap magit-hunk-section-map)))
                (define-key map (kbd "r") #'ysc/magit-review-mark-file-reviewed)
                (define-key map (kbd "k") #'ysc/magit-review-unmark-file-reviewed)
                (define-key map (kbd "u") #'ysc/magit-review-undo-file-review)
                map)))

(defun ysc/magit-review ()
  "Open the review buffer for the current repository."
  (interactive)
  (unless (magit-toplevel)
    (user-error "Not inside a Git repository"))
  (magit-setup-buffer #'ysc/magit-review-mode nil
    :directory (magit-toplevel)
    (ysc/magit-review--base (ysc/magit-review--resolve-base-branch))))

(with-eval-after-load 'magit
  (keymap-set magit-mode-map "R" #'ysc/magit-review)
  (ignore-errors
    (transient-remove-suffix 'magit-dispatch "R"))
  (transient-insert-suffix 'magit-dispatch "o"
    '("R" "Review files" ysc/magit-review)))

(provide 'magit-review)
;;; magit-review.el ends here
