(require 'markdown-mode)

(setq task-time-format "%Y%m%d %T %z")

(defun task-start()
  (interactive)
  (insert
   (format-time-string
    (format "<task-start t=\"%s\"/>" task-time-format)))
  )

(defun task-stop()
  (interactive)
  (insert
   (format-time-string
    (format "<task-stop t=\"%s\"/>" task-time-format)))
  )

(defun task-config-email()
  (save-excursion
    (end-of-buffer)
    (search-backward-regexp
     "<task-config-email s=\"\\([^\"]*\\)\"/>"
     nil t)
    (match-string-no-properties 1)
    )
  )

(setq toggl-csv-header "User,Email,Client,Project,Task,Description,Billable,Start date,Start time,End date,End time,Duration,Tags,Amount ()")

(defun task-line-starts-with (prefix)
  "Return t if current line in buffer starts with PREFIX."
  (string-equal
   prefix
   (buffer-substring-no-properties
    (line-beginning-position)
    (+ (length prefix) (line-beginning-position)))
   )
  )

(defun task--smart-length (arg)
  (if (stringp arg)
      (length arg)
    (if (integerp arg)
        arg
      (error "task--smart-length argument must be string or integer")
      )
    )
  )

(defun task-line-substring (at)
  "Return substring from AT to the end.
AT may be integer or string. Length of the string is used in the later case."
  (buffer-substring-no-properties
   (+ (task--smart-length at) (line-beginning-position))
   (line-end-position))
  )

(defun task-line()
  "Return current line in the current buffer."
  (task-line-substring 0)
  )

(defun task-assert-date()
  "Fail unless the `point' is at \"date\" markdown header."
  (unless (markdown-heading-at-point)
    (error "Date must be at heading"))
  (unless (eq 1 (markdown-outline-level))
    (error "Date must be first level heading"))
  )

(defun task-get-date()
  "Return a date the `point' is at."
  (task-assert-date)
  (if (task-line-starts-with "# ")
      (task-line-substring "# ")
    (task-line)
    )
  )

(defun task-assert-project()
  "Fail unless the `point' is at \"project\" markdown header."
  (unless (markdown-heading-at-point)
    (error "Project must be at heading"))
  (unless (eq 2 (markdown-outline-level))
    (error "Project must be second level heading"))
  )

(defun task-get-project()
  "Return a project the `point' is at."
  (task-assert-project)
  (if (task-line-starts-with "## ")
      (task-line-substring "## ")
    (task-line)
    )
  )

(defun task-assert-task()
  "Fail unless the `point' is at \"task\" markdown header."
  (unless (markdown-heading-at-point)
    (error "Task must be at heading"))
  (unless (eq 3 (markdown-outline-level))
    (error "Task must be third level heading"))
  )

(defun task-get-task()
  "Return a task the `point' is at."
  (task-assert-task)
  (task-line-substring "### ")
  )

(defun task-next-from-project(date)
  "Return the next task given the `point' is at a project.
Use current project and DATE to construct the task list.
Third element of the task list, representing the task itself,
is nil if project stumbles into another project or new date."
  (let ((project (task-get-project)))
    (markdown-next-heading)
    (if (eq 3 (markdown-outline-level))
        (list date project (task-get-task))
      (list date project nil)
      )
    )
  )

(defun task-next-from-date()
  "Return the next task given the `point' is at a date.
Use current project and DATE to construct the task list.
Skip to the next date if current date has no projects."
  (if (eobp)
      nil
    (let ((date (task-get-date)))
      (markdown-next-heading)
      (if (eq 1 (markdown-outline-level))
          (task-next-from-date)
        (task-next-from-project date)
        )
      )
    )
  )

(defun task-from-heading(date project)
  "Return the task regardless of that header the `point' is at.
Skip according to the `task-next-from-date' if at a date,
`task-next-from-project' if at a project and
return a task if it's what the `point' is at.
Search for hext task otherwise.
"
  (let ((level (markdown-outline-level)))
    (cond
     ((eq 1 level) (task-next-from-date))
     ((eq 2 level) (task-next-from-project date))
     ((eq 3 level) (list date project (task-get-task)))
     (t (progn
          (task-next-from-heading date project)))
     )
    )
  )

(defun task-next-from-heading(date project)
  "Move to the next heading and return `task-from-heading'."
  (markdown-next-heading)
  (if (eobp)
      nil
    (task-from-heading date project)
    )
  )

(defun task-first-in-buffer()
  "Return the first task in the current buffer."
  (beginning-of-buffer)
  (if (not (markdown-heading-at-point))
      (markdown-next-heading)
    )
  (task-from-heading nil nil)
  )

(defun task-next (task)
  "Return the next task in the current buffer."
  (task-next-from-heading
   (nth 0 task)
   (nth 1 task))
  )

(define-minor-mode task-tags-mode
  "Task & time tracking in Markdown document with tags"
  :lighter " ttags"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-o t") 'task-start)
            (define-key map (kbd "M-o h") 'task-stop)
            map)
  )

(add-hook 'markdown-mode-hook 'task-tags-mode)

(provide 'task-tags-mode)
