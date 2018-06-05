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

(defun task-stream--car (stream)
  (cdr (assoc 'task stream)))

(defun task-stream--cdr (stream)
  (task-stream-from-task
   (task-next
    (task-stream--car stream)))
  )

(defun task-stream-from-task (task)
  "Creates a stream of tasks starting at the given TASK.
`task-next' is used to build the stream."
  (if (eq nil task)
      nil
    (list
     (cons 'car 'task-stream--car)
     (cons 'cdr 'task-stream--cdr)
     (cons 'task task))
    )
  )

(defun task-stream-from-first-in-buffer ()
  "Creates a stream of tasks starting from the first task in the buffer.
It uses `task-first-in-buffer' to find the first task and
`task-stream-from-task' to build the stream."
  (task-stream-from-task (task-first-in-buffer))
  )

(defun task-time-tag--next-regex ()
  (when
      (search-forward-regexp
       "<task-\\(start\\|stop\\) +t=\"\\([^\"]+\\)\"/>" nil t)
    (list
     (equal "start" (match-string-no-properties 1))
     (match-string-no-properties 2))
    )
  )

(defun task-time-tag-from-task (task)
  "Return time tag and context starting at a TASK.
TASK is a list. Result conforms to `task-time-tag-and-ctxp'.
The method creates time tag context from the TASK and
returns result of the `task-time-tag-next' invocation."
  (when task
    (let ((p (point))
          (next-task (task-next task))
          (next-task-pos (point)))
      (goto-char p)
      (if next-task
          (task-time-tag-next (list task next-task next-task-pos))
        (task-time-tag-next (list task nil (buffer-end 1)))
        )
      )
    )
  )

(defun task-time-tag-ctxp (OBJECT)
  "Check if OBJECT is a time tag context.
It is usable by `task-time-tag-next' function to find next time tag.
Which is a list with

1. The first element being a list representing a current task,
2. The second element being a list representing the next task, and
3. The third argument being an integer representing position of
   the next task."
  (and
   (listp (nth 0 time-tag-ctx))
   (listp (nth 1 time-tag-ctx))
   (integerp (nth 2 time-tag-ctx)))
  )

(defun task-time-tag-assert-ctx (object)
  "Error if OBJECT doesn't conform with `task-time-tag-ctxp'."
  (unless (task-time-tag-ctxp object)
    (error "Not task-time-tag-ctxp")
    )
  )

(defun task-time-tagp (OBJECT)
  "Check if OBJECT is a time tag.
Which is a list with

1. The first element being t for start tag and nil for stop tag, and
2. The second element being a timestamp (a string currently)."
  (and
   (listp OBJECT)
   (booleanp (nth 0 OBJECT))
   (stringp (nth 1 OBJECT)))
  )

(defun task-time-tag-and-ctxp (OBJECT)
  "Check if OBJECT is a time tag and context.
Which is a list with

1. The first element conforming to `task-time-tagp', and
2. The rest of the list conforming to `task-time-tag-ctxp'."
  )

(defun task-time-tag-next (time-tag-ctx)
    "Given TIME-TAG-CTX return the next time tag and context.
TIME-TAG-CTX conforms to `task-time-tag-ctxp'.
Result conforms to `task-time-tag-and-ctxp'."
  (task-time-tag-assert-ctx time-tag-ctx)
  (let ((next-task (nth 1 time-tag-ctx))
        (next-task-pos (nth 2 time-tag-ctx)))
    (let ((time-tag (task-time-tag--next-regex)))
      (when time-tag
        (if (< (point) next-task-pos)
            (cons time-tag time-tag-ctx)
          (goto-char next-task-pos)
          (task-time-tag-from-task next-task)
          )
        )
      )
    )
  )

(defun task-time-tag-first-in-buffer ()
  "Return first time tag in the current buffer."
  (task-time-tag-from-task (task-first-in-buffer))
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
