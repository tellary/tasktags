(require 'markdown-mode)
(require 'stream)
(require 'cl)

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

(setq task-toggl-csv-header "User,Email,Client,Project,Task,Description,Billable,Start date,Start time,End date,End time,Duration,Tags,Amount ()")

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

(defun task-from-position (pos)
  "Goes to POS and invokes `task-from-current-position'
from there."
  (goto-char pos)
  (task-from-current-position)
  )

(defun task-from-current-position--find-date (start-pos)
  (let ((level (markdown-outline-level)))
    (when (and (bobp) (not (eq 1 level)))
      (error "No date found above position %s" start-pos))
    (unless
        (eq 1 level)
      (markdown-previous-heading)
      (task-from-current-position--find-date start-pos)
      )
    )
  )

(defun task-from-current-position--until-pos (task pos)
  (let ((current-task-pos (point))
        (next-task (task-next task)))
    (if (> (point) pos)
        (list
         (cons current-task-pos task)
         (cons (point) next-task))
      (task-from-current-position--until-pos next-task pos)
      )
    )
  )

(defun task-from-current-position ()
  "Finds task at the current position.
Finds the closest date, project and task headers above
the current position navigates to the task header found
and returns a task constructed out of the three.
Finds next task down the current position if the closest
header above is project or date."
  (unless (markdown-heading-at-point)
    (markdown-previous-heading))
  (let ((start-pos (point))
        (level (markdown-outline-level)))
    (task-from-current-position--find-date start-pos)
    (let ((first-task (task-from-heading nil nil)))
      (if (> (point) start-pos)
          first-task
        (let* ((current-and-next-pos-and-task
               (task-from-current-position--until-pos
                first-task start-pos))
               (current-pos-and-task
                (nth 0 current-and-next-pos-and-task))
               (next-pos-and-task
                (nth 1 current-and-next-pos-and-task))
               (current-pos (car current-pos-and-task))
               (current-task (cdr current-pos-and-task))
               (next-pos (car next-pos-and-task))
               (next-task (cdr next-pos-and-task)))

          (if (eq 2 level) ;; project
              (progn
                (goto-char next-pos)
                next-task
                )
            (goto-char current-pos)
            current-task
            )
          )
        )
      )
    )
  )

(defun task-stream--car (stream)
  (cdr (assq 'task stream)))

(defun task-stream--cdr (stream)
  (task-stream-from-task
   (task-next
    (task-stream--car stream)))
  )

(defun task-stream-from-task (task)
  "Create a stream of tasks starting at the given TASK.
`task-next' is used to build the stream."
  (if (eq nil task)
      stream-nil
    (list
     (cons 'car 'task-stream--car)
     (cons 'cdr 'task-stream--cdr)
     (cons 'null 'stream-false)
     (cons 'task task))
    )
  )

(defun task-stream-from-first-in-buffer ()
  "Create a stream of tasks starting from the first task in the buffer.
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
   (listp (nth 0 OBJECT))
   (listp (nth 1 OBJECT))
   (integerp (nth 2 OBJECT)))
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
  (and
   (listp OBJECT)
   (task-time-tagp (car OBJECT))
   (task-time-tag-ctxp (cdr OBJECT)))
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

(defun task-time-tag-stream--car (stream)
  (subseq 
   (cdr (assq 'tag-and-ctx stream))
   0 2)
  )

(defun task-time-tag-stream--tag-ctx (stream)
  (cdr (cdr (assq 'tag-and-ctx stream)))
  )

(defun task-time-tag-stream--end-pos (stream)
  (cdr (assq 'end-pos stream))
  )

(defun task-time-tag-stream--cdr (stream)
  (let* ((current-tag-ctx
          (task-time-tag-stream--tag-ctx stream))
         (end-pos (task-time-tag-stream--end-pos stream))
         (next-tag-and-ctx
          (task-time-tag-next current-tag-ctx)))
    (if (<= (point) end-pos)
        (task-time-tag-stream next-tag-and-ctx end-pos)
      stream-nil
      )
    )
  )

(defun task-time-tag-stream (tag-and-ctx &optional end-pos)
  "Create a stream of time tags starting from a TAG-AND-CTX.
`task-time-tag-next' is used to build the stream.
TAG-AND-CTX conforms to `task-time-tag-and-ctxp'.
END-POS limits the stream. No tag will appear in the
resulting stream whose position is more than END-POS.
END-POS is end of buffer if not provided."
  (if (eq nil tag-and-ctx)
      stream-nil
    (unless (task-time-tag-and-ctxp tag-and-ctx)
      (error "tag-and-ctx must be task-time-tag-and-ctxp"))
    (list
     (cons 'car 'task-time-tag-stream--car)
     (cons 'cdr 'task-time-tag-stream--cdr)
     (cons 'null 'stream-false)
     (cons 'tag-and-ctx tag-and-ctx)
     (cons 'end-pos (if end-pos end-pos (point-max)))
     )
    )
  )

(defun task-time-tag-stream-from-first-in-buffer()
  "Create a stream of time tags starting from the first tag in the buffer.
It uses `task-time-tag-first-in-buffer' to find the first tag.
It uses `task-time-tag-stream' to build the stream."
  (task-time-tag-stream (task-time-tag-first-in-buffer))
  )

(defun task-time-tag-stream-from-task-at-pos (pos &optional end-pos)
  "Create a stream of time tags starting at a task at POS,
having no tags after END-POS.

It uses `task-from-position' to find the first task
and `task-time-tag-from-task' to find first tag in stream.
It essentially means that all tags of the task at POS
are included.

No tag will appear in the
resulting stream whose position is more than END-POS.
END-POS is end of buffer if not provided."
  (task-time-tag-stream
   (task-time-tag-from-task
    (task-from-position pos))
   end-pos)
  )

(defun task-time-tag-and-taskp (OBJECT)
  "Return t if OBJECT is tag-and-task"
  (and
   (listp OBJECT)
   (task-time-tagp (nth 0 OBJECT))
   (listp (nth 1 OBJECT)))
  )

(defun task-time-tag-and-task-time-lessp (tt1 tt2)
  "Given two task time-tag-and-task's compare them by time."
  (assert (task-time-tag-and-taskp tt1))
  (assert (task-time-tag-and-taskp tt2))
  (let ((t1 (car tt1))
        (t2 (car tt2)))
    (string-lessp (cadr t1) (cadr t2))
    )
  )

(defun task-time-tag-and-task-is-stop (tag)
  "Given a time-tag-and-task return t if it's a stop task."
  (assert (task-time-tag-and-taskp tag))
  (eq (caar tag) nil)
  )

(defun task-time-tag-and-task-is-start (tag)
  "Given a time-tag-and-task return t if it's a start task."
  (assert (task-time-tag-and-taskp tag))
  (eq (caar tag) t)
  )

(defun task-time-tag-and-task-time (tag)
  "Given a time-tag-and-task return it's timestamp."
  (assert (task-time-tag-and-taskp tag))
  (nth 1 (car tag))
  )

(defun task-time-tag-and-task-task (tag)
  "Given a time-tag-and-task return the task."
  (assert (task-time-tag-and-taskp tag))
  (cadr tag)
  )
  
(defun task-time-tag-and-task-task-no-date (tag)
  "Given a time-tag-and-task return the task without the first element.
The first element encodes date of the task."
  (assert (task-time-tag-and-taskp tag))
  (cdr (task-time-tag-and-task-task tag))
  )

(defun task-time-entry (tag next-tag)
  "Construct a time entry from start and stop tag-and-task's.
Fail unless it's start and stop tags or they have different tasks."
  (assert (task-time-tag-and-taskp tag))
  (assert (task-time-tag-and-taskp next-tag))
  (unless (task-time-tag-and-task-is-stop next-tag)
    (error "Next tag is 'start': %s %s" tag next-tag))
  (when (task-time-tag-and-task-is-stop tag)
    (error "Two stop tags: %s %s" tag next-tag))

  (let ((task (task-time-tag-and-task-task-no-date tag))
        (next-task (task-time-tag-and-task-task-no-date next-tag)))
    (unless (equal task next-task)
      (error "Different tasks in tags: %s %s" tag next-tag))
    (cons
     (task-time-tag-and-task-time tag)
     (cons
      (task-time-tag-and-task-time next-tag)
      task))
    )
  )

(defun task-time-tag--to-entry-reduce-fn (tag tag-and-result)
  (if (not tag-and-result)
      (list tag)
    (let ((next-tag (car tag-and-result))
          (result (cdr tag-and-result)))
      (if (task-time-tag-and-task-is-start tag)
          (if (task-time-tag-and-task-is-start next-tag)
              (error "Two start tags: %s %s" tag next-tag)
            (cons
             tag
             (cons
              (task-time-entry tag next-tag)
              result))
            )
        (if (task-time-tag-and-task-is-stop next-tag)
            (error "Two stop tags: %s %s" tag next-tag)
          (cons tag result)
          )
        )
      )
    )
  )

(defun task-time-entryp (OBJECT)
  "Returns t if OBJECT is a time-entry.
It means 1) it's a list 2) it has start time on index 0,
3) it has stop time at index 1, 4) it has project at index 2,
and 5) it has task at index 3."

  (and
   (listp OBJECT)
   (not (eq nil (nth 0 OBJECT))) ;; Start time
   (not (eq nil (nth 1 OBJECT))) ;; Stop time
   (stringp (nth 2 OBJECT)) ;; Project
   (stringp (nth 3 OBJECT)) ;; Task
   )
  )

(defun task-time-entries-from-tag-stream (stream)
  "Returns list of time-entries from a STREAM of tags."
  (let ((sorted-tags
         (sort
          (stream-to-list stream)
          'task-time-tag-and-task-time-lessp)))
    (cdr
     (cl-reduce
      'task-time-tag--to-entry-reduce-fn
      sorted-tags
      :from-end t
      :initial-value nil)
     )
    )
  )

(defun task-time-entry--project (entry)
  (nth 2 entry)
  )

(defun task-time-entry--task (entry)
  (nth 3 entry)
  )

(defun task-time-entry--start (entry)
  (nth 0 entry)
  )

(defun task-time-entry--stop (entry)
  (nth 1 entry)
  )

(defun task-time-entry-duration (entry)
  "Return duration of the ENTRY.
ENTRY conforms to `task-time-entryp'."
  (assert (task-time-entryp entry))
  (let ((tstart (date-to-time (task-time-entry--start entry)))
        (tstop  (date-to-time (task-time-entry--stop  entry))))
    (format-time-string
     "%H:%M:%S"
     (time-subtract tstop tstart) t)
    )
  )

(defun task-timestamp-as-utc (timestamp)
  "Return calendar value of TIMESTAMP as it's in UTC zone.
Or, get clock time and replace timezone with UTC."
  (apply
   'encode-time
   (let ((time (decode-time
                (date-to-time timestamp))))
     (setcdr
      (last time) '(0))
     time
     )
   )
  )

(defun task-timestamp-toggl-date (timestamp)
  "Returns date from the TIMESTAMP string"
  (format-time-string
   "%Y-%m-%d"
   (task-timestamp-as-utc timestamp))
  )

(defun task-timestamp-toggl-time (timestamp)
  "Returns time from the TIMESTAMP string."
  (format-time-string
   "%H:%M:%S"
   (task-timestamp-as-utc timestamp))
  )

(defun task--quote-str (str)
  (concat "\"" str "\""))

(defun task-time-entry--toggl-csv-line (email entry)
  (assert (task-time-entryp entry))
  (mapconcat
   'identity
   (list
    email
    email
    ""
    (task--quote-str
     (task-time-entry--project entry))
    ""
    (task--quote-str
     (task-time-entry--task entry))
    "No"
    (task-timestamp-toggl-date
     (task-time-entry--start entry))
    (task-timestamp-toggl-time
     (task-time-entry--start entry))
    (task-timestamp-toggl-date
     (task-time-entry--stop entry))
    (task-timestamp-toggl-time
     (task-time-entry--stop entry))
    (task-time-entry-duration entry)
    ""
    ""
    )
   ","
   )
  )

(defun task-time-entries-toggl-csv (tag-stream &optional filename)
  "Converts TAG-STREAM into Toggl CSV.
Return CSV string if no FILENAME is provided.
Creates a file buffer for FILENAME and copies
resulting CSV there otherwise."
  (if (not filename)
      (let ((email (task-config-email))
            (entry-list
             (task-time-entries-from-tag-stream tag-stream)))
        (mapconcat
         'identity
         (list
          task-toggl-csv-header
          (mapconcat
           (lambda (entry)
             (task-time-entry--toggl-csv-line email entry))
           entry-list
           "\n")
          )
         "\n"
         )
        )
    (let ((csv (task-time-entries-toggl-csv tag-stream)))
      (message "csv: %s" csv)
      (switch-to-buffer (create-file-buffer filename))
      (insert csv)
      )
    )
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
