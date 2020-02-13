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

(defun task--find-prev-date ()
  (unless (markdown-heading-at-point)
    (markdown-previous-heading))
  (let ((start-pos (point))
        (level (markdown-outline-level)))
    (when (and (bobp) (not (eq 1 level)))
      (error "No date found above position %s" start-pos))
    (unless
        (eq 1 level)
      (markdown-previous-heading)
      (task--find-prev-date)
      )
    )
  )

(defun task--find-next-date()
  (unless (markdown-heading-at-point)
    (markdown-previous-heading))
  (let ((level (markdown-outline-level)))
    (unless (eq 1 level)
      (markdown-next-heading)
      (unless (eobp)
        (task--find-next-date)
        )
      )
    )
  )

(defun task-time-tag--previous-start ()
  (when
      (search-backward-regexp
       "<task-start +t=\"\\([^\"]+\\)\"/>" nil t)
    (match-string-no-properties 1)
    )
  )

(defun task-time-tag--next-start ()
  (when
      (search-forward-regexp
       "<task-start +t=\"\\([^\"]+\\)\"/>" nil t)
    (match-string-no-properties 1)
    )
  )

(defun task-toggl-csv (filename)
  "Creates Toggl CSV report.
Use region if selected, or report from the current position until end of
the current buffer."
  (interactive (list (read-string "Output file: " "toggl.csv")))
  (save-buffer)
  (let* ((begin (region-beginning))
         (end   (region-end))
         (startPos
          (save-excursion
            (goto-char begin)
            (task--find-prev-date)
            (point)
            )
          )
         (timeGE
          (save-excursion
            (goto-char begin)
            (task-time-tag--next-start))
          )
         (cmdStartPos
          (format "togglCsv --startPos %s" (- startPos 1))
          )
         (cmdTimeGE
          (if timeGE
              (format "%s --startTimeP \">= %s\"" cmdStartPos timeGE)
            cmdStartPos)
          )
         )
    (if (use-region-p)
        (let* ((endPos
                (save-excursion
                  (goto-char end)
                  (task--find-next-date)
                  (point)
                  )
                )
               (timeLE
                (save-excursion
                  (goto-char end)
                  (task-time-tag--previous-start)
                  )
                )
               (cmdEndPos
                (format "%s --endPos %s" cmdTimeGE (- endPos 1))
                )
               (cmdTimeLE
                (if timeLE
                    (format "%s --startTimeP \"<= %s\"" cmdEndPos timeLE)
                  cmdEndPos
                  )
                )
               (cmd
                (concat cmdTimeLE " " (buffer-file-name) " " filename))
               )
          (message cmd)
          (shell-command cmd)
          )
      (let ((cmd (concat cmdTimeGE " " (buffer-file-name) " " filename)))
        (message cmd)
        (shell-command cmd)
        )
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
