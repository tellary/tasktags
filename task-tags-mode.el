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

(setq task-time-tag--regex "<task-\\(start\\|stop\\) +t=\"\\([^\"]+\\)\"/>")

(defun task-time-tag--next ()
  (when
      (search-forward-regexp task-time-tag--regex  nil t)
    (match-string-no-properties 2)
    )
  )

(defun task-time-tag--prev ()
  (when
      (search-backward-regexp task-time-tag--regex  nil t)
    (match-string-no-properties 2)
    )
  )

(defun task-toggl-csv (filename)
  "Creates Toggl CSV report.
Use region if selected, or report from the current position until end of
the current buffer."
  (interactive
   (list
    (read-string "Output file: "
                 (format "%s.csv" (file-name-base)))))
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
         (firstTag
          (save-excursion
            (goto-char begin)
            (task-time-tag--next))
          )
         (cmdStartPos
          (format "togglCsv --startPos %s" (- startPos 1))
          )
         (cmdFirstTag
          (if firstTag
              (format "%s --firstTag \"%s\"" cmdStartPos firstTag)
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
               (lastTag
                (save-excursion
                  (goto-char end)
                  (task-time-tag--prev)
                  )
                )
               (cmdEndPos
                (format "%s --endPos %s" cmdFirstTag (- endPos 1))
                )
               (cmdLastTag
                (if lastTag
                    (format "%s --lastTag \"%s\"" cmdEndPos lastTag)
                  cmdEndPos
                  )
                )
               (cmd
                (concat cmdLastTag " " (buffer-file-name) " " filename))
               )
          (message cmd)
          (shell-command cmd)
          )
      (let ((cmd (concat
                  cmdFirstTag
                  " --ignoreIncompleteLastStartTag "
                  (buffer-file-name) " " filename)))
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
