(setq task-time-format "%Y%m%d %T %z")

(defun task-start()
  (interactive)
  (insert
   (format-time-string
    (format "(task-start: %s)" task-time-format)))
  )

(defun task-stop()
  (interactive)
  (insert
   (format-time-string
    (format "(task-stop: %s)" task-time-format)))
  )
