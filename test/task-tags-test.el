(require 'task-tags-mode)

(ert-deftest task-test-first-in-buffer ()
  (save-excursion
    (find-file "test.md")
    (switch-to-buffer "test.md")
    (should
     (equal (buffer-name) "test.md"))
    (markdown-syntax-propertize (buffer-end -1) (buffer-end 1))
    (let ((test (task-first-in-buffer)))
      (should
       (equal test '("2018-May-03" "Project A" "Task A1")))
      (let ((test1 (task-next test)))
        (should
         (equal test1 '("2018-May-03" "Project A" "Task A2")))
        (let ((test2 (task-next test1)))
          (should
           (equal test2 '("2018-May-03" "Project B" "Task B2")))
          (let ((test3 (task-next test2)))
            (should
             (equal test3 '("2018-May-03" "Project B" "Task B3")))
            (let ((test4 (task-next test3)))
              (should
               (equal test4 '("2018-May-06" "Project A" "Task A2")))
              (let ((test5 (task-next test4)))
                (should
                 (equal test5 '("2018-May-06" "Project A" "Task A3")))
                (let ((test6 (task-next test5)))
                  (should
                   (equal test6 nil))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
