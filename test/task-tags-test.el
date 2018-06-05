(require 'task-tags-mode)

(defun task-test-md ()
  (find-file "test.md")
  (switch-to-buffer "test.md")
  (should
   (equal (buffer-name) "test.md"))
  (markdown-syntax-propertize (buffer-end -1) (buffer-end 1))
  )

(ert-deftest task-test-first-in-buffer ()
  (save-excursion
    (task-test-md)
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

(ert-deftest task-stream-test-collect ()
  (save-excursion
    (task-test-md)
    (should
     (equal
      (stream-to-list (task-stream-from-first-in-buffer))
      '(
        ("2018-May-03" "Project A" "Task A1")
        ("2018-May-03" "Project A" "Task A2")
        ("2018-May-03" "Project B" "Task B2")
        ("2018-May-03" "Project B" "Task B3")
        ("2018-May-06" "Project A" "Task A2")
        ("2018-May-06" "Project A" "Task A3"))
      )
     )
    )
  )

(defun task-time-tag-should-equal
    (tag expected-task expected-tag)
  (should
   (equal
    (nth 1 tag)
    expected-task))
  (should
   (equal
    (car tag)
    expected-tag))
  )

(ert-deftest task-time-tag-test-first-in-buffer ()
  (save-excursion
    (task-test-md)
    (let ((t1 (task-time-tag-first-in-buffer)))
      (task-time-tag-should-equal
       t1
       '("2018-May-06" "Project A" "Task A2")
       '(t "20180506 12:20:54 -0700"))
      (let ((t2 (task-time-tag-next (cdr t1))))
        (task-time-tag-should-equal
         t2
         '("2018-May-06" "Project A" "Task A2")
         '(nil "20180506 12:31:51 -0700"))
        (let ((t3 (task-time-tag-next (cdr t2))))
          (task-time-tag-should-equal
           t3
           '("2018-May-06" "Project A" "Task A2")
           '(t "20180506 12:25:50 -0700"))
          (let ((t4 (task-time-tag-next (cdr t3))))
            (task-time-tag-should-equal
             t4
             '("2018-May-06" "Project A" "Task A2")
             '(nil "20180506 12:41:18 -0700"))
            (let ((t5 (task-time-tag-next (cdr t4))))
              (task-time-tag-should-equal
               t5
               '("2018-May-06" "Project A" "Task A3")
               '(t "20180506 09:00:02 -0700"))
              (let ((t6 (task-time-tag-next (cdr t5))))
                (task-time-tag-should-equal
                 t6
                 '("2018-May-06" "Project A" "Task A3")
                 '(nil "20180506 11:05:00 -0700"))
                (should
                 (equal
                  (task-time-tag-next (cdr t5))
                  nil))
                )
              )
            )
          )
        )
      )
    )
  )
