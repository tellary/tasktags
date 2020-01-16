(require 'ert)
(require 'task-tags-mode)

(defun task-find-md-file (file)
  (find-file file)
  (switch-to-buffer file)
  (should
   (equal (buffer-name) file))
  (markdown-syntax-propertize (buffer-end -1) (buffer-end 1))
  )

(defun task-test-md ()
  (task-find-md-file "test.md")
  )

(defun task-test-two-stops-md ()
  (task-find-md-file "test_two_stops.md")
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
                ; More tasks to follow
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
        ("2018-May-06" "Project A" "Task A3")
        ("2018-May-06" "Project B" "Task B2"))
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
    (task-test-two-stops-md)
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

(ert-deftest task-time-tag-stream-test-first-in-buffer ()
  (save-excursion
    (task-test-two-stops-md)
    (should
     (equal
      (stream-to-list (task-time-tag-stream-from-first-in-buffer))
      '(((  t "20180506 12:20:54 -0700")
         ("2018-May-06" "Project A" "Task A2"))
        ((nil "20180506 12:31:51 -0700")
         ("2018-May-06" "Project A" "Task A2"))
        ((  t "20180506 12:25:50 -0700")
         ("2018-May-06" "Project A" "Task A2"))
        ((nil "20180506 12:41:18 -0700")
         ("2018-May-06" "Project A" "Task A2"))
        ((  t "20180506 09:00:02 -0700")
         ("2018-May-06" "Project A" "Task A3"))
        ((nil "20180506 11:05:00 -0700")
         ("2018-May-06" "Project A" "Task A3")))
      )
     )
    )
  )

(ert-deftest task-time-tag-stream-test-from-task-at-pos ()
  (task-test-md)
  (should
   (equal
    (stream-to-list (task-time-tag-stream-from-task-at-pos 410))
    '(((t   "20180506 09:00:02 -0700")
       ("2018-May-06" "Project A" "Task A3"))
      ((nil "20180506 11:05:00 -0700")
       ("2018-May-06" "Project A" "Task A3"))
      ((t   "20180506 13:41:02 -0700")
       ("2018-May-06" "Project B" "Task B2"))
      ((nil "20180506 14:05:18 -0700")
       ("2018-May-06" "Project B" "Task B2")))
    )
   )
  )

(ert-deftest task-time-tag-stream-test-from-task-between-pos ()
  (task-test-md)
  (should
   (equal
    (stream-to-list (task-time-tag-stream-from-task-at-pos 272 410))
    '(((t   "20180506 12:31:51 -0700")
       ("2018-May-06" "Project A" "Task A2"))
      ((t   "20180506 12:20:54 -0700")
       ("2018-May-06" "Project A" "Task A2"))
      ((nil "20180506 12:25:50 -0700")
       ("2018-May-06" "Project A" "Task A2"))
      ((nil "20180506 12:41:18 -0700")
       ("2018-May-06" "Project A" "Task A2"))
      ((t "20180506 09:00:02 -0700")
       ("2018-May-06" "Project A" "Task A3")))
    )
   )
  )

(ert-deftest task-time-tag-and-task-test-time-lessp ()
  (let (
        (tt1
         '((t "20180506 12:20:54 -0700")
           ("2018-May-06" "Project A" "Task A2")))
        (tt2
         '((nil "20180506 12:31:51 -0700")
           ("2018-May-06" "Project A" "Task A2"))))
    (should
     (eq (task-time-tag-and-task-time-lessp tt1 tt2) t))
    (should
     (eq (task-time-tag-and-task-time-lessp tt2 tt1) nil))
    )
  )

(ert-deftest task-time-entry-test ()
  (should
   (equal
    (task-time-entry
     '((t "20180506 12:20:54 -0700")
       ("2018-May-06" "Project A" "Task A2"))
     '((nil "20180506 12:25:50 -0700")
       ("2018-May-06" "Project A" "Task A2")))
    '("20180506 12:20:54 -0700" "20180506 12:25:50 -0700" "Project A" "Task A2")
    )
   )
  )

(ert-deftest task-time-entry-test-from-tag-stream ()
  (save-excursion
    (task-test-md)
    (let ((entries-list
           (task-time-entries-from-tag-stream
            (task-time-tag-stream-from-first-in-buffer))))
      (should
       (equal
        entries-list
        '(
          ("20180506 09:00:02 -0700"
           "20180506 11:05:00 -0700"
           "Project A" "Task A3")
          ("20180506 12:20:54 -0700"
           "20180506 12:25:50 -0700"
           "Project A" "Task A2")
          ("20180506 12:31:51 -0700"
           "20180506 12:41:18 -0700"
           "Project A" "Task A2")
          ("20180506 13:41:02 -0700"
           "20180506 14:05:18 -0700"
           "Project B" "Task B2"))
        )
       )
      )
    )
  )

(ert-deftest task-timestamp-test-toggl-date ()
  (should
   (equal
    (task-timestamp-toggl-date "20180506 09:00:02 -0700")
    "2018-05-06"
    )))

(ert-deftest task-timestamp-test-toggl-time ()
  (should
   (equal
    (task-timestamp-toggl-time "20180506 09:00:02 -0700")
    "09:00:02"
    )))

(ert-deftest task-time-entry-test-toggl-csv-line ()
  (should
   (equal
    (task-time-entry--toggl-csv-line
     "name@example.com"
     '("20180506 12:31:51 -0700"
       "20180506 14:31:54 -0700"
       "Project A" "Task A2"))
    "name@example.com,name@example.com,,\"Project A\",,\"Task A2\",No,2018-05-06,12:31:51,2018-05-06,14:31:54,02:00:03,,"
    )
   )
  )

(ert-deftest task-time-entries-test-toggl-csv ()
  (task-test-md)
  (should
   (equal
    (task-time-entries-toggl-csv (task-time-tag-stream-from-first-in-buffer))
    "User,Email,Client,Project,Task,Description,Billable,Start date,Start time,End date,End time,Duration,Tags,Amount ()
name@example.com,name@example.com,,\"Project A\",,\"Task A3\",No,2018-05-06,09:00:02,2018-05-06,11:05:00,02:04:58,,
name@example.com,name@example.com,,\"Project A\",,\"Task A2\",No,2018-05-06,12:20:54,2018-05-06,12:25:50,00:04:56,,
name@example.com,name@example.com,,\"Project A\",,\"Task A2\",No,2018-05-06,12:31:51,2018-05-06,12:41:18,00:09:27,,
name@example.com,name@example.com,,\"Project B\",,\"Task B2\",No,2018-05-06,13:41:02,2018-05-06,14:05:18,00:24:16,,"
    )
   )
  )

(ert-deftest task-test-from-position ()
  (task-test-md)
  (should
   (equal
    (task-from-position 399)
    '("2018-May-06" "Project A" "Task A3")))
  )

(ert-deftest task-test-from-position-first-in-proj ()
  (task-test-md)
  (should
   (equal
    (task-from-position 272)
    '("2018-May-06" "Project A" "Task A2")))
  )

(ert-deftest task-test-from-position-after-date ()
  (task-test-md)
  (should
   (equal
    (task-from-position 124)
    '("2018-May-06" "Project A" "Task A2")))
  )

(ert-deftest task-test-from-position-after-project-first-in-date ()
  (task-test-md)
  (should
   (equal
    (task-from-position 174)
    '("2018-May-06" "Project A" "Task A2")))
  )

(ert-deftest task-test-from-position-after-project ()
  (task-test-md)
  (should
   (equal
    (task-from-position 459)
    '("2018-May-06" "Project B" "Task B2")))
  )

(ert-deftest task-test-from-position-last-task ()
  (task-test-md)
  (should
   (equal
    (task-from-position 476)
    '("2018-May-06" "Project B" "Task B2")))
  )
