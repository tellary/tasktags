(require 'stream)

(ert-deftest stream-test-from-list ()
  (let ((s (stream-from-list '(1 2 3))))
    (should (= (stream-car s) 1))
    (let ((s1 (stream-cdr s)))
      (should (= (stream-car s1) 2))
      (let ((s2 (stream-cdr s1)))
        (should (= (stream-car s2) 3))
        )
      )
    )
  )

(ert-deftest stream-test-foldl-sum ()
  (should
   (=
    (stream-foldl '+ 0 (stream-from-list '(1 2 3)))
    6)
   )
  )

(ert-deftest stream-test-foldl-list ()
  (should
   (equal
    (stream-foldl 'list 0 (stream-from-list '(1 2 3)))
    '(((0 1) 2) 3))
   )
  )

(ert-deftest stream-test-foldl-collect ()
  (should
   (equal
    (stream-foldl
     (lambda (l e)
       (cons e l))
     () (stream-from-list '(1 2 3)))
    '(3 2 1))
   )
  )

(ert-deftest stream-from-list-is-stream()
  (let ((s (stream-from-list '(1 2 3))))
    (should (equal (streamp s) t))
    )
  )

(ert-deftest stream-from-list-is-stream-from-list()
  (let ((s (stream-from-list '(1 2 3))))
    (should (equal (stream-listp s) t))
    )
  )

(ert-deftest stream-test-foldr-sum ()
  (should
   (=
    (stream-foldr '+ 0 (stream-from-list '(1 2 3)))
    6)
   )
  )

(ert-deftest stream-test-foldr-list ()
  (should
   (equal
    (stream-foldr 'list 0 (stream-from-list '(1 2 3)))
    '(1 (2 (3 0))))
   )
  )

(ert-deftest stream-test-foldr-collect ()
  (should
   (equal
    (stream-foldr
     'cons
     () (stream-from-list '(1 2 3)))
    '(1 2 3))
   )
  )

(ert-deftest stream-test-from-to-list ()
  (let ((stream (stream-from-list '(1 2 3))))
    (should
     (equal (stream-to-list stream) '(1 2 3)))
    )
  )

(ert-deftest stream-test-drop-while ()
  (let ((stream (stream-from-list '(1 3 4 7 5))))
    (should
     (equal
      (stream-to-list
       (stream-drop-while 'oddp stream))
      '(4 7 5))
     )
    )
  )

(ert-deftest stream-test-drop-while-all ()
  (let ((stream (stream-from-list '(1 3 9 7 5))))
    (should
     (equal
      (stream-to-list
       (stream-drop-while 'oddp stream))
      '())
     )
    )
  )

(ert-deftest stream-test-filter ()
  (let ((stream (stream-from-list '(1 2 3 4 5))))
    (should
     (equal
      (stream-to-list
       (stream-filter 'oddp stream))
      '(1 3 5))
     )
    )
  )

(ert-deftest stream-test-filter-last-no-match ()
  (let ((stream (stream-from-list '(1 2 3 4 5))))
    (should
     (equal
      (stream-to-list
       (stream-filter 'evenp stream))
      '(2 4))
     )
    )
  )
