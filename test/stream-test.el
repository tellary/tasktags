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
