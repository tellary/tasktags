(defun streamp (object)
  "Return t if OBJECT is a stream.
Which means it has CAR, it has CDR and they both are functions."
  (and
   (assoc 'car object)
   (functionp (cdr (assoc 'car object)))
   (assoc 'cdr object)
   (functionp (cdr (assoc 'cdr object)))
   t)
  )

(defun stream-car (stream)
  "Returns head of the STREAM.
It's analogous to `car' on regular `list'"
  (funcall (cdr (assoc 'car stream)) stream)
  )

(defun stream-cdr (stream)
  "Returns tail of the STREAM.
It's analogous to `cdr' on regular `list'"
  (funcall (cdr (assoc 'cdr stream)) stream)
  )
    
(defun stream--list-stream-list (stream)
  (cdr (assoc 'list stream)))

(defun stream-from-list (l)
  "Constructs a stream from L.
Returned result conforms to `stream-listp'."
  (if (eq nil l)
      nil
    (list
     (cons
      'car
      (lambda (stream)
        (car (stream--list-stream-list stream))))
     (cons
      'cdr
      (lambda (stream)
        (stream-from-list
         (cdr
          (stream--list-stream-list stream)))))
     (cons 'list l)
     )
    )
  )

(defun stream-listp (object)
  "Checks if OBJECT was constructed by `stream-from-list'.
It means the OBJECT is a stream (see `streamp') and
it has LIST property which is `list'."
  (and
   (streamp object)
   (assoc 'list object)
   (listp (cdr (assoc 'list object)))
   )
  )

(defun stream-foldl (fn left stream)
  "Folds a stream STREAM left to right.
See `streamp' for definitian of stream."
  (if (eq nil stream)
      left
    (let ((item (stream-car stream)))
      (let ((result (funcall fn left item)))
        (stream-foldl
         fn
         result
         (stream-cdr stream))
        )
      )
    )
  )

(defun stream-foldr (fn right stream)
  "Folds a stream STREAM right to left.
See `streamp' for definitian of stream."
  (if (eq nil stream)
      right
    (let ((item (stream-car stream)))
      (funcall
       fn
       item
       (stream-foldr fn right (stream-cdr stream)))
      )
    )
  )

(defun stream-to-list (stream)
  "Collects stream into list.
See `streamp' for definitian of stream."
  (stream-foldr 'cons () stream)
  )


(provide 'stream)
