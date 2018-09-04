(defun streamp (object)
  "Return t if OBJECT is a stream.
Which means it has CAR, it has CDR and they both are functions."
  (and
   (listp object)
   (assq 'car object)
   (functionp (cdr (assq 'car object)))
   (assq 'cdr object)
   (functionp (cdr (assq 'cdr object)))
   (assq 'null object)
   (functionp (cdr (assq 'null object))))
  )

(defun stream-car (stream)
  "Returns head of the STREAM.
It's analogous to `car' on regular `list'"
  (unless (streamp stream)
    (error "Stream expected in STREAM-CAR"))
  (funcall (cdr (assq 'car stream)) stream)
  )

(defun stream-cdr (stream)
  "Returns tail of the STREAM.
It's analogous to `cdr' on regular `list'"
  (unless (streamp stream)
    (error "Stream expected in STREAM-CDR"))
  (funcall (cdr (assq 'cdr stream)) stream)
  )

(defun stream-true (stream) t)
(defun stream-false (stream) nil)

(setq stream-nil
      (list
       (cons 'car (lambda(x) (error "car: Empty stream")))
       (cons 'cdr (lambda(x) (error "cdr: Empty stream")))
       (cons 'null 'stream-true)
       )
      )

(defun stream-list--list (stream)
  (cdr (assq 'list stream)))

(defun stream-list--car (stream)
  (car (stream-list--list stream)))

(defun stream-list--cdr (stream)
  (stream-from-list
   (cdr
    (stream-list--list stream))))

(defun stream-from-list (l)
  "Constructs a stream from L.
Returned result conforms to `stream-listp'."
  (if (eq nil l)
      stream-nil
    (list
     (cons 'car 'stream-list--car)
     (cons 'cdr 'stream-list--cdr)
     (cons 'null 'stream-false)
     (cons 'list l))
    )
  )

(defun stream-listp (object)
  "Checks if OBJECT was constructed by `stream-from-list'.
It means the OBJECT is a stream (see `streamp') and
it has LIST property which is `list'."
  (and
   (streamp object)
   (assq 'list object)
   (listp (cdr (assq 'list object)))
   )
  )

(defun stream-foldl (fn left stream)
  "Folds a stream STREAM left to right.
See `streamp' for definitian of stream."
  (if (stream-null stream)
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
  (if (stream-null stream)
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

(defun stream-null (stream)
  (assert (streamp stream))
  (funcall (cdr (assq 'null stream)) stream)
  )

(defun stream-drop-while (predicate stream)
  (if (stream-null stream)
      stream-nil
    (if (funcall predicate (stream-car stream))
        (stream-drop-while predicate (stream-cdr stream))
      stream
      )
    )
  )

(defun stream-drop-while-not (predicate stream)
  (if (stream-null stream)
      stream-nil
    (if (not (funcall predicate (stream-car stream)))
        (stream-drop-while-not predicate (stream-cdr stream))
      stream
      )
    )
  )

(defun stream-filter--do-find-next (stream)
  "Finds next value in STREAM if not cached.
It mutates state of the STREAM to cache found value.
It rather has a valid value cached as 'value,
or a null stream under 'original-stream."
  (let ((predicate (cdr (assq 'predicate stream)))
        (original-stream (assq 'stream stream))
        (value (assq 'value stream)))
    (if (eq (cdr value) 'stream-filter--find-next)
      (let ((new-value (stream-car (cdr original-stream))))
        (if (funcall predicate new-value)
            (setf (cdr value) new-value)
          (let ((drop-stream
                 (stream-drop-while-not
                  predicate
                  (cdr original-stream))))
            (if (stream-null drop-stream)
                (progn
                  (setf (cdr original-stream) stream-nil)
                  (setf (cdr value) nil)
                  )
              (setf (cdr original-stream) drop-stream)
              (setf (cdr value) (stream-car drop-stream))
              )
            )
          )
        )
      )
    )
  )

(defun stream-filter--null (stream)
  (stream-filter--do-find-next stream)
  (let ((original-stream (cdr (assq 'stream stream))))
    (stream-null original-stream)
    )
  )

(defun stream-filter--car (stream)
  (stream-filter--do-find-next stream)
  (let ((original-stream (cdr (assq 'stream stream))))
    (if (stream-null original-stream)
        (error "stream-filter--car: empty stream")
      (cdr (assq 'value stream))
      )
    )
  )

(defun stream-filter--cdr (stream)
  ;; This is to advance 'stream to the current element
  ;; if 'value is 'stream-filter--find-next
  (stream-filter--car stream)
  (let ((original-stream (cdr (assq 'stream stream)))
          (predicate (cdr (assq 'predicate stream))))
    (stream-filter predicate (stream-cdr original-stream))
    )
  )

(defun stream-filter (predicate stream)
  (if (stream-null stream)
      stream-nil
    (list
     (cons 'car 'stream-filter--car)
     (cons 'cdr 'stream-filter--cdr)
     (cons 'null 'stream-filter--null)
     (cons 'predicate predicate)
     (cons 'stream stream)
     (cons 'value 'stream-filter--find-next)
     )
    )
  )

(provide 'stream)
