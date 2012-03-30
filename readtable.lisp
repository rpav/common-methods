(in-package :common-methods)

(defun open-bracket (stream char)
  (declare (ignore char))
  (let* ((input (read-delimited-list #\] stream t))
         (package (safe-package (car input)))
         (slot-acc (intern
                    (concatenate 'string "@" (string (car input)))
                    package)))
    (if (cdr input)
        `(,slot-acc ,(cadr input))
        `(,slot-acc ,(intern "SELF")))))

(export 'cm-accessors)

(defreadtable cm-accessors
  (:merge :standard)
  (:macro-char #\[ #'open-bracket)
  (:macro-char #\] (get-macro-character #\))))

