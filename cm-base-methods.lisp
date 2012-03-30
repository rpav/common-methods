(in-package :cm-base-methods)
(in-readtable cm-accessors)

 ;; Testing

(def .p t ((:= object))
  "=> BOOLEAN
Basic equality testing.  This is by default `EQUALP`, but is primarily
intended for overriding."
  (equalp self object))

 ;; Conversion

(def .to-sequence)
(def .to-string)
(def .to-list)
(def .to-vector)

(def .to-string t ()
  (princ-to-string self))

(def .to-sequence sequence () self)
(def .to-string string () self)
(def .to-list list () self)
(def .to-vector vector () self)

(def .to-list vector ()
  (coerce self 'list))

(def .to-vector list ()
  (coerce self 'vector))


 ;;; Utility

(def .join sequence (with)
  "Join elements of a sequence with a string"
  (reduce #'(lambda (x y)
              (concatenate 'string
                           (.to-string x) with (.to-string y)))
          self))

(def .split sequence ((:on item))
  (let (list)
    (loop for s = (1+ (or p -1))
          for p = (position item self :start s)
          while p do
            (push (subseq self s p) list)
          finally
             (push (subseq self s) list)
             (return (nreverse list)))))
