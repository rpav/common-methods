(in-package :cm-base-methods)
(in-readtable cm-accessors)

 ;; Sequence-alike API
(def .each)
(def .map)
(def .first)
(def .elt)
(def .length)

(def .first sequence ()
  (elt self 0))

(def .elt sequence ((:at n))
  (elt self n))

(def .length sequence ()
  (length self))

(def .map sequence ((:do fn))
  (map (type-of self) fn self))

(def .map t ((:do fn))
  (funcall fn self))

(def .each sequence ((:do fn))
  (map nil fn self))

 ;; Additional stuff
(def .each sequence ((:to n) (:do fn))
  (let ((len (.length self)))
    (dotimes (i (1+ n))
      (funcall fn
               (if (< i len)
                   (.elt self :at i) nil)))))

(def .each list ((:to n) (:do fn))
  (let ((cur self))
    (dotimes (i (1+ n))
      (funcall fn (car cur))
      (setq cur (cdr cur)))))

(def .each t ((:do fn))
  (funcall fn self))

(def .map sequence ((:do do-fn) (:remove-if remove-if-fn))
  (remove-if remove-if-fn (.map self :do do-fn)))

 ;; Iterators
(defclass* iterator :export t)

; Subclasses implement:
(def .next)
(def .endp)

; Methods
(def .each iterator ((:do fn))
  (do ((item (.next self) (.next self)))
      ((.endp self))
      (funcall fn item)))

;; Sequence Iterator
(defclass* sequence-iterator
    :subclass (iterator)
    :export t
    :slots (sequence elt))

(definit sequence-iterator (on)
  (setf [sequence] on)
  (setf [elt] 0))

(def .iterator sequence ()
  (new sequence-iterator :on self))

(def .next sequence-iterator ()
  (if (.endp self)
      nil
      (let ((val (elt [sequence] [elt])))
        (setf [elt] (1+ [elt]))
        val)))

(def .endp sequence-iterator ()
  (>= [elt] (length [sequence])))


;; List Iterator
(defclass* list-iterator
    :subclass (iterator)
    :export t
    :slots (list))

(definit list-iterator (on)
  (setf [list] (cons nil on)))

(def .iterator list ()
  (new list-iterator :on self))

(def .next list-iterator ()
  (setf [list] (cdr [list]))
  (car [list]))

(def .endp list-iterator ()
  (endp [list]))

