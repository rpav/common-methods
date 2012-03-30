(defpackage #:common-methods
  (:use #:closer-common-lisp #:asdf #:alexandria #:named-readtables)
  (:export #:defclass* #:define-common-generic #:new
           #:def #:def* #:definit #:definit* #:->
           #:cm-accessors)
  (:nicknames #:cm))

(defpackage #:cm-methods
  (:nicknames #:m))

(defpackage #:cm-dot-methods)

(defpackage #:cm-base-methods
  (:use #:closer-common-lisp #:asdf #:named-readtables #:common-methods
        #:cm-dot-methods))

(defpackage #:cm-meta)

(defpackage #:cm-user
  (:use #:closer-common-lisp #:common-methods #:cm-dot-methods))
