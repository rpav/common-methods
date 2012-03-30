(defsystem "common-methods"
  :description "A common method system for Common Lisp/CLOS"
  :version "0.1"
  :author "Ryan Pavlik <rpavlik@gmail.com>"
  :license "LLGPL, NewBSD"

  :depends-on (#:alexandria #:iterate #:closer-mop #:named-readtables)
  
  :serial t
  :components ((:file "package")
               (:file "common-methods")
               (:file "readtable")
               (:file "cm-meta")
               (:file "cm-base-methods")
               (:file "cm-sequences")))
