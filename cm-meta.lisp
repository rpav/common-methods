(in-package :cm-base-methods)
(in-readtable cm-accessors)

 ;; Utility

(defun exact-p (symbol)
  (and (char= (char (string symbol) 0) #\*)))

(defun make-exact (symbol)
  (if (exact-p symbol)
      (string symbol)
      (concatenate 'string "*" (if (common-methods::dotted-p symbol)
                                   (string (common-methods::undotted symbol))
                                   (string symbol)))))

 ;; Methods

(def cm-meta::methods symbol ((:for classes)) :export t
  "Given a name, find methods that apply for given class names"
  (compute-applicable-methods-using-classes
   (symbol-function self)
   (mapcar #'find-class classes)))

(def cm-meta::lookup symbol () :export t
  "Find a list of methods for the common method"
  (let* ((dottedp (common-methods::dotted-p self))
         (exact (make-exact self))
         (len (length exact))
         (package (if dottedp
                      (find-package 'cm-methods)
                      (symbol-package self))))
    (loop for s being the symbols in package
          as str = (string s)
          if (and (fboundp s)
                  (or (string= str exact)
                      (and (>= (length str) len)
                           (string= exact (subseq str 0 len)))))
            collecting (generic-function-methods (symbol-function s))
              into list
          end
          finally (return list))))

(def cm-meta::direct-methods-for class ((:in package)) :export t
  (let ((package (find-package package)))
    (loop for m in (specializer-direct-methods self)
          as gf = (method-generic-function m)
          as name = (generic-function-name gf)
          if (and (symbolp name)
                  (eq (symbol-package name) (find-package package)))
            collect (symbol-name (generic-function-name gf))
              into list
          end
          finally (return list))))

(def cm-meta::direct-methods-for symbol ((:in package)) :export t
  (cm-meta:direct-methods-for (find-class self) :in package))

(def cm-meta::methods-for class ((:in package)) :export t
  (let ((class-list (compute-class-precedence-list  self)))
    (loop for class in class-list
          appending (cm-meta:direct-methods-for class :in package) into list
          finally (return (remove-duplicates (sort list #'string<))))))

(def cm-meta::methods-for symbol ((:in package)) :export t
  (cm-meta:methods-for (find-class self) :in package))

(def cm-meta::methods-for symbol () :export t
  (cm-meta:methods-for (find-class self) :in (symbol-package self)))

 ;; Documentation

(def cm-meta::doc symbol () :export t
  "Produce documentation for methods given a common method"
  (mapcar
   (-> (methods)
     (mapcar
      (-> (method)
        (let ((name (generic-function-name (method-generic-function method))))
          (list name
                (mapcar #'class-name (method-specializers method))
                (documentation method t))))
        methods))
   (cm-meta:lookup self)))
