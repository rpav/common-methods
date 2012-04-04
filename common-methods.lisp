(in-package :common-methods)

 ;; Utility
(defun join (string sequence)
  (reduce (lambda (x y)
            (concatenate 'string (string x) string (string y))) sequence))

(defun safe-package (symbol)
  (if (eq (symbol-package symbol)
          (find-package :common-lisp))
      *package*
      (symbol-package symbol)))

 ;; defclass*

(defun parse-slot (slot)
  (destructuring-bind (slot-name &key ((:= value)) type doc arg (no-arg nil))
      slot
    (let ((package (safe-package slot-name)))
      `(,slot-name :accessor ,(intern (concatenate 'string "@" (string slot-name))
                                      package)
                   ,@(unless no-arg
                       (if arg
                           `(:initarg ,arg)
                           `(:initarg ,(intern (symbol-name slot-name) 'keyword))))
                   :initform ,value
                   ,@(if type (list :type type))
                   ,@(if doc (list :documentation doc))))))


(defmacro defclass* (class-name &key subclass export slots documentation options)
  (let ((class-defn
          (append
           `(defclass ,class-name ,subclass
              ,(mapcar #'(lambda (s)
                           (let ((slot (if (listp s) s (list s))))
                             (parse-slot slot)))
                slots))
           (when (or options documentation)
             (if documentation
                 (list (append `(:documentation ,documentation) options))
                 (list options))))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(if export
              `(export ',(ensure-symbol class-name)))
         ,class-defn))))

 ;; make-common-method

(defun named-args (list)
  (if list
      (unless (and (symbolp (car list)) (eq (elt (string (car list)) 0) #\&))
        `(,(car list) ,@(named-args (cdr list))))))

(defun normal-arg (arg)
  (not (or (eq (car arg) :*)
           (eq (car arg) :**))))

(defun extract-names (arglist)
  (loop for arg on arglist by #'cddr
     if (and (keywordp (car arg)) (normal-arg arg))
     collect (car arg) into names end
     finally (return names)))

(defun method-encode (method args)
  (let ((names (sort (named-args (mapcar (lambda (x) (if (listp x) (car x) x)) args))
                     #'string<))
        (package (if (or (dotted-p method)
                         (eq (symbol-package method) (find-package 'cm-dot-methods)))
                     'cm-methods
                     (symbol-package method)))
        (method (if (dotted-p method) (undotted method) method)))
    (intern (join "*" `("" ,method ,@names)) package)))

(defun make-args (arglist)
  (flet ((sort-args (args)
           (mapcar #'cadr
                   (sort args
                         (lambda (a b) (string< (car a) (car b)))))))
    (loop for arg on arglist by #'cddr
          if (and (keywordp (car arg)) (normal-arg arg))
            collect (list (car arg) (cadr arg)) into named-args
          else
            do
               (return
                 (values (sort-args named-args)
                         (case (car arg)
                           (:* (cdr arg))
                           (:** (cadr arg))
                           (t arg))))
          end
          finally (return (sort-args named-args)))))

(defmacro make-common-method (method &optional export-p)
  (let ((defs (unless (fboundp method)
                `((declaim (inline ,method))
                  (defun ,method (object &rest method-args)
                    (let ((spec-method (method-encode ',method (extract-names method-args))))
                      (multiple-value-bind (spec-args rest-args) (make-args method-args)
                        (apply spec-method object (nconc spec-args rest-args)))))
                  (define-compiler-macro ,method (object &rest method-args)
                    (let ((spec-method (method-encode ',method (extract-names method-args))))
                      (multiple-value-bind (spec-args rest-args) (make-args method-args)
                        (etypecase rest-args
                          (list `(,spec-method ,object ,@spec-args ,@rest-args))
                          (symbol `(apply #',spec-method ,object ,@spec-args ,rest-args))
                          (null `(,spec-method ,object ,@spec-args))))))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@defs
       (if ,export-p
           (export ',method (symbol-package ',method))
           (unexport ',method (symbol-package ',method))))))


 ;; def

;;;; Influenced by SBCL's parse-defmethod
(defun parse-def (method args &optional without-class)
  (let* ((class (if without-class nil (pop args)))
         (options '((:export . nil)
                    (:type . :common)))
         qualifiers lambda-list)
    (loop until (listp (car args))
          do (push (pop args) qualifiers))
    (setf lambda-list (pop args))
    (loop while (keywordp (car args))
          do (push (cons (pop args) (pop args)) options))
    (when (and (listp method) (eq (car method) 'setf))
      (push '(:type . :setf) options))
    (values class (nreverse qualifiers) lambda-list options args)))

(defun make-method-args (arglist &key (rest-p t))
  (let* ((rest-decl-pos (position '&rest arglist))
         (rest-sym (if (not rest-decl-pos)
                       (gensym "NO-REST-")))
         (args
           (mapcar #'(lambda (arg)
                       (if (and (listp arg)
                                (keywordp (car arg)))
                           (if (cddr arg) (cdr arg) (cadr arg))
                           arg))
                   (sort (subseq arglist 0 rest-decl-pos)
                         (lambda (a b)
                           (string< (if (listp a) (car a) a)
                                    (if (listp b) (car b) b)))))))
    (values
     (if rest-p
         (if rest-sym
             (append args (list '&rest rest-sym))
             (append args (subseq arglist rest-decl-pos)))
         args)
     rest-sym)))

(defun make-generic-args (arglist)
  (mapcar #'(lambda (arg)
              (if (listp arg)
                  (if (keywordp (car arg)) (cadr arg) (car arg))
                  arg)) arglist))

(defun dotted-p (method)
  (char= (char (string method) 0) #\.))

(defun undotted (method &optional (package 'cm-dot-methods))
  (intern (subseq (string method) 1) package))

(defun dotted (method)
  (intern (concatenate 'string "." (string method)) 'cm-dot-methods))

(defun %define-common-generic (spec-method spec-args &optional options)
  (let* ((gen-args (make-generic-args spec-args)))
    (setf (elt gen-args (1+ (position '&rest gen-args))) :rest)
    `(defgeneric ,spec-method ,gen-args ,@options)))

(defun %def (method qualifiers self lambda-list options body)
  (let* ((method (if (eq (symbol-package method) (find-package 'cm-methods))
                     (dotted method)
                     method))
         (spec-method (method-encode method lambda-list))
         (dotted-p (dotted-p method))
         (export-p (cdr (assoc :export options)))
         (self-arg (car (make-method-args (list self) :rest-p nil))))
    (when dotted-p (unintern method))
    (multiple-value-bind (spec-args rest-arg)
        (make-method-args lambda-list)
      `(progn
         ,(when self
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               ,(unless (fboundp spec-method)
                  (%define-common-generic spec-method (append (list self) spec-args)))
               (defmethod ,spec-method ,@qualifiers (,self-arg ,@spec-args)
                 ,@(if rest-arg
                       (list `(declare (ignore ,rest-arg))))
                 ,@body)
               ,(if (or export-p dotted-p)
                    `(export ',spec-method (symbol-package ',spec-method))
                    `(unexport ',spec-method (symbol-package ',spec-method)))))
         ,(if dotted-p
              `(progn
                 (make-common-method ,(intern (string method) 'cm-dot-methods) t)
                 (make-common-method ,(undotted (string method) 'cm-methods) t))
              `(make-common-method ,method ,export-p))))))

(defun %defmethod (method qualifiers self lambda-list options body)
  (let* ((export-p (cdr (assoc :export options))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmethod ,method ,@qualifiers (,self ,@lambda-list)
         ,@body)
       ,(if export-p
            `(export ',method (symbol-package ',method))
            `(unexport ',method (symbol-package ',method))))))

(defun %defsetf (method qualifiers self lambda-list options body)
  (let* ((method (if (eq (symbol-package (cadr method))
                         (find-package 'cm-methods))
                     (dotted (cadr method))
                     (cadr method)))
         (value-arg (car (make-method-args (list (pop lambda-list)) :rest-p nil)))
         (spec-method (method-encode method lambda-list))
         (dotted-p (dotted-p method))
         (export-p (cdr (assoc :export options)))
         (self-arg (car (make-method-args (list self) :rest-p nil))))
    (when dotted-p (unintern method))
    (multiple-value-bind (spec-args rest-arg)
        (make-method-args lambda-list)
      `(progn
         ,(when self
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               ,(unless (fboundp `(setf ,spec-method))
                  (%define-common-generic `(setf ,spec-method) (append (list value-arg self) spec-args)))
               (defsetf ,method (object &rest method-args) (v)
                 (let ((spec-method (method-encode ',method (extract-names method-args))))
                   (multiple-value-bind (spec-args rest-args) (make-args method-args)
                     (etypecase rest-args
                       (list `(setf (,spec-method ,object ,@spec-args ,@rest-args) ,v))
                       (null `(setf (,spec-method ,object ,@spec-args) ,v))))))
               (defmethod (setf ,spec-method) ,@qualifiers (,value-arg ,self-arg ,@spec-args)
                 ,@(if rest-arg
                       (list `(declare (ignore ,rest-arg))))
                 ,@body)
               ,(if (or export-p dotted-p)
                    `(export ',spec-method (symbol-package ',spec-method))
                    `(unexport ',spec-method (symbol-package ',spec-method)))))
         ,(if dotted-p
              `(progn
                 (make-common-method ,(intern (string method) 'cm-dot-methods) t)
                 (make-common-method ,(undotted (string method) 'cm-methods) t))
              `(make-common-method ,method ,export-p))))))

(defun %defgeneric (method qualifiers self lambda-list options body)
  (if qualifiers
   (error "Qualifiers not allowed for GENERIC definition"))
  (let* ((spec-method (method-encode method lambda-list))
         (spec-args (make-method-args lambda-list))
         (dotted-p (dotted-p method))
         (export-p (cdr (assoc :export options))))
    (when dotted-p (unintern method))
    `(progn
       ,(if dotted-p
            `(progn
               (make-common-method ,(intern (string method) 'cm-dot-methods) t)
               (make-common-method ,(undotted (string method) 'cm-methods)) t)
            `(make-common-method ,method ,export-p))
       ,(%define-common-generic spec-method (append (list self) spec-args) body))))

(defun def-case (method qualifiers self lambda-list options body)
  (let ((type (cdr (assoc :type options))))
    (case type
      (:common (%def method qualifiers self lambda-list options body))
      (:setf (%defsetf method qualifiers self lambda-list options body))
      (:method (%defmethod method qualifiers self lambda-list options body))
      (:generic (%defgeneric method qualifiers self lambda-list options body))
      (t (error "Invalid type for DEF: ~A" type)))))

(defmacro def (method &rest args)
  (multiple-value-bind (class qualifiers lambda-list options body)
      (parse-def method args)
    (let ((self (if class (list (intern "SELF") class) nil)))
      (def-case method qualifiers self lambda-list options body))))

(defmacro def* (method &rest args)
  (multiple-value-bind (class qualifiers lambda-list options body)
      (parse-def method args t)
    (declare (ignore class))
    (let ((self (pop lambda-list)))
      (def-case method qualifiers self lambda-list options body))))

 ;; definit

(defun %definit (quals args body &optional self-decl)
  (let* ((rest-decl-pos (position '&rest args))
         (rest-sym (if (not rest-decl-pos) (gensym)
                       (elt args (1+ rest-decl-pos))))
         (rest-decl (list '&rest rest-sym))
         (other-args (named-args args)))
    `(defmethod common-lisp:initialize-instance ,@quals
         ,(if self-decl
              `(,self-decl ,@rest-decl &key ,@other-args &allow-other-keys)
              `(,@rest-decl &key ,@other-args &allow-other-keys))
       ,@(unless rest-decl-pos (list `(declare (ignore ,rest-sym))))
       ,@body)))

(defmacro definit (&rest args)
  (multiple-value-bind (class qualifiers lambda-list options body)
      (parse-def 'initialize-instance args)
    (declare (ignore options))
    (%definit qualifiers lambda-list body (list (intern "SELF") class))))

(defmacro definit* (&rest args)
  (multiple-value-bind (class qualifiers lambda-list options body)
      (parse-def 'initialize-instance args t)
    (declare (ignore class options))
    (%definit qualifiers lambda-list body)))


 ;; define-common-generic

(defmacro define-common-generic (method lambda-list &rest options)
  (let* ((self (intern "SELF"))
         (spec-method (method-encode method lambda-list))
         (spec-args (make-method-args lambda-list))
         (dotted-p (dotted-p method)))
    (when dotted-p (unintern method))
    `(progn
       ,(if dotted-p
            `(progn
               (make-common-method ,(intern (string method) 'cm-dot-methods) t)
               (make-common-method ,(undotted (string method) 'cm-methods)) t)
            `(make-common-method ,method nil))
       ,(%define-common-generic spec-method (append (list self) spec-args) options))))


 ;; Other

(defmacro new (class &rest args)
  `(make-instance ',class ,@args))

(defmacro -> (args &body body)
  `(lambda ,args ,@body))
