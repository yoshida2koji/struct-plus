(cl:in-package :cl-user)

(defpackage :struct+
  (:use :cl)
  (:export
   :intern-name
   :defstruct+
   :with-struct
   :let-struct))

(in-package :struct+)

(defun slots-to-slot-names (slots)
  (mapcar (lambda (slot)
            (if (consp slot)
                (first slot)
                slot))
          slots))

(defun str-accessor-name (slot-name str-type)
  (intern (format nil "~a-~a" str-type slot-name) (symbol-package str-type)))

(defun str-slot-accessors (slot-names str-type)
  (mapcar (lambda (slot)
            (if (consp slot)
                (list (first slot)
                      (str-accessor-name (second slot) str-type))
                (list slot
                      (str-accessor-name slot str-type))))
          slot-names))

(defmacro with-struct (slot-names str-var str-type &body body)
  `(with-accessors ,(str-slot-accessors slot-names str-type) ,str-var
     ,@body))

(defun str-slot-bindings (slot-names str-var str-type)
  (mapcar (lambda (slot)
            (if (consp slot)
                (list (first slot)
                      (list (str-accessor-name (second slot) str-type) str-var))
                (list slot
                      (list (str-accessor-name slot str-type) str-var))))
          slot-names))

(defmacro let-struct (slot-names str-var str-type &body body)
  (let ((str-var-sym (gensym)))
    `(let ((,str-var-sym ,str-var))
       (let ,(str-slot-bindings slot-names str-var-sym str-type)
         ,@body))))

(defun intern-name (symbol format)
  (intern (format nil (format nil "~@:(~a~)"  format) (symbol-name symbol))))

(defun ordered-slot-names (use-slots slots)
  (remove-if (lambda (s)
               (equal "_" (symbol-name s)))
             (mapcar (lambda (s1 s2)
                       (list s1 s2))
                     use-slots
                     (slots-to-slot-names slots))
             :key #'car))

(defmacro define-with-struct (str-type)
  `(defmacro ,(intern-name str-type "WITH-~a")
       (slot-names str-var &body body)
     `(with-struct ,slot-names ,str-var ,',str-type ,@body)))

(defmacro define-with-struct-ordered (str-type slots)
  `(defmacro ,(intern-name str-type "WITH-~a-ORDERED")
       (slot-names str-var &body body)
     (let ((slot-names (ordered-slot-names slot-names ',slots)))
       `(with-struct ,slot-names ,str-var ,',str-type ,@body))))

(defmacro define-let-struct (str-type)
  `(defmacro ,(intern-name str-type "LET-~a")
       (slot-names str-var &body body)
     `(let-struct ,slot-names ,str-var ,',str-type ,@body)))

(defmacro define-let-struct-ordered (str-type slots)
  `(defmacro ,(intern-name str-type "LET-~a-ORDERED")
       (slot-names str-var &body body)
     (let ((slot-names (ordered-slot-names slot-names ',slots)))
       `(let-struct ,slot-names ,str-var ,',str-type ,@body))))

(defmacro define-str-slot-names (str-type slots)
  `(defun ,(intern-name str-type "~a-SLOT-NAMES") ()
     ',(slots-to-slot-names slots)))

(defun export-symbols (name slots)
  (let (symbols)
    (flet ((add (fmt &rest syms)
             (push (intern (string-upcase (apply #'format nil fmt syms)))
                   symbols)))
      (add "~a" name)
      (add "make-~a" name)
      (add "copy-~a" name)
      (add "~a-p" name)
      (add "with-~a" name)
      (add "with-~a-ordered" name)
      (add "let-~a" name)
      (add "let-~a-ordered" name)
      (add "~a-slot-names" name)
      (mapc (lambda (slot)
              (add "~a" slot)
              (add "~a-~a" name slot))
            (slots-to-slot-names slots)))
    (nreverse symbols)))

(defmacro defstruct+ (name (&key export-all-p include) &rest slots)
  (let* ((include-str (if (consp include) (car include) include))
         (include-str-slots (if (consp include) (cdr include) nil))
         (accessible-slots (append include-str-slots slots)))
    `(progn
       ,(when export-all-p
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (export ',(export-symbols name accessible-slots))))
       (defstruct ,(if include-str `(,name (:include ,include-str)) name)
         ,@slots)
       (define-str-slot-names ,name ,accessible-slots)
       (define-with-struct ,name)
       (define-with-struct-ordered ,name ,accessible-slots)
       (define-let-struct ,name)
       (define-let-struct-ordered ,name ,accessible-slots)
       (values))))
