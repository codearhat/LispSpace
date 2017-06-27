;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;; Syntax nodes:
;;;;
;;;; Syntax node
;;;;   type
;;;;
;;;; Literal
;;;;   value
;;;;
;;;; Lexical variable
;;;;   name
;;;;
;;;; Query variable
;;;;   name
;;;;
;;;; Slot access form
;;;;   slot
;;;;   arg
;;;;
;;;; Association-end access form
;;;;   association-end
;;;;   arg
;;;;
;;;; Function call
;;;;   function
;;;;   args
;;;;
;;;; Macro call
;;;;   macro-name
;;;;   args
;;;;
;;;; Special form
;;;;   operator
;;;;   args

(defmacro define-syntax-node (name (&rest supers) slots)
  `(progn
     ;; syntax-node class
     ,(bind ((supers (append supers (list 'copyable-mixin))))
            `(defclass* ,name ,supers
               ,slots))
     ;; make
     ,(bind ((make-fn-name (concatenate-symbol "make-" name)))
            `(defmacro ,make-fn-name (&rest args)
               `(make-instance ',',name ,@args)))
     ;; copy
     ,@(bind ((slot-names (mapcar [if (consp !1) (first !1) !1] slots)))
             `((define-copy-method copy-inner-class progn ((-self- ,name) copy copy-htable)
                                   (with-slot-copying (copy copy-htable -self-)
                                     (copy-slots ,@slot-names)))
               (define-copy-method (copy-inner-class copy-shallow) progn ((-self- ,name) copy copy-htable)
                                   (with-slot-copying (copy copy-htable -self-)
                                     ,@(mapcar (lambda (slot-name)
                                                 `(when (slot-boundp -self- ',slot-name)
                                                    (copy-set-slot ,slot-name (slot-value -self- ',slot-name))))
                                               slot-names)))))
     ;; predicate
     ,(bind ((predicate-name (if (position #\- (symbol-name name))
                                 (concatenate-symbol name "-p")
                                 (concatenate-symbol name "p"))))
            `(defun ,predicate-name (object)
               (typep object ',name)))))

;;;
;;; Reader
;;;
(defun pattern-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((spec (read stream t nil t)))
    (if (and (consp spec) (symbolp (first spec)))
        (bind ((instance (allocate-instance (find-class (first spec)))))
          (apply #'shared-initialize instance nil (rest spec)))
        (error "wrong pattern syntax: ~A~%" spec))))

(defmacro enable-pattern-reader (&optional (dispatch-character #\M))
  "Enable the pattern reader for the rest of the file (being loaded or compiled).
Be careful when using in different situations, because it modifies *readtable*."
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (set-dispatch-macro-character #\# ,dispatch-character #'pattern-reader)))

;;;
;;; AST nodes
;;;

(defconstant +unknown-type+ :unknown)

(define-syntax-node syntax-object ()
  ((persistent-type +unknown-type+)
   (volatilep :accessor volatilep)))

(define-syntax-node unparsed-form (syntax-object)
  ((form)))

(define-syntax-node atomic-form (syntax-object)
  ())

(define-syntax-node literal-value (atomic-form)
  ((value)))

(define-syntax-node variable (atomic-form)
  ((name)))

(define-syntax-node lexical-variable (variable)
  ())

(define-syntax-node dynamic-variable (variable)
  ())

(define-syntax-node query-variable (variable)
  ((persistent-type +persistent-object-class+)
   (referenced-slots nil)))

(define-syntax-node joined-variable (query-variable)
  ((object :documentation "Object which owns the association-end.")
   (association-end :documentation "The association-end of the object or NIL (means id).")))

(define-syntax-node compound-form (syntax-object)
  ((operator)
   (operands)))

(define-syntax-node function-call (compound-form)
  ((operator :initarg :fn :accessor fn-of)
   (operands :initarg :args :accessor args-of)))

(define-syntax-node slot-access (function-call)
  ((operator :initarg :accessor :accessor accessor-of)
   (slot nil)))

(define-syntax-node association-end-access (slot-access)
  ((slot :initarg :association-end :accessor association-end-of)))

(define-syntax-node macro-call (compound-form)
  ((operator :initarg :macro :accessor macro-of)
   (operands :initarg :args :accessor args-of)))

(define-syntax-node special-form (compound-form)
  ())

(defmethod make-load-form ((object syntax-object) &optional env)
  (make-load-form-saving-slots
   object
   :environment env))

(defgeneric arg-of (slot-access)
  (:method ((access slot-access))
           (first (args-of access))))

(defgeneric (setf arg-of) (value slot-access)
  (:method (value (access slot-access))
           (setf (args-of access) (list value))))

(def print-object (variable :identity nil)
    (princ (name-of -self-)))

(def print-object literal-value
    (princ (value-of -self-)))

(def print-object (compound-form :identity nil)
  (progn
    (princ (if (slot-boundp -self- 'operator) (operator-of -self-) "?"))
    (princ " ")
    (princ (if (slot-boundp -self- 'operands) (operands-of -self-) "?"))))

(defun null-literal-p (syntax)
  (and (literal-value-p syntax)
       (null (value-of syntax))))

;;; Walker
;;;
;; define a ghost of the hu.dwim.walker ast class hierarchy so that each walked ast node has 'syntax-object as its superclass
#|
#.(iter (for node-class :in (hu.dwim.walker:collect-standard-walked-form-subclasses))
        (for node-class-name = (class-name node-class))
        (for local-name = (intern (symbol-name node-class-name) :hu.dwim.perec))
        (assert (not (eq node-class-name local-name)))
        (collect `(defclass ,local-name (syntax-object ,node-class-name)
                    ())
          :into class-definitions)
        (collect (cons node-class-name local-name)
          :into ast-node-type-mappings)
        (finally
         (return
           `(progn
              (defparameter *query-walker-ast-node-type-mapping* (alexandria:alist-hash-table ',ast-node-type-mappings :test 'eq))
              (def layered-method hu.dwim.walker::ast-node-type-for (type)
                (or (gethash type *query-walker-ast-node-type-mapping*)
                    (call-next-layered-method)))
              ,@class-definitions))))
|#


;;;;
;;;; Unparse
;;;;

(defgeneric unparse-query-syntax (syntax)
  (:method :around ((syntax syntax-object))
           (if (slot-boundp syntax 'volatilep)
               `(,(if (volatilep syntax) 'volatile 'static) ,(call-next-method))
               (call-next-method)))
  (:method ((unparsed unparsed-form))
    (form-of unparsed))
  (:method ((variable variable))
    (name-of variable))
  (:method ((literal literal-value))
    (if (self-evaluating-p (value-of literal))
        (value-of literal)
        `(quote ,(value-of literal))))
  (:method ((form compound-form))
    (cons (operator-of form) (mapcar 'unparse-query-syntax (operands-of form))))
  (:method ((pair cons)) ;; legacy
    (rcons (unparse-query-syntax (car pair))
           (unparse-query-syntax (cdr pair))
           pair))
  (:method (object) ;; legacy
    object))

(defun self-evaluating-p (val)
  (and (atom val)
       (or (not (symbolp val))
           (keywordp val)
           (eq val t)
           (eq val nil))))

(defun parse-args-p (macro-name)
  (or (member macro-name '(and or))
      (member macro-name hu.dwim.rdbms::*sql-constructor-names*))) ; TODO missing export

(defun expand-macro-call-p (macro-name)
  (member macro-name hu.dwim.rdbms::*sql-constructor-names*))

;;;;
;;;; Substitute
;;;;
(defgeneric substitute-syntax (syntax subs)

  (:method :around (syntax subs)
           (aif (assoc syntax subs)
                (cdr it)
                (call-next-method)))
  
  (:method ((syntax t) (subs null))
    syntax)

  (:method ((syntax t) (subs cons))
    syntax)

  (:method ((literal literal-value) (subs cons)) ; FIXME
    (bind ((value (substitute-syntax (value-of literal) subs)))
      (if (eq value (value-of literal))
          literal
          value)))

  (:method ((cons cons) (subs cons))
    (rcons (substitute-syntax (car cons) subs)
           (substitute-syntax (cdr cons) subs)
           cons))

  (:method ((unparsed unparsed-form) (subs cons))
    (bind ((form (substitute-syntax (form-of unparsed) subs)))
      (if (eq form (form-of unparsed))
          unparsed
          (aprog1 (copy-shallow unparsed)
            (setf (form-of it) form)))))

  (:method ((compound compound-form) (subs cons))
    (bind ((operands (substitute-syntax (operands-of compound) subs)))
      (if (eq operands (operands-of compound))
          compound
          (aprog1 (copy-shallow compound)
            (setf (operands-of it) operands))))))

(defgeneric syntax-fold (syntax f g)

  (:method (syntax f g)
           (funcall f syntax))

  (:method ((compound compound-form) f g)
           (funcall g (funcall f compound) (syntax-fold (operands-of compound) f g)))

  (:method ((unparsed unparsed-form) f g)
           (funcall g (funcall f unparsed) (syntax-fold (form-of unparsed) f g)))

  (:method ((cons cons) f g)
           (funcall g (funcall f cons) (syntax-fold (car cons) f g) (syntax-fold (cdr cons) f g))))

(defun find-if-syntax (predicate syntax)
  (syntax-fold
   syntax
   (lambda (node) (when (funcall predicate node) node))
   (lambda (parent &rest children) (or parent (some #'identity children)))))


(defun syntax= (left right)
  (cond ((eql left right) #t)
        ((and (syntax-object-p left) (syntax-object-p right))
         (syntax-object= left right))
        ((and (consp left) (consp right))
         (and (syntax= (first left) (first right))
              (syntax= (rest left) (rest right))))
        (t #f)))

(defgeneric syntax-object= (left right)
  (:method-combination and)

  (:method and ((left syntax-object) (right syntax-object))
           (syntax= (persistent-type-of left) (persistent-type-of right)))

  (:method and ((left unparsed-form) (right unparsed-form))
           (syntax= (form-of left) (form-of right)))

  (:method and ((left literal-value) (right literal-value))
           (equalp (value-of left) (value-of right)))

  (:method and ((left variable) (right variable))
           (eql (name-of left) (name-of right)))

  (:method and ((left joined-variable) (right joined-variable))
           (and (syntax= (object-of left) (object-of right))
                (eql (association-end-of left) (association-end-of right))))

  (:method and ((left compound-form) (right compound-form))
           (and (syntax= (operator-of left) (operator-of right))
                (syntax= (operands-of left) (operands-of right))))

  (:method and ((left slot-access) (right slot-access))
           (eql (slot-of left) (slot-of right))))
