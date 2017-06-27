;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;; Type specifier:
;;;;
;;;;   persistent classes (types of persistent objects)
;;;;   names of persistent classes
;;;;   and,or,not
;;;;   satisfies, member, eql,
;;;;   unbound, null, nil, t, serialized, boolean, integer, integer-16, integer-32, integer-64,
;;;;   float, float-32, float-64, double, number, string, text, symbol, symbol*, date, time, timestamp,
;;;;   duration, list, form, unsigned-byte, vector, unsigned-byte-vector, ip-address,
;;;;   set, disjunct-set, ordered-set
;;;;
;;;; Type expression:
;;;;   an expression that evaluates to a type specifier
;;;;

;;;
;;; Declare function types
;;;
(def function persistent-ftype-of (name)
  (get name 'persistent-ftype +unknown-type+))

(def function (setf persistent-ftype-of) (type-specifier name)
  (setf (get name 'persistent-ftype) type-specifier))

(def macro declaim-ftype (names &body type-spec)
  (assert (length= 1 type-spec))
  `(progn
     ,@(iter (for name in (ensure-list names))
             (collect `(setf  (persistent-ftype-of ',name) ',(first type-spec))))))

;;;
;;; Infer function argument and return types
;;;

(def function type-variables-of (type-specifier)
  (ecase (first type-specifier)
    (forall (mapcar
             (lambda (var-spec)
               (etypecase var-spec
                 (cons (cons (first var-spec) (second var-spec)))
                 (symbol (cons var-spec t))))
             (second type-specifier)))
    (function nil)))

(def function arg-types-of (type-specifier)
  (ecase (first type-specifier)
    (forall (arg-types-of (third type-specifier)))
    (function (second type-specifier))))

(def function return-type-of (type-specifier)
  (ecase (first type-specifier)
    (forall (return-type-of (third type-specifier)))
    (function (third type-specifier))))

(def function contains-type-variable-p (type-pattern type-variables)
  (etypecase type-pattern
    (null #f)
    (symbol (assoc type-pattern type-variables))
    (atom #f)
    (cons (or (contains-type-variable-p (car type-pattern) type-variables)
              (contains-type-variable-p (cdr type-pattern) type-variables)))))

(def function match-types (type-specifier args &optional type-variables)
  "Success: return type + type of each arg; failed: return NIL."
  (ecase (first type-specifier)
    (forall
     (match-types (third type-specifier)
                  args
                  (append (type-variables-of type-specifier) type-variables)))
    (function
     (bind ((expected-arg-types (expand-arg-typespec (second type-specifier) args))
            (expected-return-type (third type-specifier)))
       (when expected-arg-types
         (bind ((arg-types (mapcar #'persistent-type-of args))
                (type-variable-bindings (match-type-patterns expected-arg-types arg-types type-variables)))
           (when (and (not (eq type-variable-bindings :failed))
                      (every (lambda (variable)
                               (assoc (car variable) type-variable-bindings))
                             type-variables))
             (substitute-type-variables (list* expected-return-type expected-arg-types)
                                        type-variable-bindings))))))))

(def function match-type-patterns (patterns types type-variables)
  (bind (type-environment)
    (iter (for pattern in patterns)
          (for type in types)
          (setf type-environment (match-type-pattern pattern type type-variables type-environment))
          (until (eq type-environment :failed))
          (finally (return type-environment)))))

(def function match-type-pattern (pattern type type-variables type-environment)
  (cond
    ((eq type-environment :failed) :failed)
    ((eq type +unknown-type+) type-environment)
    ((contains-syntax-p type) :failed)
    ((and (symbolp pattern) (assoc pattern type-variables))
     (if (assoc pattern type-environment)
         (if (match-type-pattern (cdr (assoc pattern type-environment)) type type-variables type-environment)
             type-environment
             (error "Inconsistent types: ~S and ~S" (cdr (assoc pattern type-environment)) type))
         (if (subtypep type (cdr (assoc pattern type-variables)))
             (acons pattern type type-environment)
             (error "Expected type: ~S, found type: ~S" (cdr (assoc pattern type-variables)) type))))
    ((not (contains-type-variable-p pattern type-variables))
     (if (subtypep type pattern)
         type-environment
         :failed))
    ((listp pattern)
     (if (subtypep type (first pattern))
         (if (listp type)
             (match-type-pattern (second pattern) (second type) type-variables type-environment) ;; FIXME
             type-environment)
         :failed))
    (t :failed)))

(def function parse-arg-typespec (arg-typespec)
  (iter (with rest-arg-type = nil)
        (with last-marker = nil)
        (for next-arg in arg-typespec)
        (if (member next-arg '(&optional &rest &key))
            (setf last-marker next-arg)
            (ecase last-marker
              ((nil) (collect next-arg into mandatory-arg-types))
              (&optional (collect next-arg into optional-arg-types))
              (&rest (assert (null rest-arg-type))
                     (setf rest-arg-type next-arg))
              (&key (assert (and (listp next-arg) (keywordp (first next-arg)) (second next-arg)))
                    (collect (cons (first next-arg) (second next-arg)) into key-arg-types))))
        (finally (return (values mandatory-arg-types optional-arg-types rest-arg-type key-arg-types)))))

(def function expand-arg-typespec (arg-typespec args)
  (bind (((:values mandatory-arg-types optional-arg-types rest-arg-type key-arg-types) (parse-arg-typespec arg-typespec))
         (args args)
         (result nil))

    (iter (for type in mandatory-arg-types)
          (if (null args)
              (progn
                (warn "Missing mandatory arg with type ~S" type)
                (return-from expand-arg-typespec nil))
              (progn
                (pop args)
                (push type result))))

    (iter (for type in optional-arg-types)
          (while args)
          (pop args)
          (push type result))

    (iter (while args)
          (for next-arg = (pop args))
          (for next-keyword = (typecase next-arg
                                (keyword next-arg)
                                (literal-value (when (keywordp (value-of next-arg))
                                                 (value-of next-arg)))))
          (acond
           ((and next-keyword (assoc next-keyword key-arg-types))
            (unless args
              (warn "Missing value for &key param: ~S" next-keyword)
              (return-from expand-arg-typespec nil))
            (push 'keyword result)
            (pop args)
            (push (cdr it) result))
           (rest-arg-type
            (push rest-arg-type result))
           (t
            (warn "Extra arg: ~S" next-arg)
            (return-from expand-arg-typespec nil))))

    (reverse result)))

(def function substitute-type-variables (patterns type-variable-bindings)
  (sublis type-variable-bindings patterns))



;;;----------------------------------------------------------------------------
;;; Type inference
;;;

(defun infer-types (query) ; TODO clean up
  "Annotates types to the SYNTAX nodes of the query."
  (process-toplevel-typep-asserts query)
  (mapc-query [%infer-types !1 query] query)
  (when (eq (action-of query) :update)
    (iter (for (place value) :on (action-args-of query) :by 'cddr)
          (cond
            ((and (not (has-default-type-p place))
                  (has-default-type-p value))
             (setf (persistent-type-of value) (persistent-type-of place)))
            ((and (not (has-default-type-p value))
                  (has-default-type-p place))
             (setf (persistent-type-of place) (persistent-type-of value))))))
  (mapc-query #'check-types query)
  (when (offset-of query)
    (setf (persistent-type-of (offset-of query)) 'integer))
  (when (limit-of query)
    (setf (persistent-type-of (limit-of query)) 'integer)))

(defun process-toplevel-typep-asserts (query)
  (setf (asserts-of query)
        (mapcan
         (lambda (assert)
           (cond
             ((and (function-call-p assert)
                   (eq (fn-of assert) 'typep)
                   (query-variable-p (first (args-of assert))))
              (restrict-variable-type (first (args-of assert))
                                      (type-syntax->type (second (args-of assert))))
              nil)
             (t
              (list assert))))
         (asserts-of query))))

(defgeneric %infer-types (syntax query)
  (:method (syntax query)
    (declare (ignore query))
    (persistent-type-of syntax))

  #+nil ; TODO this should be enabled in some form...
  (:method ((node literal-value) query)
    (setf (persistent-type-of node) (type-of (value-of node))))

  (:method ((subselect subselect) query)
    (infer-types subselect)
    (persistent-type-of subselect))

  (:method ((form compound-form) query)
    (mapc [%infer-types !1 query] (operands-of form))
    (bind ((operator (operator-of form))
           (operands (operands-of form))
           (type (persistent-ftype-of operator)))
      (when (and type (not (eq type +unknown-type+)))
        (when-bind inferred-types (match-types type operands)
          (setf (persistent-type-of form) (first inferred-types))
          (iter (for type in (rest inferred-types))
                (for operand in operands)
                (when (has-default-type-p operand)
                  (setf (persistent-type-of operand)
                        (if (or (lexical-variable-p operand)
                                (dynamic-variable-p operand)
                                (and (literal-value-p operand)
                                     (null (value-of operand))
                                     (not (typep nil type))))
                            (ensure-null-subtypep type)
                            type))
                  (%infer-types operand query))))))
    (persistent-type-of form))

  (:method ((access slot-access) query)
    (call-next-method)
    (unless (slot-of access)
      (setf (slot-of access)
            (find-slot-for-access access (slots-for-slot-access access))))
    (when (slot-of access)
      (setf (persistent-type-of access)
            (slot-definition-type (slot-of access))))
    (persistent-type-of access)))

(defun restrict-variable-type (variable type)
  (let ((orig-type (persistent-type-of variable)))
    (cond
      ((eq orig-type +persistent-object-class+) (setf (persistent-type-of variable) type))
      ((and (listp orig-type) (eq (first orig-type) 'and)) (appendf (persistent-type-of variable) type))
      (t (setf (persistent-type-of variable) (list 'and orig-type type))))))

(defgeneric slots-for-slot-access (access)
  (:method ((access slot-access))
           (effective-slots-for-accessor (accessor-of access)))
  
  (:method ((access association-end-access))
           (effective-association-ends-for-accessor (accessor-of access))))

(defun find-slot-for-access (access slots)
  (generalize-slot-access
   (or (find-slot-by-owner-type (arg-of access) slots)
       (find-slot-by-slot-type (persistent-type-of access) slots))))

(defun generalize-slot-access (slot)
  (aif (and slot (persistent-effective-slot-precedence-list-of slot))
       (first (last it))
       slot))

(defun find-slot-by-owner-type (owner slots)
  (bind ((owner-type (normalized-type-for* (persistent-type-of owner))))
    (acond
     ((length= 1 slots)
      (first slots))
     ((and (not (eq owner-type +unknown-type+))
           (not (contains-syntax-p owner-type)))
      (find owner-type slots :key 'persistent-slot-definition-class :test 'subtypep)))))

(defun find-slot-by-slot-type (type slots)
  (cond
    ((length= 1 slots) (first slots))
    ((and (not (eq type +unknown-type+))
          (not (contains-syntax-p type)))
     (find type slots
           :key 'slot-definition-type
           :test [subtypep (normalized-type-for* !2) !1]))))

(defgeneric check-types (syntax)

  (:method (syntax)
    #+nil
    (when (eq (persistent-type-of syntax) +unknown-type+)
      (warn "No type found: ~S" (unparse-query-syntax syntax)))
    (values))

  (:method ((form compound-form))
    (mapc #'check-types (operands-of form))
    (call-next-method))

  (:method ((access slot-access))
    (when (null (slot-of access))
      (slot-not-found-warning access (slots-for-slot-access access)))))

(defun slot-not-found-warning (access slots)
  (flet ((qualified-name-of (slot)
           (concatenate-symbol (class-name (persistent-slot-definition-class slot))
                               ":"
                               (slot-definition-name slot))))
    (if slots
        (warn 'ambiguous-slot-warning
              :accessor (accessor-of access)
              :access-type (persistent-type-of access)
              :arg-type (persistent-type-of (arg-of access))
              :slot-names (mapcar #'qualified-name-of slots))
        (warn 'slot-not-found-warning
              :accessor (accessor-of access)
              :access-type (persistent-type-of access)
              :arg-type (persistent-type-of (arg-of access))))))

;;;----------------------------------------------------------------------------
;;; Type utils
;;;

(defun type-syntax->type (type)
  (if (and (literal-value-p type)
           (typep (value-of type) 'persistent-class))
      (value-of type)
      type))

(defun subtypep* (sub super)
  (if (or (eq sub +unknown-type+)
          (eq super +unknown-type+)
          (contains-syntax-p sub)
          (contains-syntax-p super))
      (values nil nil)
      (subtypep sub super)))

(defun normalized-type-for* (type)
  (if (or (eq type +unknown-type+)
          (contains-syntax-p type))
      type
      (normalized-type-for type)))

(defun has-default-type-p (syntax)
  (eq (persistent-type-of syntax)
      (funcall (slot-definition-initfunction
                (find-slot (class-name (class-of syntax)) 'persistent-type)))))

(defun contains-syntax-p (type)
  (or (typep type 'syntax-object)
      (and (consp type)
           (some #'contains-syntax-p type))))

(defun maybe-null-subtype-p (type)
  (or (eq type +unknown-type+)
      (bind (((:values normalized-type null-subtype-p unbound-subtype-p) (destructure-type type)))
        (declare (ignore normalized-type unbound-subtype-p))
        null-subtype-p)))

(defun ensure-null-subtypep (type)
  (if (null-subtype-p type)
      type
      `(or null ,type)))



(defgeneric backquote-type-syntax (type)
  (:documentation "Generates a type expression that evaluates to the type.")

  (:method ((self-evaluating t))
           self-evaluating)
  
  (:method ((class persistent-class))
           class)

  (:method ((type-name symbol))
           `(quote ,type-name))

  (:method ((type syntax-object))
           type)

  (:method ((combined-type list))
           `(list
             ',(first combined-type)
             ,@(mapcar 'backquote-type-syntax (rest combined-type)))))

;; TODO: is this the same as the one in alexandria
;; TODO: eliminate this function if yes
#+nil
(defun type= (type-1 type-2)
  (or (equalp type-1 type-2)
      (and (or (persistent-class-p type-1) (symbolp type-1))
           (or (persistent-class-p type-2) (symbolp type-2))
           (eq (find-persistent-class* type-1)
               (find-persistent-class* type-2)))))

(def function simplify-persistent-class-type* (type)
  (prog1-bind simplified-type (simplify-persistent-class-type type)
    (unless (contains-syntax-p simplified-type)
      (setf simplified-type (canonical-type-for simplified-type)))))

(defgeneric simplify-persistent-class-type (type)

  (:method ((class persistent-class))
           class)

  (:method ((type-name symbol))
           (bind ((class (find-class type-name #f)))
             (typecase class
               (persistent-class class)
               (otherwise type-name)))) ; FIXME signal error?

  (:method ((type syntax-object)) ;; type unknown at compile time
           type)

  (:method ((combined-type list))
           (if (contains-syntax-p combined-type)
               ;; delay until run-time
               combined-type
               ;; and/or/not types
               (case (car combined-type)
                 ((or and join)
                  (bind  ((operands (remove-duplicates (mapcar #'simplify-persistent-class-type
                                                               (rest combined-type))
                                                       :test #'type=)))
                    (if (length= 1 operands)
                        (first operands)
                        (cons (car combined-type) operands))))
                 (not
                  `(not ,(simplify-persistent-class-type (second combined-type))))
                 (t
                  (error "Unsupported type constructor in ~A" combined-type))))))

