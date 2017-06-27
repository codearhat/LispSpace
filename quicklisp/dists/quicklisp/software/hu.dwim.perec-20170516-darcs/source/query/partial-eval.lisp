;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(enable-pattern-reader #\M)

(def (special-variable e) *enable-partial-eval* #f)

;;;
;;; Partial eval
;;;
(defun partial-eval-query (query)
  (setf (asserts-of query)
        (mapcar [partial-eval !1 query] (asserts-of query))))


(defun partial-eval (syntax query)
  "Returns the partially evaluated SYNTAX. The SYNTAX can be a SYNTAX-OBJECT or a lisp form
 containing syntax objects. The result is always a SYNTAX-OBJECT."
  (syntax-from-value (%partial-eval-syntax syntax query) syntax))

(defgeneric %partial-eval-syntax (syntax query)
  (:documentation
   "Partially evaluates SYNTAX and returns a partially evaluated SYNTAX-OBJECT or the value
if it was fully evaluated.")

  (:method :around ((syntax syntax-object) query)
           (if (slot-boundp syntax 'volatilep)
               (bind ((*enable-partial-eval* (not (volatilep syntax))))
                 (call-next-method))
               (call-next-method)))

  (:method (syntax query)
           (%partial-eval-syntax (parse-query-form syntax (get-variables query)) query))

  (:method ((syntax syntax-object) query)
           (error "Unknown syntax: ~S~%" syntax))

  (:method ((subselect subselect) query)
    (partial-eval-query subselect)
    subselect)

  (:method ((unparsed unparsed-form) query)
           unparsed)

  (:method ((literal literal-value) query)
           (if *enable-partial-eval*
               (value-of literal)
               literal))

  (:method ((variable variable) query)
           variable)

  (:method ((variable dynamic-variable) query)
           (bind ((variable-name (name-of variable)))
             (if (and *enable-partial-eval* (boundp variable-name))
                 (symbol-value variable-name)
                 variable)))

  (:method ((call macro-call) query)
           (bind ((args (args-of call)))
             (%partial-eval-macro-call
              (macro-of call) (length args) (first args) (second args) args call query)))

  (:method ((call function-call) query)
           (bind ((args (mapcar [%partial-eval-syntax !1 query] (args-of call))))
             (%partial-eval-function-call
              (fn-of call) (length args) (first args) (second args) args call)))

  (:method ((form special-form) query)
           (%partial-eval-special-form (operator-of form) (operands-of form) form query)))

;; TODO: THESE MIGHT BE ACTUALLY OBSOLETE COMMENTS DUE TO VOLATILE/STATIC
;; TODO: partial eval should not allow partial evaluation of functions by default
;; TODO: there should be a positive list of partial evaluatable functions such as sql-* in the RDBMS package
;; TODO: some CL symbols, such as list, append, etc.
;; TODO: authorization will have to take care what could be partial evaluated and what not based on meta data on the
;; TODO: subject and related entities
;; TODO: query cache should be separated per session

(defgeneric %partial-eval-function-call (fn n-args arg-1 arg-2 args call)

  (:method (fn n-args arg-1 arg-2 args call)
           (if (and *enable-partial-eval* (notany 'syntax-object-p args))
               (apply fn args)
               (progn (setf (args-of call) (mapcar 'syntax-from-value args (args-of call)))
                      call)))

  ;; (typep query-variable t1) -> nil
  ;;    when the types t1 and (persistent-type-of query-variable) does not have common subtypes
  (:method ((fn (eql 'typep)) (n-args (eql 2)) (variable query-variable) (type persistent-class) args call)
           (let ((variable-type (persistent-type-of variable)))
             (if (and (persistent-class-p variable-type)
                      (null (intersection (adjoin type (persistent-effective-subclasses-of type))
                                          (adjoin variable-type (persistent-effective-subclasses-of variable-type)))))
                 nil
                 (call-next-method))))

  ;; (member x nil) -> nil
  ;; (member x <list>) -> (member x <list2>) where list2 contains those elements of list,
  ;;                                         that have matching type
  (:method ((fn (eql 'member)) (n-args (eql 2)) object (list list) args call)
           (bind ((type (persistent-type-of object))
                  (list (if (persistent-class-p type) (collect-if [typep !1 type] list) list)))
             (cond
               ((null list) nil)
               (t (setf args (list object list))
                  (call-next-method 'member 2 object list args call)))))

  ;; evaluating sql-text causes an error
  (:method ((fn (eql 'sql-text)) (n-args (eql 1)) string dummy args call)
    (setf (args-of call) (mapcar 'syntax-from-value args (args-of call)))
    call))

(defgeneric %partial-eval-macro-call (macro n-args arg-1 arg-2 args call query)

  (:method (macro n-args arg-1 arg-2 args call query)
           call)

  (:method ((macro (eql 'and)) n-args arg-1 arg-2 args call query)
           (%partial-eval-and/or call query))

  (:method ((macro (eql 'or)) n-args arg-1 arg-2 args call query)
           (%partial-eval-and/or call query)))

(defun %partial-eval-and/or (call query)
  (bind ((args (mapcar [%partial-eval-syntax !1 query] (args-of call))))
    (if (and *enable-partial-eval* (notany 'syntax-object-p args))
        (eval (cons (macro-of call) (mapcar 'boolean-from-generalized-boolean args)))
        (progn (setf (args-of call) (mapcar 'syntax-from-generalized-boolean args))
               (simplify-boolean-syntax call)))))

(defgeneric %partial-eval-special-form (operator args form query)
  ;; special forms (currently not evaluated, TODO) 
  (:method (operator args form query)
           form))

(defun syntax-from-value (value orig-syntax)
  (cond
    ((syntax-object-p value) value)
    ((syntax-object-p orig-syntax) (make-literal-value :value value :persistent-type (persistent-type-of orig-syntax)))
    (t (make-literal-value :value value))))

(defun syntax-from-generalized-boolean (value)
  (if (syntax-object-p value)
      value
      (make-literal-value :value (if value #t #f))))

(defun boolean-from-generalized-boolean (value)
  (assert (not (syntax-object-p value)))
  (if value #t #f))

(defun is-true-literal (syntax)
  "Returns #t if SYNTAX is a true literal as generalized boolean."
  (and (typep syntax 'literal-value)
       (not (eq (value-of syntax) #f))))

(defun is-false-literal (syntax)
  "Returns #t if SYNTAX is a false literal."
  (and (typep syntax 'literal-value)
       (eq (value-of syntax) #f)))

(defun simplify-boolean-syntax (syntax)
  "Makes the following simplifications on SYNTAX:
   (not false)                -> true
   (not true)                 -> false
   (not (not x))              -> x
   (or)                       -> false
   (or x)                     -> x
   (or x... false y...)       -> (or x... y...)
   (or x... true y...)        -> true
   (or x... (or y...) z...)   -> (or x... y... z...)
   (and)                      -> true
   (and x)                    -> x
   (and x... true y...)       -> (and x... y...)
   (and x... false y...)      -> false
   (and x... (and y...) z...) -> (and x... y... z...)

where x, y and z are arbitrary objects and '...' means zero or more occurence,
and false/true means a generalized boolean literal."
  
  (flet ((simplify-args (operator args)
           (iter (for arg in args)
                 (for simplified = (simplify-boolean-syntax arg))
                 (if (and (macro-call-p simplified) (eq (macro-of simplified) operator))
                     (appending (args-of simplified))
                     (collect simplified)))))
    (pattern-case syntax
      (#M(function-call :fn not :args (?arg))
         (bind ((arg (simplify-boolean-syntax ?arg)))
           (pattern-case arg
             (#M(function-call :fn not :args (?arg)) ?arg)
             (#M(literal-value :value #f) (make-literal-value :value #t))
             (#M(literal-value :value ?true) (make-literal-value :value #f))
             (?otherwise syntax))))
      (#M(macro-call :macro or :args ?args)
         (bind ((operands (remove-if 'is-false-literal (simplify-args 'or ?args))))
           (cond
             ((null operands) (make-literal-value :value #f))
             ((length= 1 operands) (first operands))
             ((find-if 'is-true-literal operands) (make-literal-value :value #t))
             (t (make-macro-call :macro 'or :args operands)))))
      (#M(macro-call :macro and :args ?args)
         (bind ((operands (remove-if 'is-true-literal (simplify-args 'and ?args))))
           (cond
             ((null operands) (make-literal-value :value #t))
             ((length= 1 operands) (first operands))
             ((find-if 'is-false-literal operands) (make-literal-value :value #f))
             (t (make-macro-call :macro 'and :args operands)))))
      (?otherwise syntax))))
