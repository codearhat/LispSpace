;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;; Query frontend

(defmacro select (&whole select-form (&rest select-list) &body clauses &environment env)
  "Selects object from the model.

  Syntax:

     select [<options>] <select-list> <from clause> [<where-clause>] [<group-by-clause> [<having-clause>]] [<order-by-clause>] [<offset-clause>] [<limit-clause>]

     <options>:         (&key result-type flatp uniquep)
     <select-list>:     (<expr>*)
     <from-clause>:     (<variable-spec>*)
     <variable-spec>:   <symbol> | (<symbol> <type-expr>)
     <where-clause>:    (where <bool-expr>)
     <group-by-clause>: (group-by <expr>*)
     <having-clause>:   (having <bool-expr>)
     <order-by-clause>: (order-by <order-spec>*)
     <order-spec>:      :ascending|:descending <expr>
     <offset-clause>:   (offset <integer>)
     <limit-clause>:    (limit <integer>)

  Semantics:

     Analogous to SQL:
       First the cartesian product of the set of objects specified by the FROM clause
       is created. Then the product is filtered by the WHERE clause and projected
       according to the SELECT-LIST. Finally the result is sorted according to the 
       ORDER-BY clause.

     Options may modify the result:

     result-type: (member 'list 'scroll)
        If the value is 'scroll then the result of the query returned as an instance
        of the 'scroll class. If the value is 'list the the result is a list.
        Default is 'list.

     flatp: generalized-boolean
        If true and the result-type is 'list then result is a flattened list, i.e. the 
        select list expressions are appended rather than added to the result.
        Default is true for one element select lists, false otherwise.

     uniquep: generalized-boolean
        If true then the value of the select list will not be added to the result,
        when it is equal to a previously seen value.

     prefetch-mode: (member :none :accessed :all)
        Determines which slots filled in the cached objects returned by the query.
        If :none then only the oid is filled in,
        if :accessed then only the slots that are stored in accessed tables are filled in,
        if :all then all slots are filled in.
        Default is :accessed.

  Example:

     (let ((yesterday (day-before-today)))
       (select ((name-of topic) message)
         (from (topic topic) message)
         (where (and (typep message 'message)
                     (eq (topic-of message) topic)
                     (after (date-of message) yesterday)))))"
  (declare (ignore select-list clauses))
  (bind ((lexical-variables (remove-duplicates (collect-variables-in-lexenv env))))
    (if (and (consp (second select-form))
             (keywordp (first (second select-form)))
             (getf (second select-form) :compile-at-macroexpand))
        `(progn ,@(alexandria:parse-body (cddr (compile-query (make-query select-form lexical-variables)))))
        `(execute-query
          (make-query ',select-form ',lexical-variables)
          ,@lexical-variables))))

(def walker select
  (let ((lexical-variables (remove-duplicates
                            (append (collect-variables-in-lexenv (walk-environment/lexical-environment -environment-))
                                    (iter (for (name type) :in (walk-environment/variables -environment-))
                                          (case type
                                            (:variable (collect name))))))))
    (walk-form `(execute-query
                 (make-query ',-form- ',lexical-variables)
                 ,@lexical-variables)
               :parent -parent-
               :environment -environment-)))

(defmacro purge (&whole purge-form (&rest purge-list) &body clauses &environment env)
  "TODO"
  (declare (ignore purge-list clauses))
  (bind ((lexical-variables (remove-duplicates (collect-variables-in-lexenv env))))
    `(execute-query
      (make-query ',purge-form ',lexical-variables)
      ,@lexical-variables)))

(defmacro update (&whole update-form (&rest update-list) &body clauses &environment env)
  "(update (instance user)
     (set (name-of user) \"JD\")
     (from ...)
     (where (string= (name-of user) \"BB\")))"
  (declare (ignore update-list clauses))
  (bind ((lexical-variables (remove-duplicates (collect-variables-in-lexenv env))))
    `(execute-query
      (make-query ',update-form ',lexical-variables)
      ,@lexical-variables)))

(defmacro simple-select (options variable &body body)
  (bind ((variable-specification
          (typecase variable
            (null '-instance-)
            (symbol `(-instance- ,variable))
            (t variable)))
         (variable-name (first (ensure-list variable-specification))))
    `(select ,options (,variable-name)
      (from ,variable-specification)
      ,@body)))

(defmacro select-first-matching-instance (&optional variable &body body)
  `(let ((scroll (simple-select (:result-type scroll :flatp #t) ,variable ,@body)))
    (when (> (element-count scroll) 0)
      (setf (page-size scroll) 1)
      (first-page! scroll)
      (elt (elements scroll) 0))))

(defmacro select-last-matching-instance (&optional variable &body body)
  `(let ((scroll (simple-select (:result-type scroll :flatp #t) ,variable ,@body)))
    (when (> (element-count scroll) 0)
      (setf (page-size scroll) 1)
      (last-page! scroll)
      (elt (elements scroll) 0))))

(defun select-similar-assert-for (type rest)
  (if rest
      (bind ((class (ensure-finalized (find-class type))))
        `(and ,@(iter (for (initarg value) on rest by 'cddr)
                      (collect `(equal (,(reader-name-of
                                          (find initarg (class-slots class)
                                                :key [first (slot-definition-initargs !1)]))
                                        -instance-)
                                 ,value)))))
      #t))

(defmacro select-similar-instance (type &rest rest &key &allow-other-keys)
  `(select-instance (-instance- ,type)
    (where ,(select-similar-assert-for type rest))))

(defmacro select-similar-instances (type &rest rest &key &allow-other-keys)
  `(select-instances (-instance- ,type)
    (where ,(select-similar-assert-for type rest))))

(defmacro select-instance (&optional variable &body body)
  `(let ((result (simple-select (:flatp #t) ,variable ,@body (limit 2))))
     (assert (<= (length result) 1) nil "Query returned multiple result.")
     (first result)))

(defmacro select-instances (&optional variable &body body)
  "Select objects using one variable and collect the values of that variable based upon a set of asserts."
  `(simple-select (:result-type list) ,variable ,@body))

(defmacro select-the-only-one (&whole select-form (&rest select-list) &body clauses)
  (declare (ignore clauses select-list))
  `(let ((result (select (:flatp #t) ,@(rest select-form) (limit 2))))
     (assert (<= (length result) 1) nil "Query returned multiple result.")
     (assert (not (zerop (length result))) nil "Query did not return anything.")
     (first result)))

;;;;;;
;;; Execute and compile

(defgeneric execute-query (query &rest lexical-variable-values)
  (:documentation "Executes the query with the given variable values, compiles the query when needed."))

(defgeneric compile-query (query)
  (:documentation "Compiles the query to lisp code that executes the query."))

;;;;;;
;; Query builder interface

(defgeneric make-query (select-form &optional lexical-variables)
  (:documentation
   "Creates a query object from the SELECT-FORM.
When the SELECT-FORM is NIL, an empty query created which can be modified by
ADD-LEXICAL-VARIABLE, ADD-QUERY-VARIABLE, ADD-ASSERT and ADD-COLLECT"))

(defgeneric add-lexical-variable (query variable)
  (:documentation
   "Add a lexical variable named VARIABLE to the QUERY.
Lexical variables can be referenced in the asserts and collects of the query and their
values are passed to EXECUTE-QUERY in the order they are added to the QUERY."))

(defgeneric add-query-variable (query variable)
  (:documentation
   "Add a query variable named VARIABLE to the QUERY.
Query variables can be referenced in the asserts and collects of the QUERY."))

(defgeneric add-assert (query condition)
  (:documentation
   "Add an assert for the CONDITION form to the QUERY."))

(defgeneric add-collect (query expression)
  (:documentation
   "Add a collect for the EXPRESSION form to the QUERY."))

(defgeneric add-group-by (query expression)
  (:documentation
   "Add a group-by clause specified by EXPRESSION to the QUERY."))

(defgeneric add-having (query expression)
  (:documentation
   "Add a having condition specified by EXPRESSION to the QUERY."))

(defgeneric add-order-by (query expression &optional direction)
  (:documentation
   "Add an order-by clause specified by EXPRESSION and DIRECTION to the QUERY."))

(defgeneric set-order-by (query expression &optional direction)
  (:documentation
   "Set an order-by clause specified by EXPRESSION and DIRECTION to the QUERY."))

;;;;;;
;; Association end querying

(defmethod compute-association-end-query ((association-end persistent-association-end-effective-slot-definition))
  (prog1-bind query (make-query nil)
    (add-query-variable query '-associated-instance-)
    (add-lexical-variable query '-instance-)
    (add-assert query `(typep -associated-instance- ',(class-name (associated-class-of association-end))))
    (let ((other-association-end (other-association-end-of association-end)))
      (if (eq (cardinality-kind-of other-association-end) :1)
          (add-assert query `(eq -instance- (,(reader-name-of other-association-end) -associated-instance-)))
          (add-assert query `(member -associated-instance- (,(reader-name-of association-end) -instance-)))))
    (let ((type (slot-definition-type association-end)))
      (if (ordered-set-type-p type)
          (add-order-by query (list (reader-name-of (find-slot (associated-class-of association-end) (third type)))
                                    '-associated-instance-))))
    (add-collect query '-associated-instance-)))
