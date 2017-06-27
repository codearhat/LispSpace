;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;
;;;; The executable query represented by a plan tree.
;;;; Lisp code that executes the query is generated from the plan.
;;;;

(defcclass* plan-node ()
  ((query)
   (binder nil :type function :documentation "Function that creates a list of bindings
 when called with the name of the input record.")))

(defcclass* list-result-node (plan-node)
  ((list :type list))
  (:documentation "Creates a result-set from a constant list."))

(defcclass* sql-query-node (plan-node)
  ((result-type)
   (distinct nil)
   (columns)
   (tables)
   (where nil)
   (group-by nil)
   (having nil)
   (order-by nil)
   (offset nil)
   (limit nil))
  (:documentation "Creates a result-set from the records returned by an SQL query."))

(defcclass* unary-operation-node (plan-node)
  ((binder (compute-as (binder-of (input-of -self-))))
   (input :type plan-node))
  (:documentation "Base class for transformer nodes with one input."))

(defcclass* filter-operation (unary-operation-node)
  ((condition :documentation "A boolean expression."))
  (:documentation "Filters the result-set by a boolean condition."))

(defcclass* projection-operation (unary-operation-node)
  ((values :type list :documentation "List of expressions."))
  (:documentation "Computes a function of the input record."))

(defcclass* sort-operation (unary-operation-node)
  ((sort-spec :type list :documentation "List of :ascending/:descending and expression pairs.")))

(defcclass* unique-operation (unary-operation-node)
  ()
  (:documentation "Make the records of the result-set unique. (Using EQUAL)"))

(defcclass* group-operation (unary-operation-node)
  ((group-by :documentation "Grouping expressions.")
   (collected-expressions :documentation "Collected expressions, each expression depends on the grouping expressions."))
  (:documentation "Groups records and computes aggregate functions."))

(defcclass* limit-operation (unary-operation-node)
  ((offset :documentation "Integer expression or NIL.")
   (limit :documentation "Integer expression or NIL."))
  (:documentation "Limits the range of the records by offset/limit."))

(defcclass* conversion-operation (unary-operation-node)
  ((result-type)
   (flatp))
  (:documentation "Converts the result-set to the expected result type."))

(defcclass* delete-operation (unary-operation-node)
  ((variables))
  (:documentation "Makes the instances assigned to VARIABLES in the input result-set transient."))

(defcclass* update-operation (unary-operation-node)
  ((variable :documentation "The query variable containing the instance to be updated.")
   (place-value-pairs :documentation "The list of accessor and value expression pairs.")
   (slots-to-update (compute-as (compute-slots-to-update -self-)))
   (column-value-pairs (compute-as (compute-column-value-pairs -self-))))
  (:documentation "Updates the slots of VARIABLE with the specified values."))


;;; TODO: union,intersection,difference,product,join

(defmethod print-object ((node plan-node) stream)
  (princ (class-name (class-of node)) stream)
  (pprint-newline :mandatory stream))

(defmethod print-object ((node unary-operation-node) stream)
  (call-next-method)
  (pprint-indent :current 2 stream)
  (print-object (input-of node) stream))

;;;
;;; Interface for query compiler and mapping
;;;
(def function query-to-lisp-form (query)
  (compile-plan (optimize-plan (generate-plan query))))

(def method syntax-to-sql ((subselect subselect))
  (bind ((plan (optimize-plan (generate-plan subselect))))
    (unless (typep plan 'sql-query-node)
      (error "Subselect cannot be mapped to lisp: ~S." subselect))
    (with-slots (query result-type distinct columns tables where group-by having order-by offset limit) plan
      (sql-subquery
        :query
        (sql-select
          :distinct distinct
          :columns columns
          :tables  tables
          :where where
          :group-by group-by
          :having having
          :order-by order-by
          :offset offset
          :limit limit)))))

;;;
;;; Generate initial plan for the query.
;;;

(defgeneric generate-plan (query)
  (:documentation "Generates a PLAN for the QUERY.")

  (:method :around ((query query))
           (aprog1 (call-next-method)
             (perec.dribble "Initial plan:~%~<~S~:>" it)))

  (:method ((query query))

           (case (action-of query)
             (:collect
                 (if (contradictory-p query)
                     (add-conversion
                      (generate-list-result-set nil query)
                      query)
                     (add-conversion
                      (add-limit
                       (add-unique
                        (add-projection
                         (add-sorter
                          (add-having-filter
                           (add-grouping
                            (add-where-filter
                             (generate-sql-query query)
                             query)
                            query)
                           query)
                          query)
                         query)
                        query)
                       query)
                      query)))
             (:purge
                 (if (contradictory-p query)
                     nil
                     (add-lisp-delete
                      (add-where-filter
                       (generate-sql-query query)
                       query)
                      query)))
             (:update
                 (add-lisp-update
                  (add-where-filter
                   (generate-sql-query query)
                   query)
                  query)))))

(defun contradictory-p (query)
  (is-false-literal
   (simplify-boolean-syntax
    (make-macro-call :macro 'and :args (asserts-of query)))))

(defun generate-list-result-set (list query)
  (make-instance 'list-result-node
                 :query query
                 :binder (query-variable-binder query)
                 :list list))

(defun generate-sql-query (query)
  (bind ((tables (sql-table-references-for query)))
    (if tables
        (bind ((instance (make-instance 'sql-query-node
                                        :query query
                                        :binder (query-variable-binder query)
                                        :result-type (result-type-of query)
                                        :distinct (uniquep query)
                                        :columns (sql-select-list-for query)
                                        :tables  tables)))
          (dolist (variable (query-variables-of query))
            (when (joined-variable-p variable)
              (add-sql-where-conditions
               instance
               (list (sql-join-condition-for-joined-variable variable)))))
          instance)
        (generate-list-result-set nil query))))

(defun add-where-filter (input query)
  (bind ((asserts (asserts-of query)))
    (if asserts
       (make-instance 'filter-operation
                      :query query
                      :input input
                      :condition `(and ,@asserts))
       input)))

(defun add-grouping (input query)
  (bind ((group-by (group-by-of query))
         (collects (collects-of query))
         ((:values aggregates non-aggregated-variables)
          (collect-aggregate-calls collects group-by))) ;; FIXME order-by,having
    (when (and aggregates non-aggregated-variables)
      (error "Collect clause (~S) not compatible with the group-by clause (~S)"
             collects group-by))
    (if (or group-by aggregates)
        (make-instance 'group-operation
                       :query query
                       :binder (field-binder aggregates) ;; TODO expressions above this node
                       :input input                         ;; (in sort/projection)
                       :group-by group-by                   ;; could refer only these variables
                       :collected-expressions aggregates)
        input)))

(defun add-having-filter (input query)
  (bind ((having (having-of query)))
    (if having
       (make-instance 'filter-operation
                      :query query
                      :input input
                      :condition `(and ,@having))
       input)))

(defun add-sorter (input query)
  (bind ((order-by (order-by-of query)))
    (if (and order-by (or (member :asc order-by)
                          (member :ascending order-by)
                          (member :desc order-by)
                          (member :descending order-by))) ;; FIXME ???
        (progn
          (make-instance 'sort-operation
                         :query query
                         :input input
                         :sort-spec order-by))
        input)))

(defun add-projection (input query)
  (make-instance 'projection-operation
                 :query query
                 :binder nil ;; no bindings needed above the projection
                 :input input
                 :values (collects-of query)))

(defun add-unique (input query)
  (if (uniquep query)
      (make-instance 'unique-operation
                     :query query
                     :input input)
      input))

(defun add-limit (input query)
  (if (or (offset-of query) (limit-of query))
      (make-instance 'limit-operation
                     :query query
                     :input input
                     :offset (offset-of query)
                     :limit (limit-of query))
      input))

(defun add-conversion (input query)
  (make-instance 'conversion-operation
                 :query query
                 :binder nil
                 :input input
                 :result-type (result-type-of query)
                 :flatp (flatp query)))

(defun add-lisp-delete (input query)
  (make-instance 'delete-operation
                 :query query
                 :binder nil
                 :variables (action-args-of query)
                 :input input))

(defun add-lisp-update (input query)
  (bind ((place-value-pairs (iter (for (place value) on (action-args-of query) by #'cddr)
                                  (collect (list place value)))))
    
    (make-instance 'update-operation
                   :query query
                   :binder nil
                   :input input
                   :variable (first (query-variables-of query))
                   :place-value-pairs place-value-pairs)))

;;;
;;; Optimize the plan
;;;

(defun optimize-plan (plan)
  (aprog1 (%optimize-plan plan)
    (perec.dribble "Improved plan:~%~<~S~:>" it)))

(defgeneric %optimize-plan (plan)

  (:method (plan-node)
    plan-node)
  
  (:method ((op unary-operation-node))
    (setf (input-of op) (%optimize-plan (input-of op)))
    op)

  (:method ((delete delete-operation))
    (let ((*suppress-alias-names* #t))  ;FIXME
      (call-next-method)))

  (:method ((conversion conversion-operation))
    (call-next-method)
    (etypecase (query-of conversion)
      (subselect
       (when (eq (result-type-of conversion) 'scroll)
         (error "Scrolls are not supported in subselects: ~S." (query-of conversion)))
       (input-of conversion))
      (query
       conversion)))

  (:method ((filter filter-operation))
    (call-next-method)
    (typecase (input-of filter)
      (sql-query-node
       ;; Move filter conditions that can be mapped to sql into the
       ;; sql select.
       (bind ((sql-query (input-of filter))
              ((:values sql-conditions lisp-conditions)
               (to-sql (rest (condition-of filter)))))
         (if (null (group-by-of sql-query))
             (add-sql-where-conditions sql-query sql-conditions)
             (add-sql-having-conditions sql-query sql-conditions))
         (if lisp-conditions
             (progn
               (setf (condition-of filter) `(and ,@lisp-conditions))
               filter)
             sql-query)))
      (t
       filter)))

  (:method ((sort sort-operation))
    (call-next-method)
    (typecase (input-of sort)
      (sql-query-node
       (bind ((sql-query (input-of sort))
              (sql-order-by (order-by-to-sql (sort-spec-of sort))))
         (if sql-order-by
             (progn
               (setf (order-by-of sql-query) sql-order-by)
               sql-query)
             sort)))
      (t
       sort)))

  (:method ((grouping group-operation))
    (call-next-method)
    (typecase (input-of grouping)
      (sql-query-node
       ;; If we are grouping the result of an sql select,
       ;; and each grouping expression is a column reference and
       ;; each collected expression can be mapped to sql,
       ;; then merge the grouping with the sql select.
       (bind ((sql-query (input-of grouping))
              ((:values sql-exprs lisp-exprs) (to-sql (collected-expressions-of grouping)))
              ((:values sql-groupby lisp-groupby) (to-sql (group-by-of grouping))))
         (if (or lisp-exprs lisp-groupby) ;; TODO check each group-by is a column-ref
             grouping
             (progn
               (setf (binder-of sql-query) (binder-of grouping)
                     (columns-of sql-query) sql-exprs
                     (group-by-of sql-query) sql-groupby)
               sql-query))))
      (t
       grouping)))

  (:method ((projection projection-operation))
    (call-next-method)
    (typecase (input-of projection)
      (sql-query-node
       ;; If no persistent object returned by the query, and the collects
       ;; can be mapped to sql, then merge the projection with the sql select.
       (bind ((sql-query (input-of projection))
              (query (query-of projection))
              (collects (collects-of query))
              ((:values sql-exprs lisp-exprs) (to-sql* collects)))
         (if (null lisp-exprs)
             (cond
               ((or (typep query 'subselect)
                    (every [or (sql-text-p !1) (has-identity-reader-p (persistent-type-of !1))] collects))
                (setf (binder-of sql-query) (field-binder collects)
                      (columns-of sql-query) sql-exprs)
                sql-query)
               ((every [not (set-type-p* (persistent-type-of !1))] collects)
                (setf (binder-of sql-query) (field-binder collects)
                      (columns-of sql-query) sql-exprs
                      (binder-of projection) (identity-binder collects))
                projection)
               (t
                projection))
             projection))) ;; all needed table is joined?
      (t
       projection)))

  (:method ((limit-op limit-operation))
    (call-next-method)
    (typecase (input-of limit-op)
      (projection-operation
       ;; swap limit and projection
       (bind ((projection (input-of limit-op)))
         (setf (input-of limit-op) (input-of projection)
               (input-of projection) limit-op)
         (%optimize-plan projection)))
      (sql-query-node
       ;; add limit/offset to sql query
       (bind ((sql-query (input-of limit-op))
              (lisp-offset (offset-of limit-op))
              (lisp-limit (limit-of limit-op))
              (sql-offset (when lisp-offset (transform-to-sql lisp-offset)))
              (sql-limit (when lisp-limit (transform-to-sql lisp-limit))))
         (when sql-offset
           (setf (offset-of sql-query) sql-offset
                 (offset-of limit-op) nil))
         (when sql-limit
           (setf (limit-of sql-query) sql-limit
                 (limit-of limit-op) nil))
         (if (or (offset-of limit-op) (limit-of limit-op))
             limit-op
             sql-query)))
      (t limit-op))))

(defun order-by-to-sql (order-by)
  (iter (for (dir expr) on order-by by 'cddr)
        (bind (((:values sort-key success) (transform-to-sql expr))
               (ordering (ecase dir
                           ((:asc :ascending) :ascending)
                           ((:desc :descending) :descending))))
          (if success
              (collect (sql-sort-spec :sort-key sort-key :ordering ordering))
              (leave)))))

(defun to-sql (list)
  (iter (for form in list)
        (bind (((:values sql success) (transform-to-sql form)))
          (if success
              (collect sql into sql-forms)
              (collect form into lisp-forms)))
        (finally (return-from to-sql (values sql-forms lisp-forms)))))

(defun to-sql* (list)
  (iter (for form in list)
        (bind ((sql (transform-to-sql* form)))
          (if sql
              (appending sql into sql-forms)
              (collect form into lisp-forms)))
        (finally (return-from to-sql* (values sql-forms lisp-forms)))))

(defun sql-text-p (expr)
  (and (function-call-p expr)
       (eq (fn-of expr) 'sql-text)))


;;;
;;; Compile plan to lisp code.
;;;

(defun compile-plan (plan)
  (aprog1 (%compile-plan plan)
    (perec.dribble "Compiled plan:~%~S" it)))


(defgeneric %compile-plan (plan)
  (:documentation "Compiles a PLAN to executable lisp code.")

  (:method ((list-node list-result-node))
    (with-slots (list) list-node
      `(make-list-result-set ',list)))

  (:method ((sql-query sql-query-node))
    (with-slots (query result-type distinct columns tables where group-by having order-by offset limit)
        sql-query
      (with-unique-names (scroll-offset scroll-limit)
        `(open-result-set
          ',result-type
          (named-lambda result-set-visitor (,scroll-offset ,scroll-limit)
            (declare (ignorable ,scroll-offset ,scroll-limit))
            ,(hu.dwim.rdbms::expand-sql-ast-into-lambda-form ;; TODO export
              (sql-select
                :distinct distinct
                :columns columns
                :tables  tables
                :where where
                :group-by group-by
                :having having
                :order-by order-by
                :offset (ecase result-type
                          (scroll (if offset
                                      (sql-+ offset (sql-literal :value (sql-unquote :form scroll-offset)
                                                                 :type (sql-numeric-type)))
                                      (sql-literal :value (sql-unquote :form scroll-offset)
                                                   :type (sql-numeric-type))))
                          (list offset))
                :limit (ecase result-type
                         (scroll (sql-literal :value (sql-unquote :form scroll-limit)
                                              :type (sql-numeric-type)))
                         (list limit)))))
          ,@(when (eq result-type 'scroll)
                  (list (hu.dwim.rdbms::expand-sql-ast-into-lambda-form
                         (sql-select
                           :columns (list (hu.dwim.rdbms::sql-count-*))
                           :tables (list (sql-derived-table
                                           :subquery (sql-subquery
                                                       :query
                                                       (sql-select
                                                         :distinct distinct
                                                         :columns columns
                                                         :tables tables
                                                         :where where
                                                         :group-by group-by
                                                         :having having
                                                         :offset offset
                                                         :limit limit))
                                           :alias (gensym "pg")))))))))))

  (:method ((filter filter-operation))
    (with-slots (input condition) filter
      (with-unique-names (row)
        (bind (((:values bindings condition) (generate-bindings input row condition)))
          `(make-filtered-result-set
            ,(%compile-plan input)
            (lambda (,row)
              (let* (,@bindings)
                ,condition)))))))

  (:method ((projection projection-operation))
    (with-slots (input values) projection
      (with-unique-names (row)
        (bind (((:values bindings values) (generate-bindings input row values)))
          `(make-mapped-result-set
            ,(%compile-plan input)
            (lambda (,row)
              (let* (,@bindings)
                ;;,(ignorable-variables-declaration variables)
                (list ,@values))))))))

  (:method ((sort sort-operation))
    (with-slots (input sort-spec) sort
      (with-unique-names (row1 row2)
        (bind (((:values bindings1 sort-spec1) (generate-bindings input row1 sort-spec))
               ((:values bindings2 sort-spec2) (generate-bindings input row2 sort-spec)))
          `(make-ordered-result-set
            ,(%compile-plan input)
            (lambda (,row1 ,row2)
              (let* (,@bindings1 ,@bindings2)
                ,(generate-comparator sort-spec1 sort-spec2))))))))

  (:method ((delta unique-operation))
    (with-slots (input) delta
      `(make-unique-result-set
        ,(%compile-plan input))))

  (:method ((gamma group-operation))
    (with-slots (input group-by collected-expressions) gamma
      (with-unique-names (row acc)
        (bind (((:values group-by-bindings group-by) (generate-bindings input row group-by))
               ((:values collect-bindings collected-expressions) (generate-bindings input row collected-expressions)))
          `(make-grouped-result-set
            ,(%compile-plan input)
            (lambda (,row)
              (declare (ignorable ,row))
              (let* (,@group-by-bindings)
                (list ,@group-by))) ;; FIXME compared with equal, not ok for local-time
            (lambda ()
              ,(aggregate-init-fn-body-for collected-expressions))
            (lambda (,row ,acc)
              (let* (,@collect-bindings)
                ,(aggregate-collect-fn-body-for acc collected-expressions)))
            (lambda (,acc)
              ,(aggregate-map-fn-body-for acc collected-expressions)))))))

  (:method ((limit-op limit-operation))
    (with-slots (input offset limit) limit-op
      `(make-limited-result-set
        ,(%compile-plan input)
        ,offset
        ,limit)))

  (:method ((conversion conversion-operation))
    (with-slots (input result-type flatp) conversion
      (ecase result-type
        (list `(to-list ,(%compile-plan input) :flatp ,flatp))
        (scroll `(to-scroll ,(%compile-plan input) :flatp ,flatp)))))

  (:method ((delete delete-operation))
    (with-slots (input variables) delete
      (etypecase input
        ;;
        ;; There are list filter conditions:
        ;;   execute the filter+delete by iterating on the objects to be deleted
        ;;   use execute :visitor because it's implemented with cursors
        (filter-operation
         (bind ((input2 (input-of input))
                (condition (condition-of input)))
           (assert (typep input2 'sql-query-node))
           (with-unique-names (row)
             (bind ((bindings (generate-bindings input row (append variables condition))))
               `(execute ,(%compile-plan input2) ; FIXME broken
                         :visitor
                         (lambda (,row)
                           (let* ,bindings ; FIXME broken
                             (when ,condition
                               ,@(mapcar
                                  (lambda (variable) `(make-transient ,variable))
                                  variables)))))))))
        ;; sql deletes
        (sql-query-node
         (assert (length= 1 variables)) ; FIXME check earlier
         (bind ((variable (first (variables-of delete)))
                (type (persistent-type-of variable))
                (tables-and-where-clauses (when (persistent-class-p type) (tables-for-delete type))))
           (if (length= 1 tables-and-where-clauses)
               ;; all data contained by one table -> simple sql delete
               (bind ((table (car (first tables-and-where-clauses)))
                      (extra-where-clause (cdr (first tables-and-where-clauses))))
                 `(execute ,(hu.dwim.rdbms::expand-sql-ast-into-lambda-form
                             (sql-delete :table (sql-table-reference-for table nil)
                                         :where (if extra-where-clause
                                                    `(and ,extra-where-clause
                                                          ,(where-of input))
                                                    (where-of input))))))
               (bind (((:values create-temporary-table deletes drop-temporary-table)
                       (sql-delete-from-tables (mapcar #'car tables-and-where-clauses)
                                               (sql-select
                                                 :columns (list (sql-id-column-reference-for variable))
                                                 :tables  (tables-of input)
                                                 :where (where-of input)))))
                 `(execute-protected
                   ,create-temporary-table
                   (list ,@deletes)
                   ,drop-temporary-table))))))))

  (:method ((update update-operation))
    (with-slots (input place-value-pairs) update
      (etypecase input
        (filter-operation
         (with-unique-names (row count)
           (bind ((filter input)
                  (sql-query (input-of filter))
                  (condition (condition-of filter))
                  (bindings (generate-bindings filter row (append place-value-pairs condition))))
             (assert (typep sql-query 'sql-query-node))
             `(let ((,count 0))
                (execute ,(%compile-plan sql-query) ; FIXME broken
                         :visitor
                         (lambda (,row)
                           (let* ,bindings ; FIXME broken
                             (when ,condition
                               (incf ,count)
                               (setf ,@(apply #'append place-value-pairs))))))
                ,count))))
        (sql-query-node
         (bind ((sql-query input)
                (variable (variable-of update))
                (class (persistent-type-of variable))
                (slots (slots-to-update-of update))
                (storage-locations (collect-storage-locations-for-updating-classes-and-slots
                                    (list* class (persistent-effective-subclasses-of class))
                                    (mapcar #'slot-definition-name slots)))
                (column-value-pairs (column-value-pairs-of update)))
           (cond
             ;; some place is not a slot access or some value cannot be mapped to sql => execute in lisp
             ((or (null storage-locations)
                  (null column-value-pairs))
              (with-unique-names (row count)
                (bind (((:values bindings place-value-pairs) (generate-bindings sql-query row place-value-pairs)))
                  `(prog1-bind ,count 0
                     (execute ,(hu.dwim.rdbms::expand-sql-ast-into-lambda-form ;; TODO export
                                (sql-select
                                  :columns (columns-of sql-query)
                                  :tables  (tables-of sql-query)
                                  :where (where-of sql-query)))
                              :visitor
                              (lambda (,row)
                                (let* ,bindings
                                  (incf ,count)
                                  (setf ,@(apply #'append place-value-pairs)))))))))
             ;; there is only one table to update => single SQL command
             ((length= 1 storage-locations)
              (bind ((storage-location (first storage-locations))
                     (table (first (tables-of storage-location)))
                     (extra-where-clause (where-of storage-location))
                     (columns (mapcar #'car column-value-pairs))
                     (sql-values (mapcar #'cdr column-value-pairs)))
                `(nth-value
                  1
                  (execute ,(hu.dwim.rdbms::expand-sql-ast-into-lambda-form
                             (if (where-of input)
                                 ;; TODO eliminate the subselect by adding a FROM clause (Postgres only)
                                 ;; to the UPDATE and using the WHERE clause of the SELECT
                                 (sql-update
                                   :table (sql-table-reference-for table nil)
                                   :columns (sql-column-references-for columns nil)
                                   :values sql-values
                                   :where (sql-in
                                           (sql-id-column-reference-for table)
                                           (sql-subquery
                                             :query
                                             (sql-select
                                               :columns (list (sql-id-column-reference-for variable))
                                               :tables  (tables-of input)
                                               :where (where-of input)))))
                                 (sql-update
                                   :table (sql-table-reference-for table nil)
                                   :columns (sql-column-references-for columns nil)
                                   :values sql-values
                                   :where extra-where-clause)))))))
             (t
              ;; multiple tables needs to be updated => select the ids first into a temporary table,
              ;; then one UPDATE command for each table
              ;; (FIXME ordering issues when the new values refers the the updated columns)
              (bind (((:values create-temporary-table select-count updates drop-temporary-table)
                      (sql-update-tables storage-locations
                                         column-value-pairs
                                         (sql-select
                                           :columns (list (sql-id-column-reference-for variable))
                                           :tables  (tables-of input)
                                           :where (where-of input)))))
                `(unwind-protect
                      (progn
                        ,(when create-temporary-table `(execute ,create-temporary-table))
                        ,@(mapcar (lambda (update) `(execute ,update)) updates)
                        (elt (elt (execute ,select-count) 0) 0))
                   ,(when drop-temporary-table `(execute ,drop-temporary-table))))))))))))

;;;
;;; Helpers
;;;
(deftype syntax* ()
  '(or cons syntax-object))

(deftype binder ()
  '(function
    (symbol  ; name of the 'row' variable
     symbol  ; name of the 'index' variable
     syntax*) ; expressions that use the bindings
    ;; ->
    (values
     list       ; list of let-bindings (updating the index variable)
     syntax*))) ; expressions in which the bound expressions are substituted by the variables

(defun binder-append (binder1 binder2)
  (lambda (row index referenced-by)
    (bind (((:values bindings1 expr1)
            (funcall binder1 row index referenced-by))
           ((:values bindings2 expr2)
            (funcall binder2 row index expr1)))
      (values
       (append bindings1 bindings2)
       expr2))))

(def function generate-bindings (node row referenced-by)
  (bind ((index (gensym "I"))
         (binder (binder-of node))
         ((:values bindings exprs) (funcall binder row index referenced-by)))
    (values
     `((,index 0) ,@bindings)
     exprs)))

(defun generate-lexical-variable-name (expr)
  (if (function-call-p expr)
      (gensym (symbol-name (fn-of expr)))
      (gensym "VAR")))

(defun generate-lexical-variable-names (exprs)
  (mapcar #'generate-lexical-variable-name exprs))

(defun identity-binder (exprs)
  (lambda (row i referenced-by)
    (bind ((names (generate-lexical-variable-names exprs)))
      (values
       (mapcar (lambda (name) `(,name (prog1 (elt ,row ,i) (incf ,i))))
               names)
       referenced-by))))

(defun field-binder (exprs)
  (lambda (row i referenced-by)
    (bind ((names (generate-lexical-variable-names exprs))
           (substitutions (mapcar #'cons exprs names)))
      (values
       (iter (for name in names) ;; TODO generate binding for referenced exprs only
             (for expr in exprs)
             (bind ((reader-and-count (compute-column-reader (persistent-type-of expr))))
               (etypecase reader-and-count
                 (syntax-object (collect `(,name (bind (((reader . column-count) ,reader-and-count)) (prog1 (funcall reader ,row ,i) (incf ,i column-count))))))
                 (cons (collect `(,name (prog1 (funcall ',(car reader-and-count) ,row ,i) (incf ,i ,(cdr reader-and-count)))))))))
       (substitute-syntax referenced-by substitutions)))))

(defun or-null-identity-reader (rdbms-values index)
  (acase (elt rdbms-values index)
    (:null nil)
    (t it)))

(defun reverse-columns (reader column-count)
  (lambda (rdbms-values index)
    (funcall reader (nreverse (subseq rdbms-values index (+ index column-count))) 0)))

(defcfun (compute-column-reader :memoize-test-fn equalp :computed-in computed-universe/perec) (type)
  (cond
    ((eq type +unknown-type+)
     (cons #'or-null-identity-reader 1)) ;; FIXME unsafe
    ((contains-syntax-p type)
     (make-function-call :fn 'compute-column-reader :args (list (backquote-type-syntax type))))
    ((~persistent-class-type-p type)
     (cons #'object-reader +oid-column-count+))
    (t (bind ((mapping (compute-mapping (canonical-type-for type)))
              (reader (reader-of mapping))
              (column-count (length (rdbms-types-of mapping))))
         ;; FIXME columns generated in reversed order for tagged types
         (if (tagged-p mapping)
             (cons (reverse-columns reader column-count) column-count)
             (cons reader (length (rdbms-types-of mapping))))))))

(defun has-identity-reader-p (type)
  (and (not (set-type-p* type))
       (bind ((form (compute-column-reader type)))
         (and (consp form)
              (member (car form) (list 'identity-reader #'identity-reader) :test #'equal)))))

(defun ~persistent-class-type-p (type)
  "KLUDGE because (subtypep '(and persistent-class-1 persistent-class-2) 'persistent-object) does not work."
  (flet ((type-equal (type-1 type-2)
           (bind (((:values subtype-p-1 valid-p-1) (subtypep type-1 type-2))
                  ((:values subtype-p-2 valid-p-2) (subtypep type-2 type-1)))
             (values (and subtype-p-1 subtype-p-2)
                     (and valid-p-1 valid-p-2)))))
    (type-equal type (canonical-type-for `(and ,type persistent-object)))))

(defun query-variable-binder (query)
  (query-variable-binder2 (query-variables-of query)
                          (prefetch-mode-of query)))

(defun query-variable-binder2 (variables &optional (prefetch-mode :accessed))
  (lambda (row i referenced-by)
    (bind ((referenced-variables (collect-query-variables referenced-by))
           (names (mapcar (lambda (variable) (gensym (symbol-name (name-of variable)))) variables))
           (substitutions (mapcar #'cons variables names)))
      (values
       (iter (with skipped-columns = 0)
             (for variable in variables)
             (for name in names)
             (for type = (backquote-type-syntax (persistent-type-of variable)))
             (for slots = (prefetched-slots-for variable prefetch-mode))
             (for column-counts = (mapcar #'column-count-of slots))
             (for total-column-count = (reduce '+ column-counts :initial-value +oid-column-count+))
             (cond ((not (member variable referenced-variables))
                    (incf skipped-columns total-column-count))

                   ((zerop skipped-columns)
                    (collect
                        `(,name (prog1
                                    (cache-instance-with-prefetched-slots ,row ,i ,type ',slots ',column-counts)
                                  (incf ,i ,total-column-count)))))
                   (t
                    (collect
                        `(,name (prog1
                                    (cache-instance-with-prefetched-slots
                                     ,row (+ ,i ,skipped-columns) ,type ',slots ',column-counts)
                                  (incf ,i ,(+ total-column-count skipped-columns)))))
                    (setf skipped-columns 0))))
       (substitute-syntax referenced-by substitutions)))))

(defun ignorable-variables-declaration (variables)
  `(declare (ignorable ,@(mapcar 'name-of variables))))

(defun generate-comparator (sort-spec-1 sort-spec-2)
  (bind ((lessp (ecase (first sort-spec-1)
                  ((:asc :ascending) 'lessp)
                  ((:desc :descending) 'greaterp)))
         (expr1 (second sort-spec-1))
         (expr2 (second sort-spec-2)))
    (if (null (cddr sort-spec-1))       ; last
        `(,lessp ,expr1 ,expr2)
        (with-unique-names (obj1 obj2)
          `(let ((,obj1 ,expr1)
                 (,obj2 ,expr2))
            (or
             (,lessp ,obj1 ,obj2)
             (and
              (equal ,obj1 ,obj2)
              ,(generate-comparator (cddr sort-spec-1) (cddr sort-spec-2)))))))))

(defun check-aggregate-calls (exprs)
  "Returns true if some expression contains a call to an aggregate function.
If true then all query variables must be under some aggregate call."
  (some 'aggregate-expression-p exprs))

(defun collect-aggregate-calls (expr group-by)
  "Returns the list of aggregate function calls in EXPR and the list of non-aggregated
 query variables."
  (bind ((result (syntax-fold
                  expr
                  (lambda (node)
                    (cons (when (and (query-variable-p node)
                                     (not (member node group-by :test 'syntax=)))
                            (list node))
                          (when (or (and (function-call-p node)
                                         (aggregate-function-name-p (fn-of node)))
                                    (member node group-by :test 'syntax=))
                            (list node))))
                  (lambda (parent &rest children)
                    (if (cdr parent)
                        (cons nil (cdr parent))
                        (cons (reduce #'union (mapcar #'car children) :initial-value (car parent))
                              (reduce #'union (mapcar #'cdr children) :initial-value nil)))))))
    (values (cdr result) (car result))))

(defun collect-query-variables (syntax)
  (syntax-fold
   syntax
   (lambda (node)
     (when (query-variable-p node)
       (list node)))
   (lambda (parent &rest children)
     (declare (ignore parent))
     (if (length= 1 children)
         (first children)
         (reduce #'union children :initial-value nil)))))

(defun aggregate-function-call-p (expr)
  (and (function-call-p expr)
       (aggregate-function-name-p (fn-of expr))))

(defun aggregate-init-fn-body-for (exprs)
  `'(,@(mapcar
        (lambda (expr)
          (when (aggregate-function-call-p expr)
              (funcall (aggregate-function-initial-state (aggregate-function-for (fn-of expr))))))
        exprs)))

(defun aggregate-collect-fn-body-for (acc-var exprs)
  `(list
    ,@(mapcar
       (lambda (expr)
         (if (aggregate-function-call-p expr)
             `(funcall
               ,(aggregate-function-accumulate (aggregate-function-for (fn-of expr)))
               ,(first (args-of expr)) (pop ,acc-var)) ;; FIXME handle unbound slot access
             `(progn
               (pop ,acc-var)
               ,expr)))
       exprs)))

(defun aggregate-map-fn-body-for (acc-var exprs)
  `(vector
    ,@(mapcar
       (lambda (expr)
         (if (aggregate-function-call-p expr)
             `(funcall
               ,(aggregate-function-extract (aggregate-function-for (fn-of expr)))
               (pop ,acc-var))
             `(pop ,acc-var)))
       exprs)))

(defun add-sql-where-conditions (sql-query conditions)
  (when conditions
    (bind ((where (where-of sql-query)))
      (if (null where)
          (progn
            (setf (where-of sql-query) (apply 'sql-and conditions)))
          (progn
            (assert (and (typep where 'sql-n-ary-operator) (string= (hu.dwim.rdbms::name-of where) "AND")))
            (appendf (hu.dwim.rdbms::expressions-of where) conditions))))))

(defun add-sql-having-conditions (sql-query conditions)
  (when conditions
    (bind ((having (having-of sql-query)))
      (if (null having)
          (progn
            (setf (having-of sql-query) (apply 'sql-and conditions)))
          (progn
            (assert (and (typep having 'sql-n-ary-operator) (string= (hu.dwim.rdbms::name-of having) "AND")))
            (appendf (hu.dwim.rdbms::expressions-of having) conditions))))))

(def function compute-slots-to-update (update-operation)
  (iter (for place :in (mapcar #'first (place-value-pairs-of update-operation)))
        (if (and (slot-access-p place)
                 (slot-of place))
            (collect (slot-of place))
            (return nil))))

(def function compute-column-value-pairs (update-operation)
  "Note: assumes that the effective slot's type does not change in the inheritance chain."
  (iter outer
        (for slot :in (slots-to-update-of update-operation))
        (for value :in (mapcar #'second (place-value-pairs-of update-operation)))
        (for column-names = (column-names-of slot))
        (for sql-values = (reverse (transform-to-sql* value)))  ; FIXME tag is the first column
        (unless sql-values (leave))                             ; but transform-to-sql* returns
        (unless (length= column-names sql-values) (leave))      ; the tag as the second value
        (iter (for column-name :in column-names)
              (for sql-value :in sql-values)
              (in outer (collect (cons column-name sql-value))))))

(defun tables-for-delete (class)
  "Returns the tables and where clauses, where instances of CLASS are stored."
  (bind ((classes (delete-if #'abstract-p (list* class (persistent-effective-subclasses-of class))))
         (tables (reduce #'union (mapcar #'data-tables-of classes)))
         (where-clause (make-class-id-matcher-where-clause classes)))
    (mapcar
     (lambda (table)
       (cons table
             (etypecase table
               (class-primary-table
                (when (set-difference (stored-persistent-classes-of table) classes)
                  where-clause))
               (association-primary-table
                nil))))
     tables)))