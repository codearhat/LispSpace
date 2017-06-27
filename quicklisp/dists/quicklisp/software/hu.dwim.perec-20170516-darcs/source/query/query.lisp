;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;(declaim-debug)

;;;
;;; Query
;;;
(defclass* query (copyable-mixin)
  ((lexical-variables
    nil
    :type (list lexical-variable))
   (query-variables
    nil
    :type (list query-variable))
   (flatp
    :type boolean)
   (uniquep
    #f
    :accessor uniquep
    :type boolean)
   (prefetch-mode
    :all
    :type (member :none :accessed :all))
   (result-type
    'list
    :type (member list scroll))
   (asserts
    nil
    :type list
    :documentation "List of conditions of assert forms.")
   (action
    :collect
    :type (member :collect :purge :update))
   (action-args
    nil
    :type list
    :documentation "List of expressions of the action form.")
   (group-by
    nil
    :type list
    :documentation "List of slot values.")
   (having
    nil
    :type list)
   (order-by
    nil
    :type list
    :documentation "Format: (:ascending <expr1> :descending <expr2> ...)")
   (offset
    nil
    :type (or null integer literal-value symbol lexical-variable sql-literal)
    :documentation "Offset of the first returned element, default is 0")
   (limit
    nil
    :type (or null integer literal-value symbol lexical-variable sql-literal)
    :documentation "Number of max. returned elements, default is all.")
   (sql-select-list
    nil)
   (sql-where
    nil)
   (sql-order-by
    nil
    :type list
    :documentation "Format: (:ascending <sql-expr-1> :descending <sql-expr-2> ...)")))

(define-copy-method copy-inner-class progn ((self query) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots lexical-variables query-variables flatp uniquep prefetch-mode result-type
                asserts action action-args group-by having order-by offset limit sql-select-list
                sql-where sql-order-by)))

(defmethod print-object ((query query) stream)
  (print-unreadable-object (query stream :type t)
    (prin1 (query-hash-key-for query) stream)))

(defun query-hash-key-for (query)
  (list (mapcar 'name-of (lexical-variables-of query)) (select-form-of query)))

(defgeneric options-of (query)
  (:method ((query query))
           (nconc
            (when (slot-boundp query 'flatp)
              (list :flatp (flatp query)))
            (when (uniquep query)
              (list :uniquep #t))
            (unless (eq (prefetch-mode-of query) :all)
              (list :prefetch-mode (prefetch-mode-of query)))
            (unless (eq (result-type-of query) 'list)
              (list :result-type (result-type-of query))))))

(defun mapc-query (fn query)
  (mapc fn (action-args-of query))
  (mapc fn (asserts-of query))
  (mapc fn (group-by-of query))
  (mapc fn (having-of query))
  (mapc [when (syntax-object-p !1) (funcall fn !1)] (order-by-of query))
  (when (offset-of query) (funcall fn (offset-of query)))
  (when (limit-of query) (funcall fn (limit-of query))))

(defun map-query (f query)
  (setf (action-args-of query) (mapcar [funcall f :action-arg !1] (action-args-of query))
        (asserts-of query) (mapcar [funcall f :assert !1] (asserts-of query))
        (group-by-of query) (mapcar [funcall f :group-by !1] (group-by-of query))
        (having-of query) (mapcar [funcall f :having !1] (having-of query))
        (order-by-of query) (iter (for (dir expr) :on (order-by-of query) :by 'cddr)
                                  (collect dir)
                                  (collect (funcall f :order-by expr)))
        (offset-of query) (when (offset-of query) (funcall f :offset (offset-of query)))
        (limit-of query) (when (limit-of query) (funcall f :limit (limit-of query))))
  query)

(defmethod flatp :around ((query query))
  (if (slot-boundp query 'flatp)
      (call-next-method)
      (<= (length (collects-of query)) 1)))

(defmethod add-lexical-variable ((query query) variable-name)
  (aprog1 (make-lexical-variable :name variable-name)
    (nconcf (lexical-variables-of query) (list it))))

(defmethod add-query-variable ((query query) variable-name)
  (aprog1 (make-query-variable :name variable-name)
    (push it (query-variables-of query))))

(defun get-query-variable-names (query)
  (mapcar 'name-of (query-variables-of query)))

(defun get-query-variable-types (query)
  (mapcar 'persistent-type-of (query-variables-of query)))

(defun add-joined-variable (query variable)
  (push variable (query-variables-of query)))

(defun get-variables (query)
  (append (query-variables-of query) (lexical-variables-of query)))

(defgeneric select-form-of (query)
  (:method ((query query))
    (flet ((optional (clause)
             (when clause (list clause))))
      (ecase (action-of query)
        (:collect
            (bind ((action-list (action-args-of query))
                   (options (options-of query))
                   (variables (get-query-variable-names query))
                   (asserts (asserts-of query))
                   (where-clause (case (length asserts)
                                   (0 nil)
                                   (1 `(where ,(first asserts)))
                                   (t `(where (and ,@asserts)))))
                   (group-by-clause (when (group-by-of query) `(group-by ,@(group-by-of query))))
                   (having-clause (case (length (having-of query))
                                    (0 nil)
                                    (1 `(having ,(first (having-of query))))
                                    (t `(having (and ,@(having-of query))))))
                   (order-by-clause (when (order-by-of query) `(order-by ,@(order-by-of query))))
                   (offset-clause (when (offset-of query) `(offset ,(offset-of query))))
                   (limit-clause (when (limit-of query) `(limit ,(limit-of query)))))
              `(select ,@(optional options) ,action-list
                       (from ,@variables)
                       ,@(optional where-clause)
                       ,@(optional group-by-clause)
                       ,@(optional having-clause)
                       ,@(optional order-by-clause)
                       ,@(optional offset-clause)
                       ,@(optional limit-clause))))
        (:purge
            (bind ((action-list (action-args-of query))
                   (options (options-of query))
                   (variables (get-query-variable-names query))
                   (asserts (asserts-of query))
                   (where-clause (case (length asserts)
                                   (0 nil)
                                   (1 `(where ,(first asserts)))
                                   (t `(where (and ,@asserts))))))
              `(purge ,@(optional options) ,action-list
                      (from ,@variables)
                      ,@(optional where-clause))))
        (:update
            (bind ((action-list (action-args-of query))
                   (options (options-of query))
                   (variables (get-query-variable-names query))
                   (asserts (asserts-of query))
                   (from-clause (when (rest variables) `(from ,@(rest variables))))
                   (where-clause (case (length asserts)
                                   (0 nil)
                                   (1 `(where ,(first asserts)))
                                   (t `(where (and ,@asserts))))))
              `(update ,@(optional options) (,(first variables))
                       (set ,@action-list)
                       ,@(optional from-clause)
                       ,@(optional where-clause))))))))


(defgeneric collects-of (query)
  (:method ((query query))
           (assert (eq (action-of query) :collect))
           (action-args-of query)))

(defgeneric (setf collects-of) (value query)
  (:method (value (query query))
           (assert (eq (action-of query) :collect))
           (setf (action-args-of query) value)))

(defmethod add-assert ((query query) condition)
  (appendf (asserts-of query) (list condition)))

(defmethod add-collect ((query query) expression)
  (appendf (collects-of query) (list expression)))

(defmethod add-group-by ((query query) expression)
  (appendf (group-by-of query) (list expression)))

(defmethod add-having ((query query) expression)
  (appendf (having-of query) (list expression)))

(defmethod add-order-by ((query query) expression &optional (direction :ascending))
  (assert (member direction '(:asc :ascending :desc :descending)))
  (nconcf (order-by-of query) (list direction expression)))

(defmethod set-order-by ((query query) expression &optional (direction :ascending))
  (assert (member direction '(:asc :ascending :desc :descending)))
  (setf (order-by-of query) (list direction expression)))

(defgeneric add-where-clause (query where-clause)
  (:method ((query query) where-clause)
           (setf (sql-where-of query)
                 (combine-with 'sql-and (sql-where-of query) where-clause))))

;;;
;;; Subselects
;;;
(define-syntax-node subselect (syntax-object query)
  ())

(def method initialize-instance :after ((instance subselect) &key)
     (setf (prefetch-mode-of instance) :none))

(defmethod options-of ((subselect subselect))
  (when (uniquep subselect)
    (list :uniquep #t)))

(def print-object subselect
    (write (select-form-of -self-)))

(def method unparse-query-syntax ((subselect subselect))
  (select-form-of subselect))

;;;
;;; Query builder
;;;
;;; TODO remove it (move to dwim?)
(defclass* query-builder (copyable-mixin)
  ((current-query-variable nil)))

(define-copy-method copy-inner-class progn ((self query-builder) copy copy-htable)
  (with-slot-copying (copy copy-htable self)
    (copy-slots current-query-variable)))

(defclass* simple-query-builder (query-builder query)
  ())

(defun preprocess-query-expression (query expression)
  (setf expression (substitute/tree (name-of (current-query-variable-of query))
                                    '*current-query-variable* expression))
  expression)

(defmethod add-query-variable ((query query-builder) variable-name)
  (setf (current-query-variable-of query) (call-next-method)))

(defmethod add-assert ((query simple-query-builder) condition)
  (call-next-method query (preprocess-query-expression query condition)))

(defmethod add-collect ((query simple-query-builder) expression)
  (call-next-method query (preprocess-query-expression query expression)))

(defmethod add-order-by ((query simple-query-builder) expression &optional (direction :ascending))
  (call-next-method query (preprocess-query-expression query expression) direction))

;;;
;;; Parse
;;;

(defun parse-query-form (form variables)
  (acond
    ((syntax-object-p form) form)
    ((and (symbolp form) (find form variables :key 'name-of)) it)
    ((and (atom form) (constantp form))
     (make-literal-value :value (if (symbolp form)
                                    (symbol-value form)
                                    form)))
    ((and (consp form) (eq (first form) 'quote))
     (make-literal-value :value (second form)))
    ((symbolp form)
     (make-dynamic-variable :name form))
    ((and (eq (first form) 'slot-value)
          (consp (third form))
          (eq (first (third form)) 'quote)
          (symbolp (second (third form))))
     (bind ((slots (persistent-effective-slots-for-slot-name (second (third form))))
            (accessor (when slots (reader-name-of (first slots))))) ; KLUDGE bad, bad, bad...
       (cond
         ((some [not (eq accessor (reader-name-of !1))] (rest slots))
          (warn "Cannot find a unique accessor by slot-name: ~S" form)
          (make-function-call :fn (first form)
                              :args (mapcar [parse-query-form !1 variables] (rest form))))
         ((null accessor)
          (warn "No accessor found for slot access: ~S" form)
          (make-function-call :fn (first form)
                              :args (mapcar [parse-query-form !1 variables] (rest form))))
         ((typep (first slots) 'persistent-association-end-effective-slot-definition)
          (make-association-end-access :accessor accessor
                                       :args (list (parse-query-form (second form) variables))))
         (t
          (make-slot-access :accessor accessor
                            :args (list (parse-query-form (second form) variables)))))))
    ((eq 'select (first form))
     (parse-subselect form variables))
    ((and (symbolp (first form)) (association-end-accessor-p (first form)))
     (make-association-end-access :accessor (first form)
                                  :args (list (parse-query-form (second form) variables))))
    ((and (symbolp (first form)) (slot-accessor-p (first form)))
     (make-slot-access :accessor (first form)
                       :args (list (parse-query-form (second form) variables))))
    ((and (symbolp (first form)) (macro-function (first form)))
     (if (expand-macro-call-p (first form))
         (parse-query-form (macroexpand-1 form)
                           variables)
         (make-macro-call :macro (first form)
                          :args (if (parse-args-p (first form))
                                    (mapcar [parse-query-form !1 variables] (rest form))
                                    (mapcar [make-unparsed-form :form !1] (rest form))))))
    ((and (symbolp (first form)) (special-operator-p (first form)))
     (make-special-form :operator (first form)
                        :operands (mapcar [make-unparsed-form :form !1] (rest form))))
    ((and (member (first form) '(static volatile)) (length= 1 (rest form)))
     (bind ((syntax (parse-query-form (second form) variables)))
       (setf (volatilep syntax) (eq (first form) 'volatile))
       syntax))
    ((member (first form) '(sum avg))
     (make-function-call :fn (first form)
                         :args (mapcar [parse-query-form !1 variables] (rest form))))
    ((and (symbolp (first form)) (fboundp (first form)))
     (make-function-call :fn (first form)
                         :args (mapcar [parse-query-form !1 variables] (rest form))))
    (t (error "Syntax error: ~S~%" form))))

(def function parse-subselect (form &optional variables)
  (bind ((lexical-variables (collect-if #'lexical-variable-p variables))
         ((&key action action-args query-variables asserts group-by having order-by offset limit options)
          (parse-query form)))
    (assert (eq action :collect))
    (assert (null options))
    (aprog1 (make-instance 'subselect
                           :lexical-variables lexical-variables
                           :query-variables query-variables
                           :asserts asserts
                           :action :collect
                           :action-args action-args
                           :group-by group-by
                           :having having
                           :order-by order-by
                           :offset offset
                           :limit limit)
      ;; Note: the outer query variables are unaccessible when parsing the subquery by will
      (parse-query-expressions it))))

(defun parse-query-expressions (query)
  (bind ((variables (get-variables query)))
    (map-query [parse-query-form !2 variables] query)))

(def function parse-query (form)

  (labels ((query-macro-expand (form)
             (if (member (first form) '(select purge update))
                 form
                 (bind (((:values form expanded-p) (macroexpand-1 form)))
                   (if expanded-p
                       (query-macro-expand form)
                       form))))
           (make-query-variables (variable-specs)
             (iter (for variable-spec in variable-specs)
                   (typecase variable-spec
                     (symbol (collect (make-query-variable :name variable-spec)))
                     (cons (collect (make-query-variable :name (car variable-spec))))
                     (otherwise (error "Symbol or symbol/type pair expected, found ~S in select: ~:W"
                                       variable-spec form)))))
           (make-asserts (where-clause variable-specs)
             (append
              (iter (for variable-spec in variable-specs)
                    (when (and (listp variable-spec) (>= (length variable-spec) 2))
                      (collect `(typep ,(first variable-spec) ',(second variable-spec)))))
              (when where-clause
                (bind ((where-cond (second where-clause)))
                  (if (and (listp where-cond) (eq 'and (car where-cond)))
                      (rest where-cond)
                      (list where-cond))))))
           (find-clause (clause-name clauses &optional (optionalp #t))
             (bind ((found-clauses (remove clause-name clauses :key #'first :test-not #'eql)))
               (case (length found-clauses)
                 (0 (unless optionalp
                      (missing-query-clause-error form clause-name)))
                 (1 (first found-clauses))
                 (t (duplicated-query-clause-error form clause-name)))))
           (check-where-clause (clause)
             (when (and clause (not (length= 1 (rest clause))))
               (malformed-query-clause-error form clause "One condition expected in WHERE clause."))
             clause)
           (check-group-by-clause (clause)
             clause)
           (check-having-clause (clause)
             (when (and clause (not (length= 1 (rest clause))))
               (malformed-query-clause-error form clause "One condition expected in HAVING clause."))
             clause)
           (check-order-by-clause (clause)
             (when clause
               (unless (evenp (length (rest clause)))
                 (malformed-query-clause-error form clause "Plist expected in the body of the ORDER-BY clause."))
               (iter (for (dir expr) on (rest clause) by #'cddr)
                     (unless (member dir '(:asc :ascending :desc :descending))
                       (malformed-query-clause-error form clause ":ASCENDING or :DESCENDING expected as sorting directions."))))
             clause)
           (check-offset-limit-clause (clause)
             (when clause
               (unless (length= 1 (rest clause))
                 (malformed-query-clause-error form clause "OFFSET/LIMIT expect one integer argument.")))
             clause)
           (parse-select (options select-list clauses)
             (remf options :compile-at-macroexpand)
             (bind ((from-clause (find-clause 'from clauses #f))
                    (where-clause (check-where-clause (find-clause 'where clauses)))
                    (group-by-clause (check-group-by-clause (find-clause 'group-by clauses)))
                    (having-clause (check-having-clause (find-clause 'having clauses)))
                    (order-by-clause (check-order-by-clause (find-clause 'order-by clauses)))
                    (offset-clause (check-offset-limit-clause (find-clause 'offset clauses)))
                    (limit-clause (check-offset-limit-clause (find-clause 'limit clauses)))
                    (query-variables (make-query-variables (rest from-clause)))
                    (asserts (make-asserts where-clause (rest from-clause)))
                    (extra-clauses (set-difference clauses
                                                   (list from-clause where-clause group-by-clause
                                                         having-clause order-by-clause offset-clause
                                                         limit-clause))))
               (when extra-clauses
                 (unrecognized-query-clause-error form (first (first extra-clauses))))

               `(:query-variables ,query-variables
                                  :asserts ,asserts
                                  :action :collect
                                  :action-args ,select-list
                                  :group-by ,(rest group-by-clause)
                                  :having ,(rest having-clause)
                                  :order-by ,(rest order-by-clause)
                                  :offset ,(second offset-clause)
                                  :limit ,(second limit-clause)
                                  :options ,options)))
           (parse-purge (options purge-list clauses)
             (remf options :compile-at-macroexpand)
             (bind ((from-clause (find-clause 'from clauses))
                    (where-clause (check-where-clause (find-clause 'where clauses)))
                    (query-variables (make-query-variables (rest from-clause)))
                    (asserts (make-asserts where-clause (rest from-clause)))
                    (extra-clauses (remove from-clause (remove where-clause clauses))))

               (when extra-clauses
                 (unrecognized-query-clause-error form (first (first extra-clauses))))

               `(:query-variables ,query-variables
                                  :asserts ,asserts
                                  :action :purge
                                  :action-args ,purge-list
                                  :options ,options)))
           (parse-update (options update-list clauses)
             (remf options :compile-at-macroexpand)
             (bind ((set-clause (find-clause 'set clauses))
                    (from-clause (find-clause 'from clauses))
                    (query-variables (make-query-variables (list* update-list (rest from-clause))))
                    (where-clause (check-where-clause (find-clause 'where clauses)))
                    (asserts (make-asserts where-clause (list* update-list (rest from-clause))))
                    (extra-clauses (set-difference clauses (list set-clause from-clause where-clause))))
               (when extra-clauses
                 (unrecognized-query-clause-error form (first (first extra-clauses))))

               `(:query-variables ,query-variables
                                  :asserts ,asserts
                                  :action :update
                                  :action-args ,(rest set-clause)
                                  :options ,options))))

    (pattern-case (query-macro-expand form)
      ((select (?and (?or nil ((?is ?k keywordp) . ?rest)) ?options) (?is ?select-list listp) .
               ?clauses)
       (parse-select ?options ?select-list ?clauses))
      ((select (?is ?select-list listp) . ?clauses)
       (parse-select nil ?select-list ?clauses))
      ((purge (?and (?or nil ((?is ?k keywordp) . ?rest)) ?options) (?is ?purge-list listp) .
              ?clauses)
       (parse-purge ?options ?purge-list ?clauses))
      ((purge  (?is ?purge-list listp) . ?clauses)
       (parse-purge nil ?purge-list ?clauses))
      ((update (?and (?or nil ((?is ?k keywordp) . ?rest)) ?options) (?is ?update-list listp) .
               ?clauses)
       (parse-update ?options ?update-list ?clauses))
      ((update  (?is ?update-list listp) . ?clauses)
       (parse-update nil ?update-list ?clauses))
      (?otherwise
       (error 'query-syntax-error
              :form form)))))




;;;
;;; Construct
;;;
(defmethod make-query ((select-form null) &optional lexical-variables)
  (prog1-bind query (make-instance 'simple-query-builder)
    (mapc [add-lexical-variable query !1] lexical-variables)))

(defmethod make-query ((form cons) &optional lexical-variables)

  (bind ((lexical-variables (mapcar [make-lexical-variable :name !1] lexical-variables))
         ((&key action action-args query-variables asserts group-by having order-by offset limit options)
          (parse-query form)))
    (apply 'make-instance 'query
           :lexical-variables lexical-variables
           :query-variables query-variables
           :asserts asserts
           :action action
           :action-args action-args
           :group-by group-by
           :having having
           :order-by order-by
           :offset offset
           :limit limit
           options)))
