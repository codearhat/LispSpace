;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;----------------------------------------------------------------------------
;;; SQL mapping of expressions
;;;

;;; TODO: inherited associations mapped to column in the derived entity (missing)
;;;

(defun transform-to-sql (syntax)
  "Transforms the SYNTAX to an SQL expression. Returns the SQL expression and a success indicator."
  (bind ((sql nil)
         (success #f))
    (catch 'sql-map-failed
      (setf sql (syntax-to-sql syntax))
      (setf success #t))
    (values sql success)))

(defun transform-to-sql* (syntax)
  "Transforms the SYNTAX to a list of SQL expressions."
  (bind (result)
    (catch 'sql-map-failed
      (setf result (multiple-value-list (syntax-to-sql syntax))))
    result))

(defun sql-map-failed ()
  (throw 'sql-map-failed nil))

(defgeneric syntax-to-sql (syntax)
  (:documentation "Maps a lisp form to SQL.")
  
  (:method (syntax)
    (syntax-to-sql-literal-if-possible syntax))

  (:method ((literal literal-value))
    (literal-to-sql (value-of literal) (persistent-type-of literal) literal))

  (:method ((variable lexical-variable))
    (emit-sql-literal variable))

  (:method ((variable dynamic-variable))
    (emit-sql-literal variable))

  (:method ((variable query-variable))
    (sql-id-column-reference-for variable))

  (:method ((access slot-access))
    (slot-access-to-sql (accessor-of access) (arg-of access) access))

  (:method ((call function-call))
    (bind ((fn (fn-of call))
           (args (args-of call)))
      (function-call-to-sql fn (length args) (first args) (second args) call)))

  (:method ((call macro-call))
    (bind ((macro (macro-of call))
           (args (args-of call)))
      (macro-call-to-sql macro (length args) (first args) (second args) call))))

(defgeneric literal-to-sql (value type literal)
  (:documentation "Maps a literal value to SQL.")

  (:method (value type literal)
           (cond
             ((and (keywordp value) (eq type 'keyword)) value) ; it is a keyword arg of a call
             ((syntax-object-p type) (emit-sql-literal literal))
             (t (value->sql-literal value type (compute-type-info type))))))

(defgeneric slot-access-to-sql (accessor arg access)
  (:method (accessor arg access)
    (syntax-to-sql-literal-if-possible access))

  (:method (accessor (variable query-variable) (access slot-access))
           ;; slot accessor
           (bind ((slot (slot-of access)))
             (if (and slot (persistent-slot-p slot))
                 (sql-column-reference-for slot variable)
                 (sql-map-failed))))

  (:method (accessor (variable query-variable) (access association-end-access))
           ;; association-end accessor
           (if (association-end-of access)
               (bind ((association-end (association-end-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (if (primary-association-end-p association-end)
                        (sql-column-reference-for association-end variable)
                        (sql-subselect-for-secondary-association-end association-end variable)))
                   (:1-n
                    (if (to-one-association-end-p association-end)
                        (sql-column-reference-for association-end variable)
                        (sql-subselect-for-secondary-association-end association-end variable)))
                   (:m-n
                    (sql-subselect-for-m-n-association association-end variable))))
               (sql-map-failed))))


(defgeneric function-call-to-sql (fn n-args arg1 arg2 call)

  (:method (fn n-args arg1 arg2 call)
           (aif (get fn 'sql-mapper)
                (apply it (args-of call))
                (syntax-to-sql-literal-if-possible call)))

  ;; (member <object> (<association-end-accessor> <query-variable>))
  ;; e.g. (member m1 (messages-of topic)) --> (_m1.topic_id = _topic.id)
  (:method ((fn (eql 'member)) (n-args (eql 2)) (object query-variable) (access association-end-access) call)
           ;; member form -> join
           ;;   example:
           ;;   (member m (messages-of t)) -> m.topic_id = t.id
           (if (or (not (query-variable-p (arg-of access)))
                   (not (association-end-of access)))
               (call-next-method)
               (bind ((association-end (association-end-of access))
                      (variable (arg-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (sql-map-failed))
                   (:1-n
                    (sql-join-condition-for object variable association-end))
                   (:m-n
                    (sql-join-condition-for-m-n-association object variable association-end))))))

  ;; eq form -> join
  ;;   examples:
  ;;   (eq (topic-of message) topic) -> message.topic_id = topic.id
  ;;   (eq (wife-of man) woman)      -> man.wife_id = woman.id
  ;;   (eq (husband-of woman) man)   -> man.wife_id = woman.id
  (:method ((fn (eql 'eq)) (n-args (eql 2)) (access association-end-access) object call)

           (if (not (association-end-of access))
               (sql-map-failed)
               (bind ((association-end (association-end-of access))
                      (other-end (other-association-end-of association-end))
                      (variable (arg-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (if (primary-association-end-p association-end)
                        (call-next-method)
                        (syntax-to-sql
                         (make-function-call ;; reverse
                          :fn fn
                          :args (list (make-association-end-access :association-end other-end
                                                                   :accessor (reader-name-of other-end)
                                                                   :args (list object))
                                      variable)))))
                   (:1-n
                    (call-next-method))
                   (:m-n
                    (sql-map-failed))))))

  (:method ((fn (eql 'eq)) (n-args (eql 2)) object (access association-end-access) call)
           (if (association-end-access-p object)
               (call-next-method)
               (function-call-to-sql fn 2 access object call)))

  ;; (typep variable type)
  ;;   example:
  ;;   (typep o #<class user>) -> exists(select 1 from user u where u.id = o.id)
  (:method ((fn (eql 'typep)) (n-args (eql 2)) (variable query-variable) (type literal-value) call)
           (if (persistent-class-p (value-of type))
               (sql-exists-subselect-for-variable variable (value-of type))
               (call-next-method)))

  ;; (typep (x-of v) type)
  (:method ((fn (eql 'typep)) (n-args (eql 2)) (access association-end-access) (type literal-value) call)
    ;; TODO refine the conditions
    (if (and (association-end-of access)
             (query-variable-p (arg-of access))
             (persistent-class-p (value-of type)))
        (bind ((association-end (association-end-of access))
               (association (association-of association-end)))
          (ecase (association-kind-of association)
            (:1-1
             (if (primary-association-end-p association-end)
                 (sql-exists-subselect-for-association-end (arg-of access) association-end (value-of type))
                 (call-next-method)))
            (:1-n
             (if (to-one-association-end-p association-end)
                 (sql-exists-subselect-for-association-end (arg-of access) association-end (value-of type))
                 (call-next-method)))
            (:m-n
             (call-next-method))))
        (call-next-method)))

  ;; (null (accessor variable))

  ;; TODO NOT EXIST (subselect)
  ;;   e.g ((and (association-end-access-p expr) (not (contains-syntax-p expr)))
  ;;         `(cache-instance-with-prefetched-slots ,row ,i ,(normalized-type-for (persistent-type-of expr)) nil '(1)))
  (:method ((fn (eql 'null)) (n-args (eql 1)) (access slot-access) arg2 call)
           (bind ((slot (slot-of access)))
             (if (and slot
                      (not (typep slot 'persistent-association-end-effective-slot-definition))
                      (query-variable-p (arg-of access)))
                 (sql-slot-is-null (arg-of access) slot)
                 (call-next-method))))

  ;; (length (<n-ary-association-end-accessor> <query-var>))
  ;; e.g. (length (messages-of topic)) -->
  ;;         (select count(_m.id) from _message _m where _m.topic_id = _topic.id)
  (:method ((fn (eql 'length)) (n-args (eql 1)) (access association-end-access) arg2 call)
           (if (and (association-end-of access)
                    (query-variable-p (arg-of access)))
               (ecase (association-kind-of (association-of (association-end-of access)))
                 (:1-1
                  (sql-map-failed))
                 (:1-n
                  (sql-aggregate-subselect-for-variable
                   'sql-count
                   (association-end-of access)
                   (arg-of access)))
                 (:m-n
                  (sql-aggregate-subselect-for-m-n-association-end
                   'sql-count
                   (association-end-of access)
                   (arg-of access))))
               (call-next-method))))

(defgeneric macro-call-to-sql (macro n-args arg1 arg2 call)
  (:method (macro n-args arg1 arg2 call)
    (aif (get macro 'sql-mapper)
         (apply it (args-of call))
         (syntax-to-sql-literal-if-possible call))))

(def (function io) syntax-to-sql-literal-if-possible (syntax)
  (if (free-of-query-variables-p syntax)
      (emit-sql-literal syntax)
      (sql-map-failed)))

(defun free-of-query-variables-p (syntax)
  (typecase syntax
    (query-variable #f)
    (unparsed-form (free-of-query-variables-p (form-of syntax)))
    (compound-form (every 'free-of-query-variables-p (operands-of syntax)))
    (cons (and (free-of-query-variables-p (car syntax))
               (free-of-query-variables-p (cdr syntax))))
    (otherwise #t)))

(defun find-slot-definition (owner slot-name)
  (find-slot-by-owner-type owner (persistent-effective-slots-for-slot-name slot-name)))

(def function emit-sql-literal (syntax)
  (bind ((type (persistent-type-of syntax))
         (type-info (compute-type-info type)))
    (if type-info
        (bind ((rdbms-types (ti-rdbms-types type-info))
               (writer (ti-writer type-info)))
          (values-list
           (iter (for i index-of-sequence rdbms-types)
                 (for rdbms-type in rdbms-types)
                 (collect
                     (sql-literal :value
                                  (sql-unquote :form `(elt (funcall ,writer ,syntax) ,i))
                                  :type rdbms-type)))))
        (sql-unquote :form `(value->sql-literal ,syntax ,(backquote-type-syntax type) nil)))))

(defgeneric unbound-check-for (syntax)
  (:documentation "Returns an SQL expression that checks if the value of SYNTAX is unbound.")
  
  (:method (syntax)
    nil)

  (:method ((access slot-access))
    (bind ((type (persistent-type-of access))
           (slot (slot-of access))
           (variable (arg-of access)))
      (debug-only (assert (not (contains-syntax-p type))))
      (cond
        ((or (null slot)
             (not (query-variable-p variable))
             (eq type +unknown-type+))
         (sql-map-failed))
        ((unbound-subtype-p type)
         (check-for-rdbms-values
          (lisp-value->rdbms-equality-values (canonical-type-for type) +unbound-slot-marker+)
          (column-names-of slot)
          (column-types-of slot)
          variable))
        (t
         nil))))

  (:method ((access association-end-access))
    nil))


;; TODO needs review
(defgeneric null-check-for (syntax)
  (:documentation "Returns an SQL expression that checks if the value of SYNTAX is a unit type value (except unbound) .")
  
  (:method (syntax)
    nil)

  (:method ((literal literal-value))
    (bind ((type (persistent-type-of literal)))
      (when (eq type +unknown-type+)
        (sql-map-failed))
      (bind ((type (canonical-type-for type))
             (rdbms-values (lisp-value->rdbms-values type (value-of literal))))
        (when (eq (aref rdbms-values (1- (length rdbms-values))) :null)
          (sql-true-literal)))))

  (:method ((variable lexical-variable))
    (bind ((type (persistent-type-of variable)))
      (debug-only (assert (not (contains-syntax-p type))))
      (cond
        ((eq type +unknown-type+)
         (sql-map-failed))
        ((maybe-null-subtype-p type)
         (sql-is-null (syntax-to-sql variable))) ; FIXME
        (t
         nil))))

  (:method ((variable dynamic-variable))
    (bind ((type (persistent-type-of variable)))
      (debug-only (assert (not (contains-syntax-p type))))
      (cond
        ((eq type +unknown-type+)
         (sql-map-failed))
        ((maybe-null-subtype-p type)
         (sql-is-null (syntax-to-sql variable))) ; FIXME
        (t
         nil))))

  (:method ((access slot-access))
    (bind ((type (persistent-type-of access))
           (slot (slot-of access))
           (variable (arg-of access)))
      (debug-only (assert (not (contains-syntax-p type))))
      (when (or (eq type +unknown-type+)
                (null slot)
                (not (query-variable-p variable)))
        (sql-map-failed))
      
      (bind ((mapping (compute-mapping (canonical-type-for type)))
             (unit-types (remove 'unbound (unit-types-of mapping))))
        (when unit-types
          (sql-is-null (syntax-to-sql access)))))))

(defgeneric null-tag-for (syntax)
  (:documentation "Returns an SQL expression that gives the tag for the value of SYNTAX.")
  
  (:method (syntax)
    (sql-literal :value (compute-type-tag t)))

  (:method ((literal literal-value))
    (bind ((type (persistent-type-of literal)))
      (when (eq type +unknown-type+)
        (sql-map-failed))
      (bind ((type (canonical-type-for type))
             (mapping (compute-mapping type))
             (rdbms-values (lisp-value->rdbms-values type (value-of literal)))
             (unit-types (remove 'unbound (unit-types-of mapping))))
        (cond
          ((tagged-p mapping)
           (sql-literal :value (aref rdbms-values 0)))
          ((eq (aref rdbms-values (1- (length rdbms-values))) :null)
           (assert (and (length= 1 unit-types) (not (eq (first unit-types) 'unbound))))
           ;; NULL or H-UNUSED. FIXME (or null h-unused ...)
           (sql-literal :value (compute-type-tag (first unit-types))))
          (t
           (sql-literal :value (compute-type-tag t)))))))

  (:method ((variable lexical-variable))
    (bind ((type (persistent-type-of variable)))
      (debug-only (assert (not (contains-syntax-p type))))
      (when (eq type +unknown-type+)
        (sql-map-failed))
      
      (bind ((mapping (compute-mapping (canonical-type-for type)))
             (unit-types (remove 'unbound (unit-types-of mapping))))
        ;; FIXME does not work for (or null h-unused ...)
        (cond
          ((null unit-types) (sql-literal :value (compute-type-tag t)))
          ((member 'null unit-types) (sql-literal :value (compute-type-tag 'null)))
          (t (sql-literal :value (compute-type-tag (first unit-types))))))))

  (:method ((variable dynamic-variable))
    (bind ((type (persistent-type-of variable)))
      (debug-only (assert (not (contains-syntax-p type))))
      (when (eq type +unknown-type+)
        (sql-map-failed))
      
      (bind ((mapping (compute-mapping (canonical-type-for type)))
             (unit-types (remove 'unbound (unit-types-of mapping))))
        ;; FIXME does not work for (or null h-unused ...)
        (cond
          ((null unit-types) (sql-literal :value (compute-type-tag t)))
          ((member 'null unit-types) (sql-literal :value (compute-type-tag 'null)))
          (t (sql-literal :value (compute-type-tag (first unit-types))))))))

  (:method ((access slot-access))
    (bind ((type (persistent-type-of access))
           (slot (slot-of access))
           (variable (arg-of access)))
      (debug-only (assert (not (contains-syntax-p type)) () "Called too early, isn't it?"))

      (when (or (eq type +unknown-type+)
                (null slot)
                (not (query-variable-p variable)))
        (sql-map-failed))
      
      (bind ((type (canonical-type-for type))
             (mapping (compute-mapping type))
             (unit-types (unit-types-of mapping)))
        (cond
          ((tagged-p mapping)
           (sql-column-reference-for (tag-column-of slot) variable))
          (unit-types
           (assert (length= 1 unit-types) () "Has more than 1 unit type, and not tagged?")
           (sql-literal :value (compute-type-tag (first unit-types))))
          (t
           (sql-literal :value (compute-type-tag t))))))))

(defun check-for-rdbms-values (rdbms-values column-names column-types qualifier)
  (assert (= (length rdbms-values) (length column-names)))
  (apply 'sql-and
         (iter (for rdbms-value in-vector rdbms-values)
               (for column-name in column-names)
               (for column-type in column-types)
               (case rdbms-value
                 (#.+ignore-in-rdbms-equality-marker+ nil)
                 (:null (collect (sql-is-null (sql-column-reference-for column-name qualifier))))
                 (t (collect (sql-= (sql-column-reference-for column-name qualifier)
                                    (sql-literal :value rdbms-value
                                                 :type column-type))))))))

;;;----------------------------------------------------------------------------
;;; Functions mapped to SQL in queries
;;;
;;;

(def definer query-function (name args &body body)
  ""
  (flet ((parse-definer-options (options)
           (when (and -definer-
                      (not (keywordp (first options)))
                      (not (null options)))
             (iter (for flag :in-vector (string-downcase (pop options)))
                   (case flag
                     (#\l
                      (push #t options)
                      (push :lisp-args options))
                     (t (error "Flag '~A' is not available for definer ~S" flag (name-of -definer-))))))
           options))
    (bind (((:values body declarations documentation) (alexandria:parse-body body
                                                                            :documentation #t
                                                                            :whole -whole-))
           (declarations (apply #'append (mapcar #'cdr declarations)))
           (persistent-type-declaration (find 'persistent-type declarations :key #'first))
           (other-declarations (remove persistent-type-declaration declarations))
           (options (parse-definer-options -options-))
           (lisp-args-p (getf options :lisp-args #f)))
      (declare (ignore documentation)) ;; TODO

      (with-unique-names (all-args)
        `(progn
           ,@(when persistent-type-declaration
                   `((declaim-ftype ,name ,(second persistent-type-declaration))))

           (setf (get ',name 'sql-mapper)
                 (lambda (&rest ,all-args)
                   (apply
                    (lambda ,args
                      ,@(when other-declarations
                              `((declare ,@other-declarations)))
                      ,@body)
                    ,(if lisp-args-p
                         `(mapcar (lambda (syntax)
                                    (if (and (literal-value-p syntax)
                                             (eq (persistent-type-of syntax) 'keyword))
                                        (value-of syntax)
                                        syntax))
                                  ,all-args)
                         `(mapcar #'syntax-to-sql ,all-args))))))))))

;;;
;;; Oid
;;;
(def query-function oid-of (instance)
  ""
  (declare (persistent-type (function (persistent-object) number)))
  instance)

;;;
;;; Logical functions
;;;
(def query-function and (&rest values)
  ""
  (declare (persistent-type (function (&rest boolean) boolean)))
  (apply 'sql-and values))

(def query-function or (&rest values)
  ""
  (declare (persistent-type (function (&rest boolean) boolean)))
  (apply 'sql-or values))

(def query-function not (value)
  ""
  (declare (persistent-type (function (boolean) boolean)))
  (sql-not value))

;;;
;;; Comparison 
;;;

;; eq unbound x     = NULL
;; eq x unbound     = NULL
;; eq unused unused = TRUE
;; eq nil nil       = TRUE
;; eq x y           = x = y AND x IS NOT NULL AND y is NOT NULL
;; otherwise        = FALSE

;; CASE WHEN (x.tag=1 OR y.tag=1)      THEN NULL
;;      WHEN (x IS NULL OR y IS NULL) THEN x.tag=y.tag
;;      ELSE x = y
;; END

(def (query-function :lisp-args #t) eq (first second)
  "documentation"
  (declare (persistent-type (forall (a) (function (a a) boolean))))
  (sql-equal (syntax-to-sql first)
             (syntax-to-sql second)
             :unbound-check-1 (unbound-check-for first)
             :unbound-check-2 (unbound-check-for second)
             :null-check-1 (null-check-for first)
             :null-check-2 (null-check-for second)
             :null-tag-1 (null-tag-for first)
             :null-tag-2 (null-tag-for second)))

(def (query-function :lisp-args #t) eql (first second)
  "documentation"
  (declare (persistent-type (forall (a) (function (a a) boolean))))
  (sql-equal (syntax-to-sql first)
             (syntax-to-sql second)
             :unbound-check-1 (unbound-check-for first)
             :unbound-check-2 (unbound-check-for second)
             :null-check-1 (null-check-for first)
             :null-check-2 (null-check-for second)
             :null-tag-1 (null-tag-for first)
             :null-tag-2 (null-tag-for second)))

(def (query-function :lisp-args #t) equal (first second)
  "documentation"
  (declare (persistent-type (forall (a) (function (a a) boolean)))) ; TODO compare (or null a) with a?
  (sql-equal (syntax-to-sql first)
             (syntax-to-sql second)
             :unbound-check-1 (unbound-check-for first)
             :unbound-check-2 (unbound-check-for second)
             :null-check-1 (null-check-for first)
             :null-check-2 (null-check-for second)
             :null-tag-1 (null-tag-for first)
             :null-tag-2 (null-tag-for second)))

;; (define-sql-operator 'string= 'sql-string=) ; sql-string= tricky to implement because string=
                                               ; accepts chars and symbols too, use equal instead


(def query-function = (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a (or null number))) (function (a &rest a) boolean))))
  (apply (chained-operator 'sql-= #t) number numbers))

(def query-function /= (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a (or null number))) (function (a &rest a) boolean))))
  (apply (pairwise-operator 'sql-<> #t) number numbers))

(def query-function < (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a (or null number))) (function (a &rest a) boolean))))
  (apply (chained-operator 'sql-< #t) number numbers))

(def query-function > (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a (or null number))) (function (a &rest a) boolean))))
  (apply (chained-operator 'sql-> #t) number numbers))

(def query-function <= (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a (or null number))) (function (a &rest a) boolean))))
  (apply (chained-operator 'sql-<= #t) number numbers))

(def query-function >= (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a (or null number))) (function (a &rest a) boolean))))
  (apply (chained-operator 'sql->= #t) number numbers))

(def query-function timestamp= (&rest values)
  ""
  (declare (persistent-type (forall ((a (or null unbound date time timestamp))) (function (&rest a) boolean))))
  (apply (chained-operator 'sql-= #t) values))

(def query-function timestamp/= (&rest values)
  ""
  (declare (persistent-type (forall ((a (or null unbound date time timestamp))) (function (&rest a) boolean))))
  (apply (chained-operator 'sql-/= #t) values)) ;; TODO should not be pairwise-operator?

(def query-function timestamp> (&rest values)
  ""
  (declare (persistent-type (forall ((a (or null unbound date time timestamp))) (function (&rest a) boolean))))
  (apply (chained-operator 'sql-> #t) values))

(def query-function timestamp< (&rest values)
  ""
  (declare (persistent-type (forall ((a (or null unbound date time timestamp))) (function (&rest a) boolean))))
  (apply (chained-operator 'sql-< #t) values))

(def query-function timestamp>= (&rest values)
  ""
  (declare (persistent-type (forall ((a (or null unbound date time timestamp))) (function (&rest a) boolean))))
  (apply (chained-operator 'sql->= #t) values))

(def query-function timestamp<= (&rest values)
  ""
  (declare (persistent-type (forall ((a (or null unbound date time timestamp))) (function (&rest a) boolean))))
  (apply (chained-operator 'sql-<= #t) values))

;;;
;;; Arithmetic
;;;
(def query-function + (&rest numbers)
  ""
  (declare (persistent-type (forall ((a number)) (function (&rest a) a))))
  (apply 'sql-+ numbers))

(def query-function - (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a number)) (function (a &rest a) a))))
  (apply 'sql-- number numbers))

(def query-function * (&rest numbers)
  ""
  (declare (persistent-type (forall ((a number)) (function (&rest a) a))))
  (apply 'sql-* numbers))

(def query-function / (number &rest numbers)
  ""
  (declare (persistent-type (forall ((a number)) (function (a &rest a) a))))
  (apply 'sql-/ number numbers))

(def query-function mod (number divisor)
  ""
  (declare (persistent-type (forall ((a integer)) (function (a a) a))))
  (sql-% number divisor))

(def query-function expt (base power)
  ""
  (declare (persistent-type (forall ((a number)) (function (a a) a))))
  (sql-^ base power))

(def query-function sqrt (number)
  ""
  (declare (persistent-type (forall ((a number)) (function (a) a)))) ;; FIXME return type
  (sql-\|/ number))

(def query-function abs (number)
  ""
  (declare (persistent-type (forall ((a number)) (function (a) a))))
  (sql-@ number))

;;;
;;; String manipulation
;;;

(def query-function string+ (&rest strings)
  ""
  (declare (persistent-type (forall ((a string)) (function (&rest a) a))))
  (apply 'sql-\|\| strings))

(def query-function subseq (string start &optional end) ;; TODO other sequence types
  ""
  (declare (persistent-type (forall ((a string)) (function (a integer-32 &optional integer-32) a))))
  (sql-subseq string start end))

(def (query-function :lisp-args #t) like (string pattern &key (start 0) end (case-sensitive-p #t))
  ""
  (declare (persistent-type (forall ((a (or null string)))
                                    (function (a a &key (:start integer-32)
                                                 (:end (or null integer-32)) (:case-sensitive-p boolean))
                                              boolean))))
  (bind ((string (syntax-to-sql string))
         (pattern (syntax-to-sql pattern))
         (start (typecase start
                  (syntax-object (syntax-to-sql start))
                  (t start)))
         (end (typecase end
                (syntax-object (syntax-to-sql end))
                (t end)))
         (case-sensitive-p (typecase case-sensitive-p
                             (literal-value (value-of case-sensitive-p))
                             (t case-sensitive-p))))
    (sql-like :string (sql-subseq string start end)
              :pattern pattern
              :case-sensitive-p case-sensitive-p)))

(def (query-function :lisp-args #t) re-like (string pattern &key (start 0) end (case-sensitive-p #t))
  ""
  (declare (persistent-type (forall ((a (or null string)))
                                    (function (a a &key (:start integer-32)
                                                 (:end (or null integer-32)) (:case-sensitive-p boolean))
                                              boolean))))
  (bind ((string (syntax-to-sql string))
         (pattern (syntax-to-sql pattern))
         (start (typecase start
                  (syntax-object (syntax-to-sql start))
                  (t start)))
         (end (typecase end
                (syntax-object (syntax-to-sql end))
                (t end)))
         (case-sensitive-p (typecase case-sensitive-p
                             (literal-value (value-of case-sensitive-p))
                             (t case-sensitive-p))))
    (sql-regexp-like :string (sql-subseq string start end)
                    :pattern pattern
                    :case-sensitive-p case-sensitive-p)))


;;;
;;; Aggregate functions
;;;

(def query-function count (column)
  ""
  (declare (persistent-type (forall (a) (function (a) integer))))
  (sql-count column))

(def query-function min (column)
  ""
  (declare (persistent-type (forall ((a (or unbound null number string date time timestamp)))
                                    (function (a) (and (not unbound) (or null a))))))
  (sql-min column))

(def query-function max (column)
  ""
  (declare (persistent-type (forall ((a (or unbound null number string date time timestamp)))
                                    (function (a) (and (not unbound) (or null a))))))
  (sql-max column))

(def query-function sum (column)
  ""
  (declare (persistent-type (forall ((a (or unbound null number))) (function (a) (and (not unbound) (or null a))))))
  (sql-sum column))

(def query-function avg (column)
  ""
  (declare (persistent-type (forall ((a (or unbound null number))) (function (a) (and (not unbound) (or null a))))))
  (sql-avg column))

;;;
;;; Misc
;;;

(def query-function length (sequence)
  ""
  (declare (persistent-type (function (set) integer-32))) ;; FIXME ambigous string/list
  (sql-length sequence)) ; FIXME works only for strings

(def query-function null (object)
  (declare (persistent-type (forall (a) (function (a) boolean))))
  (sql-is-null object))

(def (query-function :lisp-args #t) sql-text (string)
  (declare (persistent-type (forall (a) (function (string) a))))
  (if (literal-value-p string)
      (sql-fragment :sql (value-of string)))
      (sql-unquote :form `(sql-fragment :sql ,(unparse-query-syntax string))))

(def (query-function :lisp-args #t) slot-boundp (object slot-name)
  ""
  (declare (persistent-type (forall ((a persistent-object)) (function (a symbol) boolean))))
  (if (and (query-variable-p object) (literal-value-p slot-name))
      (bind ((slot-name (when (symbolp (value-of slot-name)) (value-of slot-name)))
             (slot (when slot-name (find-slot-definition object slot-name))))
        (if (and slot (typep slot 'persistent-effective-slot-definition))
            (sql-slot-boundp object slot)
            (sql-map-failed)))
      (syntax-to-sql-literal-if-possible (make-function-call :fn 'slot-boundp
                                                             :args (list object slot-name)))))

(def (query-function :lisp-args #t) member (item list &rest ignored &key test)
  "TODO disjunct-set, ordered-set types"
  (declare (persistent-type (forall (a) (function (a (set a) &key (:test (or symbol function))) boolean)))
           (ignore test ignored))
  (cond
    ((literal-value-p list)
     (if (null (value-of list))
         (sql-false-literal)
         (sql-in (syntax-to-sql item) (syntax-to-sql list))))
    ((free-of-query-variables-p list)
     (sql-unquote :form
                  `(if (null ,(unparse-query-syntax list))
                       ,(sql-false-literal)
                       ,(sql-in (syntax-to-sql item)
                                (syntax-to-sql list)))))
    (t (sql-in (syntax-to-sql item) (syntax-to-sql list)))))

(def query-function coalesce (&rest args)
  "Returns the first of its arguments that is not null. Null is returned only if all arguments are null."
  (declare (persistent-type (forall (a) (function (a &rest a) a))))
  (sql-function-call :name 'coalesce :arguments args))



