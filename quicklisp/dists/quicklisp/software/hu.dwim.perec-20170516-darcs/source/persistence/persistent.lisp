;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Load and cache

(def function instance-exists-in-database-p (instance)
  "Returns true if the instance can be found in the database"
  (and (oid-of instance)
       (not (null (select-records '(1)
                                  (list (name-of (primary-table-of (class-of instance))))
                                  :where (make-oid-matcher-where-clause instance))))))

;; TODO rename to persistent?/debug or something else. debug- here sounds as a verb not as an adjective...
(def function debug-persistent-p (instance)
  "Same as persistent-p except it never prefetches slot values. Use for debug purposes."
  (if (slot-boundp instance 'persistent)
      (persistent-p instance)
      (progn
        ;; do not count this existence check as a select, because it will not execute in release code
        (when (oid-of instance)
          (decf (select-counter-of (hu.dwim.rdbms::command-counter-of *transaction*))))
        (setf (persistent-p instance) (instance-exists-in-database-p instance)))))

(def generic initialize-revived-instance (instance &key &allow-other-keys)
  (:documentation "When a revived instance is initialized slots marked with initialize-revived-slot-p will be passed down to be initialized by shared-initialize.")

  (:method ((instance persistent-object) &rest args &key oid &allow-other-keys)
    ;; TODO: use definer
    #-debug
    (declare (optimize (speed 3) (debug 0))
             (dynamic-extent args))
    (assert oid)
    (bind ((class (class-of instance))
           (persistent-effective-slots (persistent-effective-slots-of class))
           (slot-names (mapcar #'slot-definition-name
                               (set-difference (class-slots class) persistent-effective-slots))))
      (dolist (slot persistent-effective-slots)
        (invalidate-cached-slot instance slot))
      (apply #'shared-initialize instance slot-names args))))

(def generic make-revived-instance (class &key &allow-other-keys)
  (:documentation "Creates a new instance representing the given oid as its identity. The instance will not be associated with the current transaction nor will it be stored in the database. The instance may or may not be known to be either persistent or transient. This generic function should not be called outside of hu.dwim.perec but methods may be defined on it.")

  (:method ((class persistent-class) &rest args &key &allow-other-keys)
    (apply #'initialize-revived-instance (allocate-instance class) args)))

(def generic cache-instance (thing)
  (:documentation "Attaches an instance to the current transaction. The instance must be already present in the database, so load-instance would return an instance for it. The purpose of this method is to cache instances returned by a query or when the existence may be guaranteed by some other means.")

  (:method ((rdbms-values sequence))
    (cache-instance (rdbms-values->oid rdbms-values)))

  (:method ((oid integer))
    (aif (cached-instance-of oid)
         (prog1 it
           (setf (persistent-p it) #t))
         (setf (cached-instance-of oid) (make-revived-instance (find-class (oid-class-name oid)) :oid oid :persistent #t))))

  (:method ((instance persistent-object))
    (debug-only (assert (debug-persistent-p instance)))
    (setf (cached-instance-of (oid-of instance)) instance)))

(def condition* instance-not-found-error (error)
  ((oid nil))
  (:report (lambda (c stream)
             (format stream "Instance not found for oid ~A" (oid-of c)))))

(def condition* persistent-instance-expected (error)
  ((instance nil))
  (:report (lambda (c stream)
             (format stream "Operation not available on transient instances; called with ~A" (instance-of c)))))

(def generic load-instance (thing &key otherwise prefetch skip-existence-check copy-cached-slot-values)
  (:documentation "Loads an instance with the given oid and attaches it with the current transaction if not yet attached. If no such instance exists in the database then one of two things may happen. If the value of otherwise is a lambda function with one parameter then it is called with the given instance. Otherwise the value of otherwise is returned. If prefetch is false then only the identity of the instance is loaded, otherwise all slots are loaded. Note that the instance may not yet be committed into the database and therefore may not be seen by other transactions. Also instances not yet committed by other transactions are not returned according to transaction isolation rules. The instance returned will be kept for the duration of the transaction and any subsequent calls to load, select, etc. will return the exact same instance for which eq is required to return #t.")

  (:method ((instance persistent-object) &rest args &key (skip-existence-check #f) (copy-cached-slot-values #f) &allow-other-keys)
    (ensure-exported (class-of instance))
    (unless (persistent-p instance)
      (error 'persistent-instance-expected :instance instance))
    (prog1-bind new-instance (apply #'load-instance (oid-of instance) args)
      (when copy-cached-slot-values
        (assert skip-existence-check)
        (setf (persistent-p new-instance) (persistent-p instance))
        (copy-cached-slot-values instance new-instance))))

  (:method ((oid integer) &key (otherwise nil otherwise-provided-p) (prefetch #f) (skip-existence-check #f) &allow-other-keys)
    (declare (ignore prefetch))
    (flet ((instance-not-found ()
             (cond ((not otherwise-provided-p)
                    (error 'instance-not-found-error :oid oid))
                   ((functionp otherwise)
                    (funcall otherwise oid))
                   (t otherwise))))
      (aif (cached-instance-of oid)
           it
           (let ((new-instance (make-revived-instance (find-class (oid-class-name oid)) :oid oid)))
             ;; REVIEW: is this the correct thing to do?
             ;; we push new-instance into the cache first
             ;; even though we are unsure if the instance is persistent or not
             ;; because prefetching slots may recursively call load-instance from persistent-p
             ;; we also want to have non persistent instances in the cache anyway
             (setf (cached-instance-of oid) new-instance)
             (if (or skip-existence-check (persistent-p new-instance))
                 new-instance
                 (progn
		   ;; REVIEW #2: following up on the above comment: the
		   ;; old code was definitely not correct, since
		   ;;   (load-instance <nonexisting-oid> ...)
		   ;; would put a transient object for that oid into the
		   ;; cache, with slots all set to the
		   ;; +NOT-CACHED-SLOT-MARKER+, which then leaks into
		   ;; user code (in particular with :otherwise, but also
		   ;; without otherwise if user code traps the condition).
		   ;;
		   ;; My workaround is to remove the object here, fixing at
		   ;; least this particular problem.
		   ;;   -- david
		   (remove-instance-from-transaction-cache new-instance)
		   (instance-not-found))))))))

;;;;;;
;;; Purge

(def generic purge-instance (instance)
  (:documentation "Purges the given instance without respect to associations and references to it.")

  (:method ((instance persistent-object))
    (ensure-exported (class-of instance))
    (dolist (table (data-tables-of (class-of instance)))
      (delete-records (name-of table)
                      (make-oid-matcher-where-clause instance)))
    (update-instance-cache-for-deleted-instance instance)
    (setf (persistent-p instance) #f)))

;; TODO: what about invalidating cache instances, references?
(def generic purge-instances (class)
  (:documentation "Purges all instances of the given class without respect to associations and references thus it may introduce new broken references.")

  (:method ((name null))
    (values))

  (:method ((name symbol))
    (purge-instances (find-class name)))

  (:method ((classes-or-names sequence))
    (map nil #'purge-instance classes-or-names))

  (:method ((class persistent-class))
    (ensure-exported class)
    (bind ((classes (remove-if #'abstract-p (list* class (persistent-effective-subclasses-of class))))
           (tables (reduce #'union (mapcar #'data-tables-of classes)))
           (where-clause (make-class-id-matcher-where-clause classes)))
      (dolist (table tables)
        (ensure-exported table)
        (delete-records (name-of table)
                        (etypecase table
                          (class-primary-table
                           (when (set-difference (stored-persistent-classes-of table) classes)
                             where-clause))
                          (association-primary-table
                           nil)))))))

(def (function e) purge-instance-recursively (instance &key (skip-predicate (constantly #f)))
  "Recursively purges INSTANCE and a minimal set of other instances that refer to it. Makes sure that the database integrity is kept, i.e. no broken references are introduced."
  (check-type instance persistent-object)
  (labels ((recurse (instance)
             (when (persistent-p instance)
               ;; NOTE: purge must come before recurse, but we need to get the data before purge
               (bind ((instances (iter (with class = (class-of instance))
                                       (with data-tables = (data-tables-of class))
                                       (for slot :in (persistent-effective-slots-of class))
                                       (for table = (table-of slot))
                                       (when (and (typep slot 'persistent-association-end-slot-definition)
                                                  (not (typep slot 'persistent-association-end-slot-definition-d))
                                                  (slot-boundp-using-class class instance slot))
                                         (if (eq :m-n (association-kind-of (association-of slot)))
                                             (setf (slot-value-using-class class instance slot) nil)
                                             (unless (member table data-tables)
                                               (dolist (referred-instance (ensure-list (slot-value-using-class class instance slot)))
                                                 (if (funcall skip-predicate referred-instance)
                                                     (bind ((other-slot (find-slot (class-of referred-instance)
                                                                                   (slot-definition-name (other-association-end-of slot)))))
                                                       (declare (ignore other-slot))
                                                       (not-yet-implemented))
                                                     (collect referred-instance)))))))))
                 (purge-instance instance)
                 (foreach #'recurse instances)))))
    (recurse instance)))

;;;;;;
;;; Drop

(def function drop-persistent-classes ()
  (flet ((drop-table* (owner)
           (when (table-exists-p (name-of owner))
             (drop-table (name-of owner) :cascade #t))
           (invalidate-computed-slot owner 'ensure-exported)))
    (iter (for (class-name class) :in-hashtable *persistent-classes*)
          (awhen (primary-table-of class)
            (drop-table* it)))
    (iter (for (association-name association) :in-hashtable *persistent-associations*)
          (awhen (primary-table-of association)
            (drop-table* it)))))

;;;;;;
;;; Lock

(def macro with-waiting-for-rdbms-lock (wait &body forms)
  (with-unique-names (body)
    `(block with-waiting-for-rdbms-lock
       (flet ((,body ()
                ,@forms))
         (if ,wait
             (,body)
             (handler-case
                 (,body)
               (unable-to-obtain-lock-error ()
                 (return-from with-waiting-for-rdbms-lock #f))))))))

(def function lock-columns (instance columns wait)
  (with-waiting-for-rdbms-lock wait
    (bind ((class (class-of instance))
           (records
            (execute (sql-select :columns columns
                                 :tables (list (name-of (direct-instances-data-view-of class)))
                                 :where (make-oid-matcher-where-clause instance)
                                 :for :update
                                 :wait wait))))
      (assert (= 1 (length records)))
      #t)))

(def generic lock-class (class &key wait)
  (:documentation "Lock all instances in the current transaction. If wait is false and the class cannot be locked then returns #f otherwise returns #t.")

  (:method ((class persistent-class) &key (wait #t))
    (with-waiting-for-rdbms-lock wait
      (unless (primary-table-of class)
        ;; TODO
        (error "There's no primary-table for the class ~S, so which table to lock? This is fixable, but needs further thinking and this error is a safe bet for now..." class))
      (execute (sql-lock-table :table (name-of (primary-table-of class))
                               :mode :exclusive
                               :wait wait))
      #t))

  (:method ((class-name symbol) &key (wait #t))
    (bind ((class (find-persistent-class class-name)))
      (unless class
        (error "~S does not name a persistent class" class-name))
      (lock-class class :wait wait))))

(def generic lock-instance (instance &key wait)
  (:documentation "Lock instance in the current transaction. If wait is false and the instance cannot be locked then returns #f otherwise returns #t.")

  (:method ((instance persistent-object) &key (wait #t))
    (lock-columns instance (list (sql-all-columns)) wait)))

(def generic lock-slot (instance slot &key wait)
  (:documentation "Lock a slot for an instance in the current transaction. If wait is false and the slot cannot be locked then returns #f otherwise returns #t.")

  (:method ((instance persistent-object) (slot symbol) &key (wait t))
    (lock-slot instance (find-slot (class-of instance) slot) :wait wait))

  (:method ((instance persistent-object) (slot persistent-effective-slot-definition) &key (wait t))
    (lock-columns instance (columns-of slot) wait)))

;;;;;;
;;; Count

(def generic count-instances (class)
  (:documentation "Counts all instances of the given class")

  (:method ((class-name symbol))
    (count-instances (find-class class-name)))

  (:method ((class persistent-class))
    (first-elt (first-elt (execute (sql-select :columns (list (sql-count (sql-all-columns)))
                                               :tables (list (name-of (all-instances-identity-view-of class)))))))))

;;;;;;
;;; Revive

(def macro revive-instance (place &rest args)
  "Load instance found in PLACE into the current transaction, update PLACE if needed. The instance being revived cannot be part of another ongoing transaction. Use load-instance if that is needed."
  (with-unique-names (instance)
    `(bind ((,instance ,place))
       (when ,instance
         (assert (or (not (instance-in-transaction-p ,instance))
                     (eq (transaction-of ,instance)
                         *transaction*))
                 nil "The place ~S being revived holds an alive instance in another transaction. Maybe you should use load-instance and friends." ',place)
         (setf ,place (load-instance ,instance ,@args))))))

(def macro revive-instances (&rest places)
  `(progn
     ,@(iter (for place :in places)
             (collect `(revive-instance ,place)))
     (values)))

(def macro with-revived-instances (instances &body body)
  "Rebind the variables specified in INSTANCES, revive them in the current transaction and execute BODY in this lexical environment."
  (unless (every #'symbolp instances)
    (error "with-revived-instances works only on variables"))
  `(rebind (,@instances)
     ,@(iter (for instance :in instances)
             (collect `(revive-instance ,instance)))
     ,@body))

(def macro with-revived-instance (instance &body body)
  "See WITH-REVIVED-INSTANCES."
  `(with-revived-instances (,instance)
     ,@body))

;;;;;;
;;; Reload

(def macro with-reloaded-instances (instances &body body)
  "Rebind the variables specified in INSTANCES, reload them in the current transaction and execute BODY in this lexical environment. If an entry is a list then bind with the first form and reload the second form."
  `(bind ,(iter (for entry :in instances)
                (for variable = (if (consp entry)
                                    (first entry)
                                    entry))
                (for expression = (if (consp entry)
                                      (second entry)
                                      entry))
                (collect `(,variable (load-instance ,expression))))
     ,@body))

(def macro with-reloaded-instance (instance &body body)
  "See WITH-RELOADED-INSTANCES."
  `(with-reloaded-instances (,instance)
     ,@body))

;;;;;
;;; Persistent instance

(def function %persistent-instance/fn (variable-name new-value-thunk)
  (aif (symbol-value variable-name)
       (load-instance it)
       (progn
         (register-transaction-hook :before :rollback
           (setf (symbol-value variable-name) nil))
         (setf (symbol-value variable-name)
               (funcall new-value-thunk)))))

(def (definer e :available-flags "e") persistent-instance (name &body forms)
  (bind ((name-string (symbol-name name))
         (variable-name (symbolicate (subseq name-string 0 (1- (length name-string))) '#:-instance*)))
    (with-standard-definer-options name
      `(progn
         (defparameter ,variable-name nil)
         (define-symbol-macro ,name
             (%persistent-instance/fn
              ',variable-name
              (named-lambda ,(symbolicate '#:persistent-instance/factory/ name)
                  ()
                (the-non-nil ,@forms))))))))

;;;;;;
;;; Making instances persistent and transient

(def method make-persistent-using-class (class (instance persistent-object))
  (ensure-oid instance)
  (update-instance-cache-for-created-instance instance)
  (store-all-slots instance)
  (setf (persistent-p instance) #t)
  (setf (cached-instance-of (oid-of instance)) instance)
  (iter (for slot :in (persistent-effective-slots-of class))
        (when (set-type-p* (canonical-type-of slot))
          (invalidate-cached-slot instance slot))))

(def method make-transient-using-class (class (instance persistent-object))
  (with-caching-slot-values
    (bind (((:values restored-slot-values restored-slots) (restore-all-slots instance)))
      (iter (for restored-slot-value in restored-slot-values)
            (for restored-slot in restored-slots)
            (setf (underlying-slot-boundp-or-value-using-class class instance restored-slot) restored-slot-value))))
  (remove-instance-from-transaction-cache instance)
  (purge-instance instance))

;;;;;;
;;; Broken references

(def (function e) signal-broken-references ()
  (bind ((oids (execute (make-query-for-classes-and-slots (persistent-effective-subclasses-of (find-class 'persistent-object)) nil)))
         (oid-set (make-hash-table :test #'eql))
         (tables nil)
         (table->referer-slots-map (make-hash-table)))
    (iter (for oid-record :in-sequence oids)
          (for oid = (first-elt oid-record))
          (setf (gethash oid oid-set) oid))
    (iter (for (class-name class) :in-hashtable *persistent-classes*)
          (pushnew (primary-table-of class) tables)
          (dolist (slot (persistent-effective-slots-of class))
            (when (persistent-class-type-p* (canonical-type-of slot))
              (bind ((oid-table (table-of slot)))
                (when oid-table
                  (bind ((referer-slots (gethash oid-table table->referer-slots-map)))
                    (unless (find (column-names-of slot) referer-slots :key #'column-names-of :test #'equal)
                      (setf (gethash oid-table table->referer-slots-map)
                            (pushnew slot referer-slots)))))))))
    (iter (for (table referer-slots) :in-hashtable table->referer-slots-map)
          (dolist (referer-slot referer-slots)
            (for records = (select-records (append +oid-column-names+ (columns-of referer-slot)) (list (name-of table))))
            (iter (for record :in-sequence records)
                  (bind ((referer-oid (rdbms-values->oid record))
                         (referred-oid
                          (unless (eq :null (elt record +oid-column-count+))
                            (rdbms-values->oid* record +oid-column-count+))))
                    (when (and referred-oid
                               (not (gethash referred-oid oid-set)))
                      (bind ((referer (load-instance referer-oid)))
                        (cerror "Let's see if there's more" "Slot ~A in ~A has a broken reference to ~A"
                                (slot-definition-name referer-slot) referer
                                (make-instance (oid-class-name referred-oid) :persistent #f :oid referred-oid))))))))))

(def (function e) signal-broken-instances ()
  (iter (for (class-name class) :in-hashtable *persistent-classes*)
        (awhen (primary-table-of class)
          (for records = (select-records +oid-column-names+ (list (name-of it))))
          (iter (for record :in-sequence records)
                (for oid = (rdbms-values->oid record))
                (dolist (table (data-tables-of class))
                  (when (zerop (length (select-records '(1) (list (name-of table)) :where (make-oid-matcher-where-clause oid))))
                    (cerror "Let's see if there's more" "Insance ~A is broken because no matching record can be found in ~A"
                            (make-instance (oid-class-name oid) :persistent #f :oid oid) table)))))))

(def (function e) signal-broken-database ()
  (signal-broken-references)
  (signal-broken-instances))
