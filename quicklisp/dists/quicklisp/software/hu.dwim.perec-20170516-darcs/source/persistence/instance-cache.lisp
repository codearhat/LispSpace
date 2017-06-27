;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;; TODO: consider merging the created, modified, and deleted hash-tables
(def class* transaction-instance-cache-mixin ()
  ((instances
    (make-hash-table :test #'eql)
    :type hash-table
    :documentation "A map from oid values to persistent instances used to cache instance identities and slot values during a transaction.")
   (created-instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from instances to true indicating that the instance is in the set.")
   (modified-instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from instances to true indicating that the instance is in the set.")
   (deleted-instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from instances to true indicating that the instance is in the set.")
   (bulks
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from symbols to bulks of instances. Used to cache complex trees, graphs of instances at once."))
  (:documentation "Each transaction has its own transaction level instance cache filled by the operations executed during that transaction. The cache is created empty when the transaction starts and it will be dropped when the transaction ends. Each instance loaded during a transaction will be put here to keep the identity of the in-memory instance throughout the transaction. Moreover the instance cache is responsible to manage the list of created, modified and deleted instances during the transaction."))

(def method hu.dwim.rdbms::cleanup-transaction :after ((transaction transaction-instance-cache-mixin))
  (setf (instances-of transaction) nil)
  (setf (created-instances-of transaction) nil)
  (setf (modified-instances-of transaction) nil)
  (setf (deleted-instances-of transaction) nil)
  (setf (bulks-of transaction) nil))

;;;;;;
;;; Single Instances

(def (function io) cached-instance-of (oid)
  "Returns the instance for the given oid from the current transaction's instance cachce."
  (gethash (oid-instance-id oid) (instances-of *transaction*)))

(def (function io) (setf cached-instance-of) (instance oid)
  "Puts an instance with the given oid into the current transaction's instance cache and attaches it to the current transaction. The instance must not be present in the cache before."
  (debug-only
    (assert (not (instance-in-transaction-p instance)))
    (assert (not (cached-instance-of oid))))
  (setf (transaction-of instance) *transaction*)
  (let ((key (oid-instance-id oid)))
    (assert key)
    (setf (gethash key (instances-of *transaction*)) instance)))

;; TODO what about assocation caches keeping the instance from getting gc'd?
(def (function ioe) remove-instance-from-transaction-cache (instance)
  "Removes an instance from the current transaction's instance cache and detaches it from the transaction.

Take note that the removed instance will not be attached to the current transaction anymore, so if you later want to access some of its slots that are unfetched, then you'll need to reattach it to the transaction using LOAD-INSTANCE, or its cousine, REVIVE-INSTANCE."
  (bind ((oid (oid-of instance))
         (key (oid-instance-id oid)))
    (debug-only
      (assert (instance-in-current-transaction-p instance))
      (assert (cached-instance-of oid)))
    (setf (transaction-of instance) nil)
    (remhash key (instances-of *transaction*))))

;;;;;;
;;; Transaction event

(def (function io) map-cached-instances (function)
  "Maps the given one parameter function to all instances present in the cache."
  (declare (type function function))
  (alexandria:maphash-values function (instances-of *transaction*)))

(def (function io) map-created-instances (function)
  (declare (type function function))
  (alexandria:maphash-keys function (created-instances-of *transaction*)))

(def (function io) map-modified-instances (function)
  (declare (type function function))
  (alexandria:maphash-keys function (modified-instances-of *transaction*)))

(def (function io) map-deleted-instances (function)
  (declare (type function function))
  (alexandria:maphash-keys function (deleted-instances-of *transaction*)))

(def (function o) update-instance-cache-for-created-instance (instance)
  (ecase (transaction-event-of instance)
    (:created)
    ((:modified :deleted)
     (error "Inconsistent cache"))
    ((nil)
     (setf (transaction-event-of instance) :created)
     (setf (gethash instance (created-instances-of *transaction*)) #t))))

(def (function o) update-instance-cache-for-modified-instance (instance)
  (unless (transaction-event-of instance)
    (setf (transaction-event-of instance) :modified)
    (setf (gethash instance (modified-instances-of *transaction*)) #t)))

(def (function o) update-instance-cache-for-deleted-instance (instance)
  (ecase (transaction-event-of instance)
    (:created
     (setf (transaction-event-of instance) nil)
     (remhash instance (created-instances-of *transaction*)))
    (:modified
     (setf (transaction-event-of instance) :deleted)
     (remhash instance (modified-instances-of *transaction*))
     (setf (gethash instance (deleted-instances-of *transaction*)) #t))
    (:deleted)
    ((nil)
     (setf (transaction-event-of instance) :deleted)
     (setf (gethash instance (deleted-instances-of *transaction*)) #t))))

;;;;;;
;;; Bulks

(def (function io) cached-bulk-of (bulk)
  "Returns an instance representing the bulk of persistent instances if it has been already cached in this transaction."
  (gethash bulk (bulks-of *transaction*)))

(def (function io) (setf cached-bulk-of) (instance bulk)
  "Stores an instance representing the bulk of persistent instances."
  (debug-only
    (assert (not (cached-bulk-of bulk))))
  (setf (gethash bulk (bulks-of *transaction*)) instance))

(def (function o) cache-to-many-association-ends (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (when-bind parent (funcall parent-accessor instance)
      (bind ((parent-class (class-of parent))
             (parent-children-slot (funcall children-slot-provider parent-class))
             ((:values cached-p children)
              (slot-value-cached-p parent parent-children-slot)))
        (assert cached-p)
        (setf (underlying-slot-boundp-or-value-using-class parent-class parent parent-children-slot)
              (cons instance children))))))

(def (function o) cache-to-many-association-ends-for-tree (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class)))
      (setf (underlying-slot-boundp-or-value-using-class class instance children-slot) nil)))
  (cache-to-many-association-ends instances children-slot-provider parent-accessor)
  (find-tree-root (first instances) parent-accessor))

(def macro ensure-cached-to-many-association-ends-for-tree (bulk instances children-slot-provider parent-accessor)
  `(aif (cached-bulk-of ,bulk)
    it
    (setf (cached-bulk-of ,bulk)
     (cache-to-many-association-ends-for-tree ,instances ,children-slot-provider ,parent-accessor))))

(def (function o) cache-to-many-association-ends-for-1-n-association (children children-slot-provider parent-accessor)
  (dolist (child children)
    (bind ((parent (funcall parent-accessor child))
           (parent-class (class-of parent))
           (parent-children-slot (funcall children-slot-provider parent-class)))
      (setf (underlying-slot-boundp-or-value-using-class parent-class parent parent-children-slot) nil)))
  (cache-to-many-association-ends children children-slot-provider parent-accessor)
  children)

(def macro ensure-cached-to-many-association-ends-for-1-n-association (bulk children children-slot-provider parent-accessor)
  `(aif (cached-bulk-of ,bulk)
    it
    (setf (cached-bulk-of ,bulk)
     (cache-to-many-association-ends-for-1-n-association ,children ,children-slot-provider ,parent-accessor))))
