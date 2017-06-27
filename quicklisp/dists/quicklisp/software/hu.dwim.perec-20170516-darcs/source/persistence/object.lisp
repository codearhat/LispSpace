;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Persistent object base class

(def special-variable *make-persistent-instances* #t
  "True means make-instance will make the new instance persistent by default.")

(def persistent-class* persistent-object ()
  ((oid
    nil
    :type (or null integer)
    :persistent #f
    :documentation "Life time unique identifier of the instance which can be remembered and may be used to load the instance later.")
   ;; TODO: consider moving this flag into the oid
   (persistent
    :type boolean
    :persistent #f
    :documentation "True means the instance is known to be persistent, false means the instance is known to be transient, unbound means the state is not yet determined. Actually, in the latter case slot-value-using-class will automatically determine whether the instance is in the database or not. Therefore reading the persistent slot will always return either true or false.")
   (transaction
    nil
    :type (or null transaction-mixin)
    :persistent #f
    :documentation "A reference to the transaction to which this instance is currently attached to or nil.")
   ;; TODO: kill this slot and use the hash-tables in the transaction cache
   (transaction-event
    nil
    :type (member nil :created :modified :deleted)
    :persistent #f
    :documentation ":created means the instance was created in the current transaction, :modified means the instance was not created but modified in the current transaction. :deleted means the instance was already present at the very beginning but got deleted in the current transaction."))
  (:default-initargs :persistent *make-persistent-instances*)
  (:abstract #t)
  (:direct-store :push-down)
  (:documentation "Base class for all persistent classes. If this class is not inherited by a persistent class then it is automatically added to the direct superclasses. There is only one persistent instance in a transaction with a give oid therefore eq will return true iff the oids are equal."))

(def method hash-key ((instance persistent-object))
  "Used to calculate the key for hashtable access."
  (oid-of instance))

(def (function e) persistent-object-internal-slot-p (slot)
  (member (slot-definition-name slot) '(oid persistent transaction transaction-event) :test #'eq))

(def (macro e) with-making-persistent-instances (&body forms)
  `(let ((*make-persistent-instances* #t))
     ,@forms))

(def (macro e) with-making-transient-instances (&body forms)
  `(let ((*make-persistent-instances* #f))
     ,@forms))

;;;;;;
;;; MOP methods

(def method allocate-instance ((class persistent-class) &rest args)
  (declare (ignore args))
  (prog1-bind instance (call-next-method)
    (iter (for slot :in (persistent-effective-slots-of class))
          (underlying-slot-makunbound-using-class class instance slot))))

(def method initialize-instance :around ((instance persistent-object) &rest args &key persistent)
  (bind ((class (class-of instance)))
    (when persistent
      (ensure-exported class))
    (prog1 (apply #'call-next-method instance :persistent #f args)
      (when (eq persistent #t)
        (make-persistent instance)))))

(def method make-instance :before ((class persistent-class) &key &allow-other-keys)
  (when (abstract-p class)
    (error "Cannot make instances of abstract class ~A" class)))

(def method before-committing-instance ((transaction transaction-mixin) (instance persistent-object) transaction-event)
  (bind ((class (class-of instance)))
    (dolist (slot (persistent-effective-slots-of class))
      (when (eq :on-commit (type-check-of slot))
        (bind (((:values cached-p slot-value) (slot-value-cached-p instance slot)))
          (when cached-p
            (check-slot-value-type instance slot slot-value #t)))))))

(def method after-instance-committed ((transaction transaction-mixin) (instance persistent-object) transaction-event)
  (values))

;;;;;;
;;; Utility

(def special-variable +persistent-object-class+ (find-class 'persistent-object))

(def function persistent-object-p (instance)
  (typep instance 'persistent-object))

(def (function o) p-eq (instance-1 instance-2)
  "Tests if two instances are the same persistent instance. Normally there is at most one persistent instance for each oid in a transaction so eq may be safely used. On the other hand huge transactions may require to throw away instances form the instance cache which results in several instances for the same oid within the same transaction."
  (declare (type (or null persistent-object) instance-1 instance-2))
  (or (eq instance-1 instance-2)
      (and instance-1
           instance-2
           ;; NOTE: this is somewhat faster than comparing the two oids
           (eq (class-of instance-1)
               (class-of instance-2))
           ;; TODO: oid-of is a full call to svuc due to specializing on standard slots for the computation of persistent flag
           (= (the integer (oid-of instance-1))
              (the integer (oid-of instance-2))))))

(def function print-persistent-instance (instance &key (include-persistent-flag #t) (include-oid #t))
  (declare (type persistent-object instance))
  (when include-persistent-flag
    (write-string ":persistent ")
    (write-string (cond ((not (slot-boundp instance 'persistent))
                         "#? ")
                        ((persistent-p instance)
                         "#t ")
                        (t "#f "))))
  (when include-oid
    (if (slot-boundp instance 'oid)
        (aif (oid-of instance)
             (princ it)
             (write-string "nil"))
        (write-string "?"))))

(def (definer e) print-object/persistent (class-name* &body body)
  `(def print-object ,class-name*
     ,@body
     (write-string " ")
     (print-persistent-instance -self- :include-persistent-flag #f)))

(def print-object (persistent-object :identity (or (not (slot-boundp -self- 'persistent))
                                                   (not (persistent-p -self-))))
  "Prints the oid of the instance and whether the instance is known to be persistent or transient."
  (print-persistent-instance -self-))

(def function created-p (instance)
  (eq :created (transaction-event-of instance)))

(def function modified-p (instance)
  (eq :modified (transaction-event-of instance)))

(def function deleted-p (instance)
  (eq :deleted (transaction-event-of instance)))

(def function ensure-oid (instance)
  "Makes sure that the instance has a valid oid."
  (unless (oid-of instance)
    (setf (oid-of instance) (make-new-oid (class-of instance)))))
