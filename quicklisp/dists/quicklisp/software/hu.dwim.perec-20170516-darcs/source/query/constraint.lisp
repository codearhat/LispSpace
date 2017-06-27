;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;; TODO move most of this into transaction.lisp (the query-compiler dependent parts) into source/transaction.lisp

;;;;;;
;;; Persistent constraint

(def special-variable *persistent-constraints* (make-hash-table))

(def (function e) find-constraint (name)
  (gethash name *persistent-constraints*))

(def (function e) (setf find-constraint) (new-value name)
  (setf (gethash name *persistent-constraints*) new-value))

(defcclass* persistent-constraint ()
  ((select
    (compute-as nil)
    :type form
    :documentation "This query returns a set which violate the constraint. If it returns nil then the database is considerent consistent with respect to this constraint.")
   (query
    (compute-as (make-query (select-of -self-)))
    :type query
    :documentation "The compiled query object."))
  (:documentation "A constraint defines restrictions for the instances present in the database."))



(defcclass* persistent-class-constraint (persistent-constraint)
  ((select (compute-as `(select (-instance-)
                          (from (-instance- ,(class-name (constrained-class-of -self-))))
                          (where ,(assert-of -self-))))
    :type form)
   (constrained-class
    :type persistent-class
    :computed-in computed-universe/perec)
   (assert
    :type form
    :computed-in computed-universe/perec)))

(defcclass* persistent-association-constraint (persistent-constraint)
  ((association
    :type persistent-association)))

(defcclass* persistent-slot-constraint (persistent-class-constraint)
  ((constrained-class
    (compute-as (persistent-slot-definition-class (constrained-slot-of -self-)))
    :type persistent-class)
   (constrained-slot
    :type persistent-effective-slot-definition
    :computed-in computed-universe/perec)
   (assert
    :type form
    :computed-in computed-universe/perec)))

;;;;;;
;;; Macro interface

(def (macro e) defconstraint (name &body body)
  `(setf (find-constraint ',name) (make-instance 'persistent-constraint :select ',@body)))

(def (definer e) constraint (name &body body)
  `(defconstraint ,name ,@body))

;;;;;;
;;; Conditions

(def (condition* e) persistent-constraint-violation (error)
  ((constraint
    :type constraint)
   (result
    :type list))
  (:report (lambda (constraint-violation stream)
             (format stream "Constraint ~A violated for arguments ~A"
                     (constraint-of constraint-violation) (result-of constraint-violation)))))

(def (condition* e) persistent-class-constraint-violation (persistent-constraint-violation)
  ((object
    :type persistent-object))
  (:report (lambda (constraint-violation stream)
             (format stream "Constraint ~A violated for object ~A"
                     (constraint-of constraint-violation) (object-of constraint-violation)))))

(def (condition* e) persistent-slot-constraint-violation (persistent-class-constraint-violation)
  ((slot
    :type effective-slot))
  (:report (lambda (constraint-violation stream)
             (format stream "Constraint ~A violated for the slot ~A of object ~A"
                     (constraint-of constraint-violation) (object-of constraint-violation) (slot-of constraint-violation)))))

;;;;;;
;;; Constraint checking

(def special-variable *ignore-constraints* #f)

(def special-variable *signal-constraint-violations* #t)

(def generic make-constraint-violation (constraint result)
  (:method ((constraint persistent-constraint) result)
    (make-condition 'persistent-constraint-violation
                    :constraint constraint
                    :result result))

  (:method ((constraint persistent-class-constraint) result)
    (make-condition 'persistent-class-constraint-violation
                    :constraint constraint
                    :result result
                    :object (first result)))

  (:method ((constraint persistent-slot-constraint) result)
    (make-condition 'persistent-slot-constraint-violation
                    :constraint constraint
                    :result result
                    :object (first result)
                    :slot (constrained-slot-of constraint))))

(def (generic e) check-constraint (constraint)
  (:documentation "Checks whether the constraint is violated in the current transaction and signals constraint violation errors.")

  (:method ((constraint persistent-constraint))
    (map nil (lambda (result)
               (cerror "Ignore constraint violation and mark transaction for rollback only" (make-constraint-violation constraint result))
               (mark-transaction-for-rollback-only))
         (execute-query (query-of constraint)))))

(def (function e) check-all-constraints (&key (signal-constraint-violations *signal-constraint-violations*))
  (bind ((violations nil))
    (flet ((body ()
             (maphash-values #'check-constraint *persistent-constraints*)
             violations))
      (if signal-constraint-violations
          (body)
          (handler-bind
              ((constraint-violation
                (lambda (error)
                  (push error violations)
                  (continue error))))
            (body))))))

(def method commit-transaction :before (database (transaction transaction-mixin))
  ;; TODO check here a slot of the transaction that is initialized to *ignore-constraints*
  (unless *ignore-constraints*
    (check-all-constraints)))
