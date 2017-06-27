;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;; with-database, open-database, close-database, with-transaction, with-transaction*, begin, commit, rollback are all inherited from hu.dwim.rdbms

;; TODO rename to perec-database-mixin (don't export something called database-mixin...)
(def (class* e) database-mixin ()
  ((oid-instance-id-sequence-exists #f :type boolean)))

(def method oid-instance-id-sequence-exists-p ((self standard-object))
  (error "The required hu.dwim.perec:database-mixin is missing from *database* (~A) which is of class ~S" *database* (class-of *database*)))

(def method transaction-mixin-class list ((self database-mixin))
  'transaction-mixin)

(def (class* e) transaction-mixin (transaction-instance-cache-mixin)
  ())

(def method cleanup-transaction ((transaction transaction-mixin))
  (unwind-protect
       (call-next-method)
    (map-cached-instances (lambda (instance)
                            (debug-only (assert (eq (transaction-of instance) transaction)))
                            (setf (transaction-of instance) nil)))))

(def function instance-in-transaction-p (instance)
  "Returns true iff the instance is attached to a transaction which is in progress."
  (bind ((transaction (transaction-of instance)))
    (debug-only
      (assert (or (null transaction)
                  (transaction-in-progress-p transaction))))
    (not (null transaction))))

(def function instance-in-current-transaction-p (instance)
  "Returns true iff the instance is attached to the current transaction which is in progress."
  (and (in-transaction-p)
       (eq (transaction-of instance) *transaction*)))

(def generic before-committing-instance (transaction instance transaction-event))

(def generic after-instance-committed (transaction instance transaction-event))

(def method commit-transaction :around (database (transaction transaction-mixin))
  (map-created-instances (lambda (instance)
                           (before-committing-instance transaction instance :created)))
  (map-modified-instances (lambda (instance)
                            (before-committing-instance transaction instance :modified)))
  (map-deleted-instances (lambda (instance)
                           (before-committing-instance transaction instance :deleted)))
  (call-next-method)
  (map-created-instances (lambda (instance)
                           (after-instance-committed transaction instance :created)))
  (map-modified-instances (lambda (instance)
                            (after-instance-committed transaction instance :modified)))
  (map-deleted-instances (lambda (instance)
                           (after-instance-committed transaction instance :deleted))))
