;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Copy persistent obejct

(define-copy-protocol copy-persistent-instance)

(define-copy-method (copy-self copy-persistent-instance) ((instance persistent-object))
  (bind ((class (class-of instance)))
    (make-instance class :persistent #f :oid (make-new-oid class))))

(define-copy-method (metacopy-with-contextl::copy-final copy-persistent-instance) ((instance persistent-object) (copy persistent-object))
  (ensure-persistent copy))

(define-copy-method (copy-inner-class copy-persistent-instance) progn ((instance persistent-object) (copy persistent-object) hash-table)
  (bind ((class (class-of instance)))
    (dolist (slot (class-slots class))
      (when (typep slot 'persistent-effective-slot-definition)
        (slot-boundp-or-value-using-class class instance slot
                                          [unless (unbound-slot-marker-p !1)
                                            (setf (slot-value-using-class class copy slot)
                                                  (copy-one !1 hash-table))])))))

(define-copy-method (copy-one copy-persistent-instance) ((value timestamp) htable)
  value)

;;;;;;
;;; Copy into transaction cache

(define-copy-protocol copy-into-transaction-cache)

(define-copy-method (copy-self copy-into-transaction-cache) ((instance persistent-object))
  (make-revived-instance (class-of instance) :oid (oid-of instance)))

(define-copy-method (metacopy-with-contextl::copy-final copy-into-transaction-cache) ((instance persistent-object) (copy persistent-object))
  (setf (cached-instance-of instance) (oid-of instance)))

(define-copy-method (copy-inner-class copy-into-transaction-cache) progn ((instance persistent-object) (copy persistent-object) hash-table)
  (bind ((class (class-of instance)))
    (dolist (slot (class-slots class))
      (cond ((typep slot 'persistent-effective-slot-definition)
             (bind (((:values cached-p value) (slot-value-cached-p instance slot)))
               (when cached-p
                 (setf (underlying-slot-boundp-or-value-using-class class copy slot)
                       (if (unbound-slot-marker-p value)
                           value
                           (copy-one value hash-table))))))
            ((typep slot 'closer-mop:standard-effective-slot-definition)
             (setf (closer-mop:standard-instance-access copy (closer-mop:slot-definition-location slot))
                   (closer-mop:standard-instance-access instance (closer-mop:slot-definition-location slot))))))))

(define-copy-method (copy-self copy-into-transaction-cache) ((instance timestamp))
  ;; TODO: shouldn't we treat non-transient local-time instances as immutable instead?
  (local-time::clone-timestamp instance))

(define-copy-method (copy-inner-class copy-into-transaction-cache) progn
  ((instance timestamp) (copy timestamp) hash-table))
