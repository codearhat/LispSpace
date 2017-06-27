;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/reference :in test/persistence))

(def persistent-class* referred-test ()
  ())

(def persistent-class* reference-test ()
  ((referred :type referred-test)))

(def persistent-class* reference-or-unbound-test ()
  ((referred :type (or unbound referred-test))))

(def persistent-class* reference-or-null-test ()
  ((referred :type (or null referred-test))))

(def persistent-class* reference-or-unbound-null-test ()
  ((referred :type (or unbound null referred-test))))

(def macro with-reference-or-null-transaction (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-test))
          (reference (make-instance 'reference-or-null-test)))
      (declare (ignorable referred reference))
      ,@body)))

(def test test/persistence/reference/class ()
  (let ((referred-slot (find-slot 'reference-test 'referred)))
    (is (primary-table-slot-p referred-slot))
    (is (data-table-slot-p referred-slot))
    (is (cache-p referred-slot))))

(def test test/persistence/reference/initial-value/1 ()
  (with-and-without-caching-slot-values
    (with-one-and-two-transactions (make-instance 'reference-or-null-test)
      (is (eq nil (referred-of -instance-))))))

(def test test/persistence/reference/initial-value/2 ()
  (with-and-without-caching-slot-values
    (with-one-and-two-transactions (make-instance 'reference-or-unbound-test)
      (is (not (slot-boundp -instance- 'referred))))))

(def test test/persistence/reference/initial-value/3 ()
  (with-and-without-caching-slot-values
    (with-one-and-two-transactions (make-instance 'reference-or-unbound-test)
      (is (not (slot-boundp -instance- 'referred))))))

(def test test/persistence/reference/store-value/1 ()
  (with-and-without-caching-slot-values
    (with-reference-or-null-transaction
      (setf (referred-of reference) referred)
      (is (eq referred (referred-of reference))))))
