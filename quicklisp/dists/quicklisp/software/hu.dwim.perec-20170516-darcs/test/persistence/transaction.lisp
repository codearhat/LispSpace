;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/transaction :in test/persistence))

(def persistent-class* transaction-test ()
  ((name nil :type t)))

(def test test/persistence/transaction/return-value/1 ()
  (is (eq 2 (with-transaction 1 2))))
    
(def test test/persistence/transaction/in-transaction/1 ()
  (with-transaction
    (is (instance-in-transaction-p (make-instance 'transaction-test)))))

(def test test/persistence/transaction/in-transaction/2 ()
  (is (not (instance-in-transaction-p
            (with-transaction
              (make-instance 'transaction-test))))))

(def test test/persistence/transaction/revive-instance/1 ()
  (let ((object
         (with-transaction
           (make-instance 'transaction-test))))
    (finishes
      (with-transaction
        (revive-instance object)
        (name-of object)))))

(def test test/persistence/transaction/revive-instance/2 ()
  (let ((object
         (with-transaction
           (make-instance 'transaction-test))))
    (signals error
      (with-transaction
        (name-of object)))))
