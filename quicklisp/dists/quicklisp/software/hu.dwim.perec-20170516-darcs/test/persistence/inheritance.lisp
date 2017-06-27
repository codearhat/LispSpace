;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/inheritance :in test/persistence))

(def persistent-class* inheritance-t1-test ()
  ((slot :type (text 20) :cache #f)))

(def persistent-class* inheritance-t2-test (inheritance-t1-test)
  ((slot :cache #t)))

(def test test/persistence/inheritance/store-value/1 ()
  (let ((value "hello"))
    (with-transaction
      (is (equal value (slot-of (make-instance 'inheritance-t1-test :slot value))))
      (is (equal value (slot-of (make-instance 'inheritance-t2-test :slot value)))))))

(def test test/persistence/inheritance/override/1 ()
  (is (not (cache-p (find-slot (find-class 'inheritance-t1-test) 'slot))))
  (is (cache-p (find-slot (find-class 'inheritance-t2-test) 'slot))))
