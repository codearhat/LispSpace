;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/table-ref :in test/query))

(def persistent-class* abstract-test ()
  ()
  (:abstract #t))

(def persistent-class* super-1-test ()
  ())

(def persistent-class* super-2-test ()
  ())

(def persistent-class* sub-1-test (super-1-test super-2-test)
  ())

(def persistent-class* sub-2-test (super-1-test super-2-test)
  ())

(def fixture create-type-test-data
  (with-transaction
    (purge-instances 'super-1-test)
    (purge-instances 'super-2-test)
    (purge-instances 'sub-1-test)
    (purge-instances 'sub-2-test)
    (make-instance 'sub-1-test)
    (make-instance 'sub-2-test))
  (-body-))

(def macro type-test (&body body)
  `(with-fixture create-type-test-data
     (finishes
       (run-queries
         ,@body))))

(def test test/query/table-ref/none ()
  (type-test
    (select (o)
      (from o))))

(def test test/query/table-ref/and-self ()
  (type-test
    (select (o)
      (from o)
      (where (and
              (typep o 'super-1-test)
              (typep o 'super-1-test))))))

(def test test/query/table-ref/and-supers ()
  (type-test
    (select (o)
      (from o)
      (where (and
              (typep o 'super-1-test)
              (typep o 'super-2-test))))))

(def test test/query/table-ref/or-supers ()
  (type-test
    (select (o)
      (from o)
      (where (typep o '(or super-1-test super-2-test))))))

(def test test/query/table-ref/abstract ()
  (type-test
   (select (o)
     (from (o abstract-test)))))
