;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/subselect :in test/query))

(def persistent-class* subselect-parent-test ()
  ())

(def persistent-class* subselect-child-test ()
  ((int-attr :type (or null integer-32))))

(def persistent-association*
  ((:class subselect-parent-test :slot children :type (set subselect-child-test))
   (:class subselect-child-test :slot parent :type subselect-parent-test)))

(def fixture subselect-data
  (with-transaction
    (purge-instances 'subselect-parent-test)
    (purge-instances 'subselect-child-test)
    (bind ((p1 (make-instance 'subselect-parent-test))
           (p2 (make-instance 'subselect-parent-test))
           (p3 (make-instance 'subselect-parent-test)))
      (make-instance 'subselect-child-test :parent p1 :int-attr 1)
      (make-instance 'subselect-child-test :parent p1 :int-attr 2)
      (make-instance 'subselect-child-test :parent p2 :int-attr 3)
      (make-instance 'subselect-child-test :parent p2 :int-attr 4)
      (make-instance 'subselect-child-test :parent p3 :int-attr 5)
      (make-instance 'subselect-child-test :parent p3 :int-attr 6)))
  (-body-))

(def definer subselect-test (name (&rest args) &body body)
  `(def test ,name ,args
     (with-fixture subselect-data
       (with-transaction
         ,@body))))

#+nil
(def subselect-test test/query/subselect/int ()
  (select (parent child)
    (from (parent subselect-parent-test) (child subselect-child-test))
    (where (and (eq (parent-of child) parent)
                (= (int-attr-of child) (select (max (int-attr-of c))
                                         (from (c subselect-child-test))
                                         (where (eq (parent-of c) parent))))))))
