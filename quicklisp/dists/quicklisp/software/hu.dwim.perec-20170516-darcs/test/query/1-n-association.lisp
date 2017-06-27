;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/select/association/1-n :in test/query/select/association))

(def macro with-parent-and-child-in-transaction (&body body)
  `(with-transaction
    (purge-instances 'child-test)
    (purge-instances 'parent-test)
    (bind ((parent (make-instance 'parent-test))
           (child (make-instance 'child-test)))
      ,@body)))

(def test test/query/select/association/1-n/1 ()
  (with-parent-and-child-in-transaction
    (is (null (select (p)
                (from (p parent-test))
                (where (eq p (parent-of child))))))
    (is (null (select (c)
                (from (c child-test))
                (where (member c (children-of parent))))))))

(def test test/query/select/association/1-n/2 ()
  (with-parent-and-child-in-transaction
    (is (equal (select (p)
                 (from (p parent-test))
                 (where (null (children-of p))))
               (list parent)))
    (is (equal (select (c)
                 (from (c child-test))
                 (where (null (parent-of c))))
               (list child)))))

(def test test/query/select/association/1-n/3 ()
  (with-parent-and-child-in-transaction
    (setf (parent-of child) parent)
    (is (equal (select (p)
                 (from (p parent-test))
                 (where (eq p (parent-of child))))
               (list parent)))
    (is (equal (select (c)
                 (from (c child-test))
                 (where (member c (children-of parent))))
               (list child)))))

(def test test/query/select/association/1-n/4 ()
  (with-parent-and-child-in-transaction
    (setf (parent-of child) parent)
    (is (null (select (p)
                (from (p parent-test))
                (where (null (children-of p))))))
    (is (null (select (c)
                (from (c child-test))
                (where (null (parent-of c))))))))
