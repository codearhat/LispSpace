;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/limit :in test/query))

(def persistent-class* limit-test ()
  ((int-attr :type integer-32)))

(def fixture limit-data
  (with-transaction
    (purge-instances 'limit-test)
    (iter (for i from 0 below 10)
          (make-instance 'limit-test :int-attr i)))
  (-body-))

(def definer limit-test (name (&rest args) &body body)
  `(def test ,name ,args
     (with-fixture limit-data
       (with-transaction
         ,@body))))

(def limit-test test/query/limit/sql/1 ()
  (is
   (equal
    (select ((int-attr-of o))
      (from (o limit-test))
      (offset 3)
      (limit 2))
    '(3 4))))

(def limit-test test/query/limit/sql/2 ()
  (bind ((offset 3)
         (limit 2))
    (is
     (equal
      (select ((int-attr-of o))
        (from (o limit-test))
        (offset offset)
        (limit limit))
      '(3 4)))))

(def limit-test test/query/limit/lisp/1 ()
  (is
   (equal
    (select ((int-attr-of o))
      (from (o limit-test))
      (where (evenp (int-attr-of o)))
      (offset 3)
      (limit 2))
    '(6 8))))

(def limit-test test/query/limit/scroll/1 ()
  (bind ((scroll (select (:result-type scroll)
                   ((int-attr-of o))
                   (from (o limit-test))
                   (offset 3)
                   (limit 2))))
    (setf (page-size scroll) 1)
    (is (= (page-count scroll) 2))
    (first-page! scroll)
    (is (equalp (elements scroll) #(3)))
    (next-page! scroll)
    (is (equalp (elements scroll) #(4)))))
