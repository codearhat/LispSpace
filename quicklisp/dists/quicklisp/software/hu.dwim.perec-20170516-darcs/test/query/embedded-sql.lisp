;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/embedded-sql :in test/query))

(def persistent-class* embedded-sql-test ()
  ((int-attr :type integer-32)
   (string-attr :type (text 10))))

(def fixture embedded-sql-data
  (with-transaction
    (purge-instances 'embedded-sql-test)
    (make-instance 'embedded-sql-test :int-attr 1 :string-attr "a")
    (make-instance 'embedded-sql-test :int-attr 2 :string-attr "a")
    (make-instance 'embedded-sql-test :int-attr 3 :string-attr "b"))
  (-body-))

(def definer embedded-sql-test (name (&rest args) &body body)
  `(def test ,name ,args
    (with-fixture embedded-sql-data
      (with-transaction
        ,@body))))

(def embedded-sql-test test/query/embedded-sql/where ()
  (is (equal
       (select ((int-attr-of o))
         (from (o embedded-sql-test))
         (where (and (eq (int-attr-of o)
                         (sql-text "(select max(_int_attr) from _embedded_sql_test)")))))
       '(3))))

(def embedded-sql-test test/query/embedded-sql/select-form ()
  (is (equal
       (select ((string-attr-of o) (sql-text "(select max(_int_attr) from _embedded_sql_test)"))
         (from (o embedded-sql-test))
         (order-by :ascending (int-attr-of o)))
       '(("a" 3) ("a" 3) ("b" 3)))))

(def embedded-sql-test test/query/embedded-sql/order-by ()
  (is (equal
       (select ((int-attr-of o))
         (from (o embedded-sql-test))
         (order-by :descending (sql-text "_o._int_attr")))
       '(3 2 1))))

(def embedded-sql-test test/query/embedded-sql/group-by ()
  (is (equal
       (select ((max (int-attr-of o)))
         (from (o embedded-sql-test))
         (group-by (sql-text "_o._string_attr"))
         (order-by :ascending (max (int-attr-of o))))
       '(2 3))))
