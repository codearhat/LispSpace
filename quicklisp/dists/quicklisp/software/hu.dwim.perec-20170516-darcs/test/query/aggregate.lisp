;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/aggregate :in test/query))

(def persistent-class* aggregate-test ()
  ((int-attr :type (or null integer-32))
   (str-attr :type (or unbound null (text 50)))
   (date-attr :type (or unbound null date))))

(def fixture aggregate-data
  (with-transaction
    (purge-instances 'aggregate-test)
    (make-instance 'aggregate-test
                   :int-attr 1
                   :str-attr "1"
                   :date-attr (parse-datestring "2001-01-01"))
    (make-instance 'aggregate-test
                   :int-attr 2
                   :str-attr "2"
                   :date-attr (parse-datestring "2001-01-02"))
    (make-instance 'aggregate-test
                   :int-attr 3
                   :str-attr "3"
                   :date-attr (parse-datestring "2001-01-03"))
    (make-instance 'aggregate-test
                   :int-attr nil
                   :str-attr nil
                   :date-attr nil)
    (make-instance 'aggregate-test
                   :int-attr nil))
  (-body-))

(def definer aggregate-test (name (&rest args) &body body)
  `(def test ,name ,args
    (with-fixture aggregate-data
      (with-transaction
        ,@body))))

(def aggregate-test test/query/aggregate/int ()
  (is
   (equal
    (select ((count (int-attr-of o))
             (sum (int-attr-of o))
             (min (int-attr-of o))
             (max (int-attr-of o))
             (avg (int-attr-of o)))
      (from (o aggregate-test)))
    '((3 6 1 3 2)))))

(def aggregate-test test/query/aggregate/int/2 ()
  (is
   (equal
    (select ((+ (count (int-attr-of o))
                (sum (int-attr-of o))
                (min (int-attr-of o))
                (max (int-attr-of o))
                (avg (int-attr-of o))))
      (from (o aggregate-test)))
    '(15))))

(def aggregate-test test/query/aggregate/string ()
  (is
   (equal
    (select ((count (str-attr-of o))
             (min (str-attr-of o))
             (max (str-attr-of o)) )
      (from (o aggregate-test)))
    '((3 "1" "3")))))

(def aggregate-test test/query/aggregate/date ()
  (bind ((result (first (select ((count (date-attr-of o))
                                 (min (date-attr-of o))
                                 (max (date-attr-of o)))
                          (from (o aggregate-test))))))
    (is
     (and (= (first result) 3)
          (timestamp= (second result) (parse-datestring "2001-01-01"))
          (timestamp= (third result) (parse-datestring "2001-01-03"))))))

(def aggregate-test test/query/aggregate/empty ()
  (is
   (equal
    (select ((max (int-attr-of o)))
      (from (o aggregate-test))
      (where (> (int-attr-of o) 10)))
    '(nil))))
