;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/having :in test/query))

(def persistent-class* having-test ()
  ((int-attr :type (or null integer-32))
   (str-attr :type (or null (text 50)))))

(def fixture having-data
  (with-transaction
    (purge-instances 'having-test)
    (make-instance 'having-test
                   :int-attr 1
                   :str-attr "1")
    (make-instance 'having-test
                   :int-attr 2
                   :str-attr "2")
    (make-instance 'having-test
                   :int-attr 3
                   :str-attr "2")
    (make-instance 'having-test
                   :int-attr 4
                   :str-attr "2"))
  (-body-))

(def definer having-test (name (&rest args) &body body)
  `(def test ,name ,args
     (with-fixture having-data
       (with-transaction
         ,@body))))

(def having-test test/query/having/1 ()
  (is
   (equal
    (select ((count (int-attr-of o)))
      (from (o having-test))
      (group-by (str-attr-of o))
      (having (> (count (int-attr-of o)) 1)))
    '(3))))
