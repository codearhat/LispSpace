;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(defsuite* (test/query/select-list :in test/query))

(defpclass* select-list-test ()
  ((string-attr :type (text 50))
   (or-null-string-attr :type (or null (text 50)))
   (date-attr :type date)))

(defixture select-list-data
  (with-transaction
    (purge-instances 'select-list-test)
    (make-instance 'select-list-test
                   :string-attr "string1"
                   :date-attr (parse-datestring "2007-07-11"))
    (make-instance 'select-list-test
                   :string-attr "String2"
                   :date-attr (parse-datestring "2007-07-15"))
    (-body-)))

(deftest test/query/select-list/select-columns/1 ()
  (test-query (:select-count 1 :record-count 1 :fixture select-list-data)
    (select ((string-attr-of o) (or-null-string-attr-of o))
      (from (o select-list-test))
      (where (timestamp<= (parse-datestring "2007-07-10")
                          (date-attr-of o)
                          (parse-datestring "2007-07-12"))))))

(deftest test/query/select-list/select-columns/2 ()
  (test-query (:select-count 1 :record-count 1 :fixture select-list-data)
    (select ((string-attr-of o) (date-attr-of o))
      (from (o select-list-test))
      (where (timestamp<= (parse-datestring "2007-07-10")
                          (date-attr-of o)
                          (parse-datestring "2007-07-12"))))))
