;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/expression :in test/query))

(def persistent-class* expression-test ()
  ((string-attr :type (text 50))
   (or-null-string-attr :type (or null (text 50)))
   (date-attr :type date)))

(def fixture expression-data
  (with-transaction
    (purge-instances 'expression-test)
    
    (make-instance 'expression-test
                   :string-attr "string1"
                   :date-attr (parse-datestring "2007-07-11"))
    (make-instance 'expression-test
                   :string-attr "String2"
                   :date-attr (parse-datestring "2007-07-15")))
  (-body-))

(def test test/query/expression/timestamp<= ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (timestamp<= (parse-datestring "2007-07-10")
                          (date-attr-of o)
                          (parse-datestring "2007-07-12"))))))

(def test test/query/expression/like-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (string-attr-of o) "s%ng_")))))

(def test test/query/expression/like-2 ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (string-attr-of o) "r%g" :start 2 :end 6)))))

(def test test/query/expression/like-3 ()
  (test-query (:select-count 1 :record-count 0 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (or-null-string-attr-of o) "s%ng_")))))

(def test test/query/expression/like-ci ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (like (string-attr-of o) "s%ng_" :case-sensitive-p #f)))))

(def test test/query/expression/re-like-1 ()
  (test-query (:select-count 1 :record-count 1 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (re-like (string-attr-of o) "s.+ngx?y*.")))))

(def test test/query/expression/re-like-2 ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (re-like (string-attr-of o) "r.+gx?y*" :start 2 :end 6)))))

(def test test/query/expression/re-like-ci ()
  (test-query (:select-count 1 :record-count 2 :fixture expression-data)
    (select (o)
      (from (o expression-test))
      (where (re-like (string-attr-of o) "s.+ngx?y*." :case-sensitive-p #f)))))
