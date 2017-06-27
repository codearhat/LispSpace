;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/polymorphic :in test/query) )

(def persistent-class* pt-0-test ()
  ())

(def persistent-class* pt-1-test ()
  ((attr1 :type integer)))

(def persistent-class* pt-2-test ()
  ((attr1 :type string)))

(def fixture polymorph-test-data
  (with-transaction
    (purge-instances 'pt-1-test)
    (purge-instances 'pt-2-test)
    (make-instance 'pt-1-test :attr1 1)
    (make-instance 'pt-2-test :attr1 "1"))
  (-body-))

(def test test/query/select/polymorph-1 ()
  (test-query (:select-count 0 :record-count 0 :fixture polymorph-test-data)
    (bind ((*enable-partial-eval* #t))
      (select (o)
       (from (o pt-0-test))
       (where (or (and (typep o 'pt-1-test)
                       (= (attr1-of o) 1))
                  (and (typep o 'pt-2-test)
                       (string= (attr1-of o) "1"))))))))

(def test test/query/select/polymorph-2 ()
  (test-query (:select-count nil :record-count 2 :fixture polymorph-test-data) ;; TODO select-count
    (select (o)
      (from o)
      (where (or (and (typep o 'pt-1-test)
                       (= (attr1-of o) 1))
                  (and (typep o 'pt-2-test)
                       (string= (attr1-of o) "1")))))))
