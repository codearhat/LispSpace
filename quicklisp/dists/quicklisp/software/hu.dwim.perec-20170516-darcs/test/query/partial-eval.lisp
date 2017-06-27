;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/partial-eval :in test/query))

(def persistent-class* partial-eval-test ()
  ((int-attr :type integer-32)))

(def fixture partial-eval-data
  (with-transaction
    (purge-instances 'partial-eval-test)
    (make-instance 'partial-eval-test :int-attr 1)
    (make-instance 'partial-eval-test :int-attr 2))
  (-body-))

(def special-variable *counter* 0)

(def function count-one ()
  (incf *counter*))

(def test test/query/partial-eval/static ()
  (with-fixture partial-eval-data
    (bind ((*enable-partial-eval* #t) 
           (query (make-query '(select ((int-attr-of o))
                                (from (o partial-eval-test))
                                (where (= (count-one) (int-attr-of o))))))
           (*counter* 0))
      (with-transaction
        (compile-query query)
        (is (= *counter* 1))
        (is (equal (first (execute-query query)) 1))
        (is (equal (first (execute-query query)) 1))))))

(def test test/query/partial-eval/volatile ()
  (with-fixture partial-eval-data
    (bind ((*enable-partial-eval* #t)
           (query (make-query '(select ((int-attr-of o))
                                (from (o partial-eval-test))
                                (where (= (volatile (count-one)) (int-attr-of o))))))
           (*counter* 0))
      (with-transaction
        (compile-query query)
        (is (= *counter* 0))
        (is (equal (first (execute-query query)) 1))
        (is (equal (first (execute-query query)) 2))))))
