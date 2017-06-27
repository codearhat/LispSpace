;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/cache :in test/query))

(def macro run-cache-test (&body body)
  `(with-fixture fill-data-3
     (with-transaction
       (with-new-compiled-query-cache
         (reset-compile-query-counter)
         (symbol-macrolet ((counter *compile-query-counter*))
           ,@body)))))

(def persistent-class* query-cache-test ()
  ((attr-1 :type integer-32)))

(def persistent-class* query-cache-2-test ()
  ((attr-1 :type integer-32)))

;; PORT:
(def fixture fill-data-3
  (with-transaction
    (purge-instances 'query-cache-test)
    (make-instance 'query-cache-test :attr-1 1)
    (make-instance 'query-cache-test :attr-1 2))
  (-body-))

(def test test/query/cache-1 ()
  (run-cache-test
    (is (= counter 0))
    (select (o) (from (o query-cache-test)))
    (is (= counter 1))
    (select (o) (from (o query-cache-test)))
    (is (= counter 1))))

(def test test/query/cache-2 ()
  (run-cache-test
    (bind ((query (make-query `(select (o) (from (o query-cache-test))))))
      (is (= counter 0))
      (execute-query query)
      (is (= counter 1))
      (add-assert query `(equal (attr-1-of o) 1))
      (execute-query query)
      (is (= counter 2)))))
   
(def test test/query/cache-3 ()
  (run-cache-test
    (bind ((query (make-query `(select (o) (from (o query-cache-test)))))
           (result (execute-query query)))
      (add-assert query `(equal (attr-1-of o) 1))
      (is (not (equal result (execute-query query)))))))

(def test test/query/cache-4 ()
  (with-fixture fill-data-3
    (bind ((class (find-class 'query-cache-2-test))
           (query (make-query '(select (o) (from (o query-cache-2-test))))))
      (reset-compile-query-counter)
      (with-new-compiled-query-cache
        (symbol-macrolet ((counter *compile-query-counter*))
          (with-transaction
            (is (= counter 0))
            (execute-query query)
            (is (= counter 1)))
          (ensure-class-using-class class
                                    (class-name class)
                                    :metaclass (class-of class)
                                    :direct-superclasses (class-direct-superclasses class)
                                    :direct-slots nil)
          (with-confirmed-destructive-schema-changes
            (ensure-exported class))
          (with-transaction
            (execute-query query)
            (is (= counter 2))))))))


