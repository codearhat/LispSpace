;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/update :in test/query))

(def persistent-class* update-1-test ()
  ((int-attr :type integer-32)
   (bool-attr #f :type boolean)))

(def persistent-class* update-2-test ()
  ((int-attr :type integer-32)))

(def fixture update-query-fixture
  (with-transaction
    (purge-instances 'update-1-test)
    (purge-instances 'update-2-test)
    (make-instance 'update-1-test :int-attr 0)
    (make-instance 'update-1-test :int-attr 1)
    (make-instance 'update-2-test :int-attr 0))
  (-body-))

(def macro run-update-test (&body body)
  `(with-fixture update-query-fixture
     (with-transaction* (:default-terminal-action :rollback)
       (when *show-query*
         (format t "~{~&~A~}" ',body))
       ,@body)))

(def function check-database-content (expected)
  (bind ((content (select ((int-attr-of instance))
                    (from (instance update-1-test))
                    (order-by :ascending (int-attr-of instance)))))
    (is (equal content expected))))

(def test test/query/update/all ()
  (run-update-test
    (is (= 2
           (update (instance update-1-test)
             (set (int-attr-of instance) 2))))
    (check-database-content '(2 2))))

(def test test/query/update/one ()
  (run-update-test
    (is (= 1
           (update (instance update-1-test)
             (set (int-attr-of instance) 2)
             (where (= (int-attr-of instance) 0)))))
    (check-database-content '(1 2))))

(def test test/query/update/boolean ()
  (run-update-test
    (update (instance update-1-test)
      (set (bool-attr-p instance) #t))
    (is (= 0 (first
              (select ((count instance))
                (from (instance update-1-test))
                (where (eql (bool-attr-p instance) #f))))))
    (is (= 2 (first
              (select ((count instance))
                (from (instance update-1-test))
                (where (eql (bool-attr-p instance) #t))))))
    (is (= 1
           (update (instance update-1-test)
             (set (bool-attr-p instance) #t)
             (where (= (int-attr-of instance) 0)))))))

(def test test/query/update/joined ()
  (run-update-test
    (is (= 1
           (update (instance update-1-test)
             (set (int-attr-of instance) 2)
             (from (instance2 update-2-test))
             (where (= (int-attr-of instance) (int-attr-of instance2))))))
    (check-database-content '(1 2))))
