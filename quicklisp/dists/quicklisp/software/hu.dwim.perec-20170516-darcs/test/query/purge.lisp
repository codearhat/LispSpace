;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/purge :in test/query))

(def function check-existing-records (&rest content)
  (iter (for (table-id expected-records) on content by 'cddr)
        (for table-name = (rdbms-name-for (format nil "purge-~d-test" table-id) :table))
        (for column-name = (rdbms-name-for (format nil "int-attr-~d" table-id) :column))
        ;; TODO: eliminate coerce
        (for records-in-database = (sort
                                    (apply 'concatenate 'list
                                           (coerce (select-records (list (hu.dwim.rdbms::sql-column :name column-name))
                                                                   (list (hu.dwim.rdbms::sql-table-alias :name table-name))) 'list))
                                    #'<=))
        (is (equal records-in-database expected-records)
            "Table ~S: expected ~S, but found ~S" table-name expected-records records-in-database)))

(def macro run-purge-test (&body body)
  `(with-fixture purge-query-fixture
     (with-transaction* (:default-terminal-action :rollback)
       (when *show-query*
         (format t "~{~&~A~}" ',body))
       ,@body)))

(def definer purge-query-test (name class-id attr-value &body expected)
  (bind ((class (read-from-string (format nil "purge-~d-test" class-id)))
         (accessor (read-from-string (format nil "int-attr-~d-of" class-id))))
    `(def test ,name ()
      (run-purge-test
        (purge (o)
          (from (o ,class))
          ,@(when attr-value (list `(where (= (,accessor o) ,attr-value)))))
        (apply 'check-existing-records ,(first expected))))))

;;;
;;;   0       1           * = abstract
;;;          / \
;;;         2   3
;;;          \ / \ 
;;;           4*  7
;;;          / \
;;;         5   6

(def persistent-class* purge-0-test ()
  ((int-attr-0 :type integer-32)))

(def persistent-class* purge-1-test ()
  ((int-attr-1 :type integer-32)))

(def persistent-class* purge-2-test (purge-1-test)
  ((int-attr-2 :type integer-32)))

(def persistent-class* purge-3-test (purge-1-test)
  ((int-attr-3 :type integer-32)))

(def persistent-class* purge-4-test (purge-2-test purge-3-test)
  ()
  (:abstract #t))

(def persistent-class* purge-5-test (purge-4-test)
  ((int-attr-5 :type integer-32)))

(def persistent-class* purge-6-test (purge-4-test)
  ((int-attr-6 :type integer-32)))

(def persistent-class* purge-7-test (purge-3-test)
  ((int-attr-7 :type integer-32)))

(def fixture purge-query-fixture
  (with-transaction
    (purge-instances 'purge-0-test)
    (purge-instances 'purge-1-test)
    (purge-instances 'purge-2-test)
    (purge-instances 'purge-3-test)
    (purge-instances 'purge-4-test)
    (purge-instances 'purge-5-test)
    (purge-instances 'purge-6-test)
    (purge-instances 'purge-7-test)
    (make-instance 'purge-0-test :int-attr-0 0)
    (make-instance 'purge-0-test :int-attr-0 1)
    (make-instance 'purge-1-test :int-attr-1 0)
    (make-instance 'purge-1-test :int-attr-1 1)
    (make-instance 'purge-2-test :int-attr-1 2 :int-attr-2 0)
    (make-instance 'purge-2-test :int-attr-1 3 :int-attr-2 1)
    (make-instance 'purge-3-test :int-attr-1 4 :int-attr-3 0)
    (make-instance 'purge-3-test :int-attr-1 5 :int-attr-3 1)
    (make-instance 'purge-5-test :int-attr-1 6 :int-attr-2 2 :int-attr-3 2 :int-attr-5 0)
    (make-instance 'purge-5-test :int-attr-1 7 :int-attr-2 3 :int-attr-3 3 :int-attr-5 1)
    (make-instance 'purge-6-test :int-attr-1 8 :int-attr-2 4 :int-attr-3 4 :int-attr-6 0)
    (make-instance 'purge-6-test :int-attr-1 9 :int-attr-2 5 :int-attr-3 5 :int-attr-6 1)
    (make-instance 'purge-7-test :int-attr-1 10 :int-attr-3 6 :int-attr-7 0)
    (make-instance 'purge-7-test :int-attr-1 11 :int-attr-3 7 :int-attr-7 1))
  (-body-))

(def purge-query-test test/query/purge/delete-0-all 0 nil
  `(0 nil
    1 (0 1 2 3 4 5 6 7 8 9 10 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-0-one 0 0
  `(0 (1)
    1 (0 1 2 3 4 5 6 7 8 9 10 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-1-all 1 nil
  '(0 (0 1) 1 nil 2 nil 3 nil 5 nil 6 nil 7 nil))

(def purge-query-test test/query/purge-delete-1-one 1 0
  '(0 (0 1)
    1 (1 2 3 4 5 6 7 8 9 10 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-2-all 2 nil
  '(0 (0 1)
    1 (0 1 4 5 10 11)
    2 nil
    3 (0 1 6 7)
    5 nil
    6 nil
    7 (0 1)))

(def purge-query-test test/query/purge/delete-2-one 2 0
  `(0 (0 1)
    1 (0 1 3 4 5 6 7 8 9 10 11)
    2 (1 2 3 4 5)
    3 (0 1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-3-all 3 nil
  `(0 (0 1)
    1 (0 1 2 3)
    2 (0 1)
    3 nil
    5 nil
    6 nil
    7 nil))

(def purge-query-test test/query/purge/delete-3-one 3 0
  `(0 (0 1)
    1 (0 1 2 3 5 6 7 8 9 10 11)
    2 (0 1 2 3 4 5)
    3 (1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-4-all 4 nil
  `(0 (0 1)
    1 (0 1 2 3 4 5 10 11)
    2 (0 1)
    3 (0 1 6 7)
    5 nil
    6 nil
    7 (0 1)))

(def purge-query-test test/query/purge/delete-5-all 5 nil
  `(0 (0 1)
    1 (0 1 2 3 4 5 8 9 10 11)
    2 (0 1 4 5)
    3 (0 1 4 5 6 7)
    5 nil
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-5-one 5 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 7 8 9 10 11)
    2 (0 1 3 4 5)
    3 (0 1 3 4 5 6 7)
    5 (1)
    6 (0 1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-6-all 6 nil
  `(0 (0 1)
    1 (0 1 2 3 4 5 6 7 10 11)
    2 (0 1 2 3)
    3 (0 1 2 3 6 7)
    5 (0 1)
    6 nil
    7 (0 1)))

(def purge-query-test test/query/purge/delete-6-one 6 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 6 7 9 10 11)
    2 (0 1 2 3 5)
    3 (0 1 2 3 5 6 7)
    5 (0 1)
    6 (1)
    7 (0 1)))

(def purge-query-test test/query/purge/delete-7-all 7 nil
  `(0 (0 1)
    1 (0 1 2 3 4 5 6 7 8 9)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5)
    5 (0 1)
    6 (0 1)
    7 nil))

(def purge-query-test test/query/purge/delete-7-one 7 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 6 7 8 9 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 7)
    5 (0 1)
    6 (0 1)
    7 (1)))
