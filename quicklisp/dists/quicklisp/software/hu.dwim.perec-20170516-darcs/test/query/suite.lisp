;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite (test/query/select :in test/query))

(def special-variable *show-query* nil)

(def macro with-sql-recording (&body body)
  `(unwind-protect
    (progn
      (enable-sql-log)
      ,@body)
    (disable-sql-log)))

(def macro with-debug-query-compiler (&body body)
  `(let ((*test-query-compiler* #t))
     ,@body))

(def macro without-debug-query-compiler (&body body)
  `(let ((*test-query-compiler* #f))
     ,@body))

(def macro with-record-count-check (count &body body)
  (if count
      (with-unique-names (result)
        `(bind ((,result (progn ,@body)))
          (is (= (length ,result) ,count))
          ,result))
      `(progn ,@body)))

(def macro with-select-count-check (count &body body)
  (if count
      (with-unique-names (start result end)
        `(bind ((,start (hu.dwim.perec::select-counter-of (hu.dwim.rdbms::command-counter-of *transaction*)))
                (,result (progn ,@body))
                (,end (hu.dwim.perec::select-counter-of (hu.dwim.rdbms::command-counter-of *transaction*))))
          (is (= (- ,end ,start) ,count))
          ,result))
      `(progn ,@body)))

;; TODO use the :fixtures arg for def test
(def macro test-query ((&key (select-count 1) (record-count nil) (fixture nil) (with-expected-failures nil))
                        &body forms)
  (check-type with-expected-failures boolean)
  (bind ((body `(progn
                  (run-queries
                    (without-debug-query-compiler
                      (with-select-count-check ,select-count
                        (with-record-count-check ,record-count
                          ,@forms))))
                  (run-queries
                    (with-debug-query-compiler
                      ,@forms))))
         (expected-wrapper (if with-expected-failures
                               'with-expected-failures
                               'progn)))
    `(finishes
       (,expected-wrapper
        ,(if fixture
             `(with-fixture ,fixture
                ,body)
             body)))))

(def function run-query-tests ()
  (with-sql-recording
    (let ((*show-query* #t)
          (*debug-on-assertion-failure* #f)
          (*debug-on-unexpected-error* #f))
      (test/query))))

(def function debug-query-test (test)
  (with-sql-recording
    (let ((*show-query* #t))
      (funcall test))))

(def macro run-queries (&body queries)
  `(with-transaction
    (when *show-query*
      (format t "~{~&~A~}" ',queries))
    ,@queries))

(def function first-arg (arg-1 &rest rest-args)
  (declare (ignore rest-args))
  arg-1)
