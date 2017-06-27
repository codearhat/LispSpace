;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def suite* (test/basic :in test/backend))

(def test test/basic/connect ()
  (finishes
    (with-transaction
      (execute "CREATE TEMPORARY TABLE tmp (col CHAR)")
      (execute "DROP TABLE tmp"))))

(def test test/basic/create-table ()
  (finishes
    (unwind-protect
         (execute-ddl "CREATE TABLE test_table (name varchar(40))")
      (ignore-errors
        (execute-ddl "DROP TABLE test_table")))))

(def test test/basic/encoding ()
  (bind ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl "CREATE TABLE test_table (name varchar(40))")
           (execute (format nil "INSERT INTO test_table VALUES ('~A')" unicode-text))
           (is (string= (first-elt (first-elt (execute "SELECT name FROM test_table"))) unicode-text)))
      (ignore-errors
        (execute-ddl "DROP TABLE test_table")))))

(def test test/basic/basic-binding ()
  (bind ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl [create table test_table ((name (varchar 50)))])
           (execute [insert test_table (name) ((,unicode-text varchar))])
           (is (string= (first-elt (first-elt (execute [select * test_table]))) unicode-text)))
      (ignore-errors
        (execute-ddl [drop table test_table])))))

(def test test/basic/basic-binding-with-string ()
  (bind ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl [create table test_table ((name (varchar 50)))])
           (execute "INSERT INTO test_table (name) VALUES ($1::CHARACTER VARYING)"
                    ;; TODO binding-types don't really have much effect, doesn't do type checking on the values, etc
                    :binding-types (vector (sql-character-varying-type))
                    :binding-values (vector unicode-text))
           (is (string= (first-elt (first-elt (execute [select * test_table]))) unicode-text)))
      (ignore-errors
        (execute-ddl [drop table test_table])))))

(def test test/basic/binding ()
  (bind ((columns (compile-sexp-sql-columns
                   `((a (integer 32))
                     (string_column (varchar 50))
                     (integer_column (integer 32))
                     (boolean_true_column boolean)
                     (boolean_false_column boolean)
                     (b (integer 32)))))
         (binding-literals (loop for entry :in `(("éáúóüőű" varchar)
                                                 (42 (integer 32))
                                                 (#t boolean)
                                                 (#f boolean))
                                 for value = (if (consp entry) (first entry) entry)
                                 for type = (when (consp entry) (second entry))
                                 for idx :upfrom 0
                                 collect (progn
                                           (setf type (if type
                                                          (compile-sexp-sql-type type)
                                                          (type-of (elt columns idx))))
                                           (make-instance 'sql-literal :value value :type type)))))
    (unwind-protect
         (with-transaction
           (execute-ddl [create table test_table ,columns])
           (execute [insert test_table
                            ,columns
                            ,(append (list (compile-sexp-sql-literal '(? named1 (integer 32))))
                                     binding-literals
                                     (list (compile-sexp-sql-literal '(? named2 (integer 32)))))]
                    :bindings `(named1 11
                                       named2 22))
           (execute [select ,columns test_table]
                    :visitor (let ((first-time #t))
                               (lambda (row)
                                 (let ((idx -1))
                                   (flet ((next ()
                                            (elt row (incf idx))))
                                     (is first-time)
                                     (setf first-time #f)
                                     (is (eql (next) 11))
                                     (is (string= (next) (value-of (first binding-literals))))
                                     (is (eql (next) (value-of (second binding-literals))))
                                     (is (eql (next) (value-of (third binding-literals))))
                                     (is (eql (next) (value-of (fourth binding-literals))))
                                     (is (eql (next) 22)))))))
           (signals unbound-binding-variable-error
             (execute [insert test_table
                              ,columns
                              ,(append (list (compile-sexp-sql-literal '(? named1 (integer 32))))
                                       binding-literals
                                       (list (compile-sexp-sql-literal '(? named2 (integer 32)))))])))
      (ignore-errors
        (execute-ddl [drop table test_table])))))

(def test test/basic/terminal-action ()
  (unwind-protect
       (progn
         (execute-ddl "CREATE TABLE test_table (x integer)")
         (with-transaction
           (execute "INSERT INTO test_table VALUES (42)")
           (is (= (first-elt (first-elt (execute "SELECT x FROM test_table"))) 42))
           (mark-transaction-for-rollback-only))
         (with-transaction
           (is (zerop (first-elt (first-elt (execute "SELECT count(*) FROM test_table"))))))
         (with-transaction
           (execute "INSERT INTO test_table VALUES (42)"))
         (with-transaction
           (is (= 1 (first-elt (first-elt (execute "SELECT count(*) FROM test_table")))))))
    (ignore-errors
      (execute-ddl "DROP TABLE test_table"))))

(def test test/basic/insert-record ()
  (unwind-protect
       (let ((columns (compile-sexp-sql-columns
                       `((a (integer 32))
                         (b (varchar 50))))))
         (create-table 'test_table columns)
         (with-transaction
           (insert-record 'test_table columns (list 1 "test_table"))
           (let ((row (first-elt (select-records columns '(test_table)))))
             (is (= (elt row 0) 1))
             (is (string= (elt row 1) "test_table")))))
    (ignore-errors
      (execute-ddl [drop table test_table]))))

(def test test/basic/update-records ()
  (unwind-protect
       (let ((columns (compile-sexp-sql-columns
                       `((a (integer 32))
                         (b (varchar 50))))))
         (create-table 'test_table columns)
         (with-transaction
           (execute [insert test_table (a b) (:null :null)])
           (is (eql 1 (update-records 'test_table columns (list 1 "test_table"))))
           (let ((row (first-elt (select-records columns '(test_table)))))
             (is (eql (elt row 0) 1))
             (is (string= (elt row 1) "test_table")))))
    (ignore-errors
      (execute-ddl [drop table test_table]))))

;; TODO THL this test must be independent of what backend i'm using right now
(def test test/basic/expand-sql-ast/binding ()
  (unwind-protect
       (with-expected-failures
         (with-transaction
           (execute-ddl [create table test_table ((a boolean))])
           (execute [insert test_table (a) (t)])
           (is (length= 1 (execute
                           (compile
                            nil
                            (hu.dwim.rdbms::expand-sql-ast-into-lambda-form-cached
                             (sql-select :columns '(a)
                                         :tables '(test_table)
                                         :where (sql-= (sql-identifier :name 'a)
                                                       (sql-unquote :form '(sql-binding-variable :name 'a :type (sql-boolean-type)))))))
                           :bindings '(a t))))
           (is (length= 1 (execute
                           (compile
                            nil
                            (hu.dwim.rdbms::expand-sql-ast-into-lambda-form-cached
                             (sql-select :columns '(a)
                                         :tables '(test_table)
                                         :where (sql-= (sql-identifier :name 'a)
                                                       (sql-binding-variable :name 'a
                                                                             :type
                                                                             (sql-unquote
                                                                               :form
                                                                               `(sql-boolean-type)))))))
                           :bindings '(a t))))))
    (ignore-errors
      (execute-ddl [drop table test_table]))))
