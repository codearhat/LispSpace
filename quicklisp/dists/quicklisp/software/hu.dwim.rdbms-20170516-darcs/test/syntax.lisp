;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def function find-database-class (name)
  (if (eq name t)
      (find-class 'database)
      (bind ((package (find-package (string+ "HU.DWIM.RDBMS." (string-upcase name)))))
        (when package
          (bind ((class-name (find-symbol (string-upcase name) package)))
            (find-class class-name))))))

(def definer syntax-test (name database args &body body)
  `(def test ,name ,args
     (with-database (closer-mop:class-prototype (find-database-class ',database))
       ,@body)))

(def definer dialect-test (name kind &body body)
  `(def test ,name ()
     ,@(iter (for (sql string-or-case-bodies) :on body :by #'cddr)
             (collect `(is (equalp
                            ,(ecase kind
                               (:sexp  `(format-sql-to-string (compile-sexp-sql ,sql)))
                               (:ast   `(format-sql-to-string ,sql))
                               (:reader (once-only (sql)
                                          `(etypecase ,sql
                                             (string ,sql)
                                             (function (funcall ,sql))))))
                            ,(if (stringp string-or-case-bodies)
                                 string-or-case-bodies
                                 `(cond ,@(iter (for case-body :in string-or-case-bodies)
                                                (collect `((typep *database* (find-database-class ',(first case-body)))
                                                           ,@(rest case-body))))))))))))

(def definer sexp-sql-dialect-test (name &body body)
  `(def dialect-test ,name :sexp ,@body))

(def definer ast-dialect-test (name &body body)
  `(def dialect-test ,name :ast ,@body))

(def definer reader-dialect-test (name &body body)
  `(def dialect-test ,name :reader ,@body))

(def suite* (test/syntax :in test/backend))

(def sexp-sql-dialect-test test/syntax/sexp-dialect
  '(select "bar" table)
  ((oracle "SELECT \"bar\" FROM \"table\"")
   (t "SELECT bar FROM table"))

  '(select (count *) _table)
  ((oracle "SELECT count(*) FROM \"_table\"")
   (t "SELECT count(*) FROM _table"))

  '(select ((foo.col1 "col1_alias") "bar") table)
  ((oracle "SELECT \"foo\".\"col1\" AS \"col1_alias\", \"bar\" FROM \"table\"")
   (t "SELECT foo.col1 AS col1_alias, bar FROM table"))

  `(select
    (foo.column "bar")
    ,(list (sql-table-alias :name "test_table" :alias "test_table_alias")))
  ((oracle "SELECT \"foo\".\"column\", \"bar\" FROM \"test_table\" \"test_table_alias\"")
   (t "SELECT foo.column, bar FROM test_table test_table_alias"))

  '(create table (:temporary :drop) test_table ((col1 varchar) ("col2" (integer 32))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE \"test_table\" (\"col1\" VARCHAR2, \"col2\" NUMBER(10,0)) ON COMMIT DROP")
   (postgresql "CREATE GLOBAL TEMPORARY TABLE test_table (col1 CHARACTER VARYING, col2 INT) ON COMMIT DROP")
   (sqlite "CREATE TEMPORARY TABLE test_table (col1 CHARACTER VARYING, col2 INTEGER) ON COMMIT DROP"))

  '(create table (:temporary :delete-rows) test_table (("col2" (integer 32))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE \"test_table\" (\"col2\" NUMBER(10,0)) ON COMMIT DELETE ROWS")
   (postgresql "CREATE GLOBAL TEMPORARY TABLE test_table (col2 INT) ON COMMIT DELETE ROWS")
   (sqlite "CREATE TEMPORARY TABLE test_table (col2 INTEGER) ON COMMIT DELETE ROWS")))

(def reader-dialect-test test/syntax/sql-reader
  [select "bar" table]
  ((oracle "SELECT \"bar\" FROM \"table\"")
   (t "SELECT bar FROM table"))

  (bind ((column-1 (sql-column-alias :table t :column 'col1 :alias "foo"))
         (column-2 (sql-column-alias :table t :column 'col2 :alias "bar")))
    [select (,column-1 ,column-2) table])
  ((oracle "SELECT \"t\".\"col1\" AS \"foo\", \"t\".\"col2\" AS \"bar\" FROM \"table\"")
   (t "SELECT t.col1 AS foo, t.col2 AS bar FROM table"))

  (bind ((columns (list (sql-column :name 'col1 :type (sql-integer-type))
                        (sql-column :name 'col2 :type (sql-character-varying-type))
                        (sql-identifier :name 'col3)
                        (sql-identifier :name 'col4))))
    [insert t ,columns (42
                        ,(sql-literal :value (string-upcase "some random text")
                                      :type (sql-character-varying-type))
                        ,(sql-binding-variable :name 'dynamic-binding
                                               :type (sql-character-varying-type)))])
  ((oracle "INSERT INTO \"t\" (\"col1\", \"col2\", \"col3\", \"col4\") VALUES (42, :1, :2)") ;; TODO THL and the type spec ars?
   (sqlite "INSERT INTO t (col1, col2, col3, col4) VALUES (42, :1, :2)")
   (postgresql "INSERT INTO t (col1, col2, col3, col4) VALUES (42, $1::CHARACTER VARYING, $2::CHARACTER VARYING)")))

;; TODO THL this test must be independent of what backend i'm using right now
(def syntax-test test/syntax/expand-sql-ast/unquote/1 postgresql (&optional (n 3))
  ;; "SELECT a, b FROM t WHERE (t.b OR t.b OR t.b)"
  (bind ((expected (format nil "SELECT a, b FROM t WHERE (~A)"
                           (apply 'string+
                                  (iter (for i :from 1 :to n)
                                        (unless (first-iteration-p)
                                          (collect " OR "))
                                        (collect "t.b"))))))
    ;; advanced use of the reader: the criteria is generated into a variable.
    ;; which means that it could even be the input of this function.
    (bind ((criteria [or ,@(iter (repeat (1- n))
                                 (collect (sql-column-alias :table 't :column 'b)))
                         ,(sql-column-alias :table 't :column 'b)]))
      (is (string= expected
                   (funcall [select (a b) t ,criteria]))))

    ;; building the AST by hand
    (bind ((criteria (sql-unquote
                       :form
                       `(apply 'sql-or
                               (iter (repeat ,n)
                                     (collect ,(sql-column-alias :table 't :column 'b)))))))
      (with-expected-failures
        (is (string=
             expected
             (funcall
              (compile
               nil
               (hu.dwim.rdbms::expand-sql-ast-into-lambda-form-cached
                (sql-select :columns '(a b)
                            :tables '(t)
                            :where criteria))))))))))

;; TODO THL this test must be independent of what backend i'm using right now
(def syntax-test test/syntax/expand-sql-ast/unquote/2 postgresql (&optional (n 3))
  ;; "SELECT a, b FROM t WHERE ((a = (b + $1::NUMERIC + 1)) OR (a = (b + $2::NUMERIC + 2)) OR (a = (b + $3::NUMERIC + 3)))"
  (bind ((expected (format nil "SELECT a, b FROM t WHERE (~A)"
                           (apply 'string+
                                  (iter (for i :from 1 :to n)
                                        (unless (first-iteration-p)
                                          (collect " OR "))
                                        (collect (format nil "(a = (b + $~d::NUMERIC + ~d))" i i)))))))
    ;; using the [] reader
    (bind ((criteria [or ,@(iter (for i :from 1 :to n)
                                 (rebind (i)
                                   (collect (sql-= (sql-identifier :name 'a)
                                                   (sql-+ (sql-identifier :name 'b)
                                                          (sql-binding-variable
                                                            :type (sql-integer-type)
                                                            :name i)
                                                          i)))))]))
      (is (string=
           expected
           (funcall
            [select (a b) t ,criteria]))))

    ;; building the AST by hand
    (bind ((criteria (sql-unquote
                       :form
                       `(apply 'sql-or
                               (iter (for i :from 1 :to ,n)
                                     (rebind (i)
                                       (collect
                                         ,(sql-= (sql-identifier :name 'a)
                                                 (sql-+ (sql-identifier :name 'b)
                                                        (sql-unquote :form '(sql-binding-variable
                                                                             :type (sql-integer-type)
                                                                             :name i))
                                                        (sql-unquote :form '(sql-literal :value i)))))))))))
      (is (string=
           expected
           (funcall
            (compile
             nil
             (with-expected-failures
               (hu.dwim.rdbms::expand-sql-ast-into-lambda-form
                (sql-select :columns '(a b)
                            :tables '(t)
                            :where criteria))))))))))

;; TODO THL this test must be independent of what backend i'm using right now
(def syntax-test test/syntax/expand-sql-ast/unquote/3 postgresql (&optional (n 3))
  (bind ((criteria [or ,@(loop for i from 1 to n
                               collect [= a
                                             (+ b
                                                ,(sql-binding-variable
                                                  :type (sql-integer-type)
                                                  :name (format-symbol (find-package :hu.dwim.rdbms.test) "VAR-~A" i))
                                                ,i)])])
         (extra-columns '(c d)))
    (bind (((:values command binding-variables binding-types binding-values)
            (funcall [select (a b ,@extra-columns)
                             table
                             (and (= a 42)
                                  (not (= b ,(sql-literal :type (sql-integer-type)
                                                          :value 43)))
                                  ,criteria)])))
      (is (string= command "SELECT a, b, c, d FROM table WHERE ((a = 42) AND (NOT (b = $1::NUMERIC)) AND ((a = (b + $2::NUMERIC + 1)) OR (a = (b + $3::NUMERIC + 2)) OR (a = (b + $4::NUMERIC + 3))))"))
      (is (null (first-elt binding-variables)))
      (is (every (rcurry #'typep 'sql-binding-variable) (subseq binding-variables 1)))
      (is (every (rcurry #'typep 'sql-integer-type) binding-types))
      (is (eql (first-elt binding-values) 43))
      (is (every #'null (subseq binding-values 1))))))

(def suite* (test/format :in test/backend))

(def ast-dialect-test test/syntax/format/identifier
  (sql-identifier :name "test_table")
  ((oracle "\"test_table\"")
   (t "test_table"))

  (sql-identifier :name 'test_table)
  ((oracle "\"test_table\"")
   (t "test_table")))

(def ast-dialect-test test/syntax/format/create-table
  (sql-create-table :name "a"
                    :columns (list (sql-column :name "a"
                                               :type (sql-integer-type))))
  ((oracle "CREATE TABLE \"a\" (\"a\" NUMBER(*,0))")
   (sqlite "CREATE TABLE a (a INTEGER)")
   (t "CREATE TABLE a (a NUMERIC)"))

  (sql-create-table :temporary :drop
                    :name "a"
                    :columns (list (sql-column :name "a"
                                               :type (sql-integer-type))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE \"a\" (\"a\" NUMBER(*,0)) ON COMMIT DROP")
   (sqlite "CREATE TEMPORARY TABLE a (a INTEGER) ON COMMIT DROP")
   (t "CREATE GLOBAL TEMPORARY TABLE a (a NUMERIC) ON COMMIT DROP")))

(def ast-dialect-test test/syntax/format/alter-table
  (sql-alter-table :name "a"
                   :actions (list (sql-add-column-action :name "a"
                                                         :type (sql-integer-type))))
  ((oracle "ALTER TABLE \"a\" ADD (\"a\" NUMBER(*,0))")
   (sqlite "ALTER TABLE a ADD (a INTEGER)")
   (t "ALTER TABLE a ADD a NUMERIC"))

  (sql-alter-table :name "a"
                   :actions (list (sql-alter-column-type-action :name "a"
                                                                :type (sql-integer-type))))
  ((oracle "ALTER TABLE \"a\" MODIFY (\"a\" NUMBER(*,0))")
   (sqlite "ALTER TABLE a ALTER COLUMN a TYPE INTEGER")
   (t "ALTER TABLE a ALTER COLUMN a TYPE NUMERIC"))

  (sql-alter-table :name "a"
                   :actions (list (sql-drop-column-action :name "a")))
  ((oracle "ALTER TABLE \"a\" DROP COLUMN \"a\"")
   (t "ALTER TABLE a DROP COLUMN a")))

(def ast-dialect-test test/syntax/format/drop-table
  (sql-drop-table :name "a")
  ((oracle "DROP TABLE \"a\"")
   (t "DROP TABLE a")))

(def ast-dialect-test test/syntax/format/create-index
  (sql-create-index :name "a"
                    :table-name "a"
                    :columns (list "a" "a"))
  ((oracle "CREATE INDEX \"a\" ON \"a\" (\"a\", \"a\")")
   (t "CREATE INDEX a ON a (a, a)")))

(def ast-dialect-test test/syntax/format/drop-index
  (sql-drop-index :name "a")
  ((oracle "DROP INDEX \"a\"")
   (t "DROP INDEX a")))

(def ast-dialect-test test/syntax/format/insert
  (sql-insert :table "a"
              :columns (list "a")
              :values (list "a"))
  ((oracle "INSERT INTO \"a\" (\"a\") VALUES ('a')")
   (t "INSERT INTO a (a) VALUES ('a')"))

  (sql-insert :table (sql-identifier :name "a")
              :columns (list (sql-identifier :name "a"))
              :values (list "a"))
  ((oracle "INSERT INTO \"a\" (\"a\") VALUES ('a')")
   (t "INSERT INTO a (a) VALUES ('a')")))

(def ast-dialect-test test/syntax/format/select
  (sql-select :columns (list "a")
              :tables (list "a"))
  ((oracle "SELECT \"a\" FROM \"a\"")
   (t "SELECT a FROM a"))

  (sql-select :columns (list (sql-all-columns))
              :tables (list "a"))
  ((oracle "SELECT * FROM \"a\"")
   (t "SELECT * FROM a"))

  (sql-select :columns (list "a")
              :tables (list "a")
              :where (sql-binary-operator :name '=
                                          :left (sql-identifier :name "a")
                                          :right (sql-identifier :name "b")))
  ((oracle "SELECT \"a\" FROM \"a\" WHERE (\"a\" = \"b\")")
   (t "SELECT a FROM a WHERE (a = b)"))

  (sql-select :columns (list (sql-identifier :name "a"))
              :tables (list (sql-identifier :name "a")))
  ((oracle "SELECT \"a\" FROM \"a\"")
   (t "SELECT a FROM a"))

  (sql-select :columns (list (sql-column-alias :column "a" :table "b" :alias "c"))
              :tables (list (sql-table-alias :name "a" :alias "b")))
  ((oracle "SELECT \"b\".\"a\" AS \"c\" FROM \"a\" \"b\"")
   (t "SELECT b.a AS c FROM a b")))

(def ast-dialect-test test/syntax/format/update
  (sql-update :table "a"
              :columns (list "a")
              :values (list "a"))
  ((oracle "UPDATE \"a\" SET \"a\" = 'a'")
   (t "UPDATE a SET a = 'a'"))

  (sql-update :table (sql-identifier :name "a")
              :columns (list (sql-identifier :name "a"))
              :values (list "a"))
  ((oracle "UPDATE \"a\" SET \"a\" = 'a'")
   (t "UPDATE a SET a = 'a'")))

(def ast-dialect-test test/syntax/format/delete
  (sql-delete :table "a")
  ((oracle "DELETE from \"a\"")
   (t "DELETE from a"))

  (sql-delete :table (make-instance 'sql-identifier :name "a"))
  ((oracle "DELETE from \"a\"")
   (t "DELETE from a")))

(def ast-dialect-test test/syntax/format/sequence
  (sql-create-sequence :name "a")
  ((oracle "CREATE SEQUENCE \"a\"")
   (t "CREATE SEQUENCE a"))

  (sql-drop-sequence :name "a")
  ((oracle "DROP SEQUENCE \"a\"")
   (t "DROP SEQUENCE a"))

  (sql-select :columns (list (sql-sequence-nextval-column :name "a")))
  ((oracle "SELECT \"a\".nextval FROM dual ")
   (t "SELECT NEXTVAL('a')")))

(def ast-dialect-test test/syntax/format/lock
  (sql-lock-table :table "a"
                  :mode :share
                  :wait #f)
  ((oracle "LOCK TABLE \"a\" IN SHARE MODE NOWAIT")
   (t "LOCK TABLE a IN SHARE MODE NOWAIT")))
