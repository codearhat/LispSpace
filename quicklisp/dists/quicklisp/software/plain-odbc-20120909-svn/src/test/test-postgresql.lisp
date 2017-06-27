;;; -*- Mode: lisp -*-

(in-package :test-plain-odbc)

(export '(run-pgsql-tests))


(defun pg-test-connection (conn)
  (let ((n (caar (exec-query conn "SELECT 1"))))
    (assert (= 1 n))))


 

(defun run-pgsql-tests (con)
  (dolist (sym '(
                 pgsql-type-test
                 pgsql-test1 
                 pgsql-test2 
                 pgsql-test3 
                 pgsql-test4 
                 pgsql-test5 
                 pgsql-test6
                 pgsql-test7
                 pgsql-test9 
                 pgsql-test10 
                 pgsql-test15 
                 pgsql-test20 
                 pgsql-test22 
                 pgsql-test23 
                 pgsql-test23b 
                 pgsql-test24
                 pgsql-test25
                 pgsql-test26
                 pgsql-test28 
                 ))
    (pprint sym)
    (funcall sym con)))

(defparameter *pgsql-type_test-ddl* "  

create table type_test
 (id int,
t_SMALLINT  SMALLINT,
t_integer integer,
t_BIGINT bigint,
t_real real,
t_DOUBLE_prec double precision,
t_decimal40_20 DECIMAL(40,20) ,

t_dATE date,

t_TIMESTAMP timestamp,
t_TIME 	time,

T_char char(255),
t_varchar varchar(255),
t_TEXT text,

t_bytea bytea,
t_blob lo)
")
(defun pgsql-type-test (con)
  (ignore-errors (exec-command con "drop table type_test"))
  (exec-command con *pgsql-type_test-ddl*)
  (exec-update con "insert into type_test (id) values(1)")
  (exec-update con "
  update type_test set
    t_smallint =255,
   t_integer = 256*256*256*127,
  t_BIGINT = (bigint '256')*256*256*256 *256*256*256*(bigint '127'),
  t_real =1.0e0/7.0e0,
  t_DOUBLE_prec = 1.0e0/7.0e0,
  t_decimal40_20 = '12345678901234567890.1234567890123456789',
  t_timestamp ='2004-6-25',
  /* t_TIMESTAMP timestamp,  */
  t_TIME 	= '12:56',
  T_char = 'abcdefghijkmlnop',
  t_varchar = 'abcdefghijklmnopqrstuvw'
/*  --  t_TINYBLOB = lpad('a',33000), 
  --  t_TINYTEXT lpad('a',33000),
  --  t_BLOB blob,
  --  t_TEXT text,
  --  t_MEDIUMBLOB mediumblob, 
  --  t_MEDIUMTEXT mediumtext, 
  --t_LONGBLOB longblob,
  --t_LONGTEXT = lpad('a',33000,'x') 
*/
  where id =1
")
  (commit con)
   (let ((stm (prepare-statement 
              con "update type_test set t_text=? where id =1" 
              '(:clob :in))))
     (exec-prepared-update stm 
                           (make-string 100001 :initial-element #\o)))
   (commit con)
   (let ((res (exec-query con "
    select t_smallint,
           t_integer,
           t_bigint,
           t_real,
           t_double_prec,
           t_decimal40_20,
           t_timestamp,
           t_time,
           t_char,
           t_varchar,
           t_text
        from type_test where id=1")))
     (assert (= (length res) 1))
     (setf res (first res))
     (assert (equal (nth 0 res) 255))
     (assert (equal (nth 1 res) (* 256 256 256 127)))
     (assert (equal (nth 2 res) (* 256 256 256 256  256 256 256 127)))
     ;; these are skipped since support for double in myslq is bad 
    (assert (< (abs (- (nth 3 res) (/  1 7.0))) 1e-7))
    (assert (equal (nth 4 res) (/  1 7d0)))
     ;; skipped because we do not have decimal as a datatype
     (assert (equal (nth 5 res) 12345678901234567890.1234567890123456789d0))
     (assert (equal (nth 6 res) (encode-universal-time  0 0 0 25 6 2004)))
     (assert (equal (nth 7 res) "12:56:00"))
     ;(assert (equal (nth 10 res)  "12:56:00"))
     ;; year is returend as  SQL_SMALLINT
     (assert (equal (nth 8 res) (rpad "abcdefghijkmlnop" 255)))
     (assert (equal (nth 9 res) "abcdefghijklmnopqrstuvw"))
     
     (assert (equal (nth 10 res) (make-string 100001 :initial-element #\o)))
     ))



(defun pgsql-test1 (con)
  (let ((filename (namestring (merge-pathnames "odbc-trace-test.log" *test-temp-dir* ))))
    (when (probe-file filename)
      (DELETE-FILE filename))
    (assert (not (probe-file filename)))
    (trace-connection con filename)
    (dotimes (i 5) (exec-query con "select 1"))
    (with-open-file (f filename :direction :input)
      (assert (> (file-length f) 500)))
    (untrace-connection con)
    ;(break)
    (DELETE-FILE filename)
    (exec-query con "select 1")
    (assert (not (probe-file filename)))))


(defun pgsql-test2 (con)
  (let ((str (make-funny-string 245)))
    (exec-update con "delete from type_test where id=99")
    (with-prepared-statement (stm con 
                                  "insert into type_test (id,t_varchar) values (99,?)"
                                  '(:string :in))
      (exec-prepared-update stm str))
  (with-prepared-statement (stm con 
                                "select t_varchar from type_test where id=99" )
    (let ((res (exec-prepared-query stm )))
      (assert (string= (caar res) str))))))


(defun pgsql-test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (exec-update con "delete from type_test where id=99")
    (with-prepared-statement (stm con 
                                  "insert into type_test (id,t_timestamp) values(99,?)"
                                  '(:date :in))
      (exec-prepared-update stm "3323283742"))
    (let ((res (exec-query con "select  t_timestamp + INTERVAL '1 DAY' 
                                from type_test where id =99")))
      (assert (equal (parse-integer (caar res)) (+ 86400 3323283742))))))

(defun pgsql-test4 (con)
  (exec-update con "delete from type_test where id=99")
  (with-prepared-statement (stm con "insert into type_test (id,t_double_prec) values(99,?)" 
                                '(:double :in))
    (exec-prepared-update stm 1.8d0))
  (let ((res (exec-query con "select t_double_prec+1 from type_test where id=99")))
    (assert (<= (abs (- (caar res) 2.8d0)) 1d-7))))

                                         

(defun pgsql-test5 (con)
  (exec-update con "delete from type_test")
  (commit con)
  (with-prepared-statement (stm con "insert into type_test (id,t_TEXT) values(?,?)" 
                                '(:integer :in) '(:clob :in))
    (let ((mp plain-odbc::*max-precision*))
      (dolist (len (list 0 1 2 3 4 5 900 9000 8192 8000 
                         (1- mp) 
                         mp 
                         (1+ mp)
                         (* 2 mp)
                         (1- (* 2 mp))
                         (1+ (* 2 mp))))
        (let ((string (make-funny-string len)))
          (exec-prepared-update stm len string)
          (let ((res (exec-query con (format nil "select t_text from type_test where id=~A" len))))
          (assert (equal res
                         (list (list string)))))))))
    (commit con)
    )


(defun pgsql-drop-test-proc (con proc args)
  (unless (zerop (caar (exec-query con (format nil 
                                               "
    select count(*) 
    from pg_proc
    where proname ='~A'
    " proc))))
    (pprint 
     (second 
      (MULTIPLE-VALUE-LIST
       (ignore-errors
         (exec-command con (format nil "drop function ~A ~A" proc args))))))))

(defun pgsql-drop-test-table (con proc)
  (unless (zerop (caar (exec-query con (format nil 
                                               "
    select count(*) 
    from pg_tables 
    where tablename ='~A'
    " proc))))
    (exec-command con (format nil "drop table ~A" proc))))




;; this fails since myodbc can not deal with out parameters
(defun pgsql-test6 (con)
  (pgsql-drop-test-proc con "test99" "(int,out int)")
  (exec-command con "
CREATE function test99 (p1 int, p2 out INT) returns integer as $$
     BEGIN
       p2:=p1+5;
     END;
    $$ LANGUAGE plpgsql STRICT IMMUTABLE;
  ")
  (commit con)
  (let ((stm (prepare-statement con "{call test99(?,?)}" 
                                '(:integer :in) 
                                '(:integer :out))))
    (assert (= 6 (first (exec-prepared-command stm 1))))
    (free-statement stm)))



(defun pgsql-test7 (con)
   (let ((*universal-time-to-date-dataype* 'universal-time-list)
         (*date-datatype-to-universal-time* 'list-universal-time)
         (*date-type-predicate* 'date-lisp-p))
     (let ((res (exec-query con "select cast(? as date) +interval '1 day'" 
                            '((:date 2005 4 5) :date))))
           (assert (equal res '(((:date 2005 4 6 0 0 0))))))))



;;; 
(defun pgsql-test9 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b TEXT)")
  (commit con)
  (with-prepared-statement (stm con "insert into test999 (a,b) values(?,?)" 
                                '(:integer :in) 
                                '(:clob :in))
    (let ((mp plain-odbc::*max-precision*))
      (dolist (len (list 0 1 2 3 4 5 900 9000 8192 8000 
                         (1- mp) 
                         mp 
                         (1+ mp)
                         (* 2 mp)
                         (1- (* 2 mp))
                         (1+ (* 2 mp))))
        (let ((string (make-funny-string len)))
          (exec-prepared-update stm len string)
          (let ((res (exec-query con (format nil "select b from test999 where a=~A" len))))
            (assert (equal res
                           (list (list string)))))))))
    (exec-command con "drop table test999")
    (commit con)
    )


(defun pgsql-test10 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b lo)")
  (commit con)
  (with-prepared-statement (stm con "insert into test999 (a,b) values(?,?)" 
                                '(:integer :in) 
                                '(:blob :in))
    (let ((mp plain-odbc::*max-precision*))
      (dolist (len (list 0 1 2 3 4 5 900 9000 8192 8000 
                         (1- mp) 
                         mp 
                         (1+ mp)
                         (* 2 mp)
                         (1- (* 2 mp))
                         (1+ (* 2 mp)))) 
        (let ((byte-vec (make-funny-bytes len)))
          (exec-prepared-update stm len byte-vec)
          (let ((res (exec-query con (format nil "select b from test999 where a=~A" len))))
          (assert (equalp res
                         (list (list byte-vec)))))))))
    (exec-command con "drop table test999")
    (commit con)
    )


(defun pgsql-test15(con)
  (let ((res (exec-query con "select (cast('2005-6-7 13:04:45' as timestamp)) as a")))
    (assert (= (encode-universal-time 45 4 13 7 6 2005) (caar res)))))


(defun pgsql-test20 (con)
  (ignore-errors (exec-command con  "drop table type_test"))
  (exec-command con *pgsql-type_test-ddl*)
  (dotimes (i 100)
    (let* ((str (make-string 100 :initial-element #\p))
           (binary (make-array 1000 :initial-element (random 256)))
           (id (random 1000)))
      (exec-update con "delete from type_test where id =?" id)
      (exec-update con "insert into type_test (t_blob,id,t_text) values(?,?,?)" 
                   (list binary :blob) id (list str :clob))
      (multiple-value-bind (r1 m1)
          (exec-query con 
                      "select id as aaa,t_blob as bbb,t_text as ccc from type_test where id=?"
                      id)
        (assert (equalp r1 (list (list id (coerce binary '(vector (unsigned-byte 8))) str))))
        (assert (equal m1 '("aaa" "bbb" "ccc")))
        (commit con)))))


(defun pgsql-test22 (con)
  (let ((res (first 
              (exec-query con "
          select 
           ? as t_double,
           ? as t_integer,
           ? as t_varchar,
           cast(? as bytea) as t_varbinary"
                          1223455.334 12345 "blablablub" #(1 2 3 4)))))
    (assert (equal 
             '(1223455 12345 "blablablub" (1 2 3 4))
             (list (truncate (first res))
                   (second res)
                   (third res)
                   (coerce (fourth res) 'list))))))


;; be carefull with double and float in lisp to!
(defun pgsql-test23 (con)
  (pgsql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (a double precision)")
  (pgsql-drop-test-proc con "test99" "(double precision)")
  (exec-command con "create function test99 ( aa in double precision ) returns void as $$
    begin 
    insert into testtab99 (a) values(aa+1);
    end; 
    $$ LANGUAGE plpgsql; 
    ")
  (with-prepared-statement 
   (stm con "{call test99(?)}" 
        '(:double :in))
    (let ((nix (exec-prepared-command stm 1.8d0))
          (nix2  (commit con))
          (res (exec-query con "select * from testtab99")))
      (assert (= (caar res) 2.8d0)))))

(defun pgsql-test23b (con)
  (pgsql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (a int)")
  (pgsql-drop-test-proc con "test99" "(int)")
  (exec-command con "
   create function test99 ( aa in integer ) returns void as $$
    begin 
    insert into testtab99 (a) values(aa+1);
    end; 
   $$ language plpgsql;")
  (with-prepared-statement 
   (stm con "{call test99(?)}" 
        '(:integer :in))
    (let ((nix (exec-prepared-command stm 7))
          (res (exec-query con "select * from testtab99")))
      (assert (= (caar res) 8)))))


(defun pgsql-test24 (con)
  (pgsql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (a decimal)")
  (pgsql-drop-test-proc con "test99" "(decimal)")
  (exec-command con "
   create function test99 (aa decimal) returns void as $$
    begin 
    insert into testtab99 (a) values(aa+1);
    end; $$ language plpgsql")
  (with-prepared-statement 
   (stm con "{call test99(?)}" 
        '(:integer :in))
    (let ((nix (exec-prepared-command stm 7))
          (res (exec-query con "select * from testtab99")))
      (assert (= (caar res) 8)))))


(defun pgsql-test25 (con)
  (pgsql-drop-test-proc con "test99" "(decimal,varchar(200),out refcursor,out refcursor)")
  (exec-command con "
   create function test99 (pa decimal,pb varchar(200),pc1 out refcursor,pc2 out refcursor) returns record as $$
    begin 
      open pc1 for select 3*pa as a, 'x' || pb ||'y' as b, cast('2003-4-1' as timestamp) as c;
      open pc2 for select cast('631e195c917a11dda77f6fc5602f0ac0' as bytea) as x,cast(123456789012345678 as bigint) as y,cast('abcdefghijklmnop' as bytea) as z;
    end; $$ language plpgsql")
  (multiple-value-bind (r0 c0) (exec-query con "{call test99(?,?)" 123456789 "abcdefghijklmn")
    (assert (equal c0 '("pc1" "pc2")))
    (multiple-value-bind (r1 c1) (exec-query con (format nil "fetch all in \"~A\"" (caar r0)))
      (multiple-value-bind (r2 c2) (exec-query con (format nil "fetch all in \"~A\"" (cadar r0)))
        (assert (equal c1 '("a" "b" "c")))
        (assert (equal c2 '("x" "y" "z")))
        (assert (equal (first r1) (list (* 3.0d0   123456789) "xabcdefghijklmny" 
                                        (encode-universal-time 0 0 0 1 4 2003))))
        (assert (equal (second (first r2 )) 123456789012345678 ))
        (assert (equal (coerce (third (first r2)) 'list) (coerce (map 'vector 'char-code "abcdefghijklmnop") 'list)))))))

(defun pgsql-test26 (con)
  (pgsql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (id int,bla varchar(200))")
  (exec-update con "insert into testtab99 (id,bla) values(1,'bla1')")
  (exec-update con "insert into testtab99 (id,bla) values(2,'bla2')")
  (assert (equal (exec-update con "update testtab99 set bla=? where id=?" "bla100" 1) 1))
  (assert (equal (exec-update con "update testtab99 set bla=? where id=?" "bla100" 99) 0))
  (with-prepared-statement 
   (stm con "update testtab99 set bla=? where id=?" :string :integer)
   (assert (equal (exec-prepared-update stm  "bla100" 1) 1))
   (assert (equal (exec-prepared-update stm  "bla100" 99) 0))
  ))


(defun pgsql-test28 (con)
  (pgsql-drop-test-proc con "test99" "(a1 varchar(200),b1 int,c1 timestamp,a2 out varchar(200), b2 out int,c2 out timestamp)")
   (exec-command con "
    create function test99(a1 varchar(200),b1 int,c1 timestamp,a2 out varchar(200), b2 out int,c2 out timestamp) returns record as $$
    begin
     a2:= a1||'x';
     b2:=b1+3;
     c2:=c1+ interval '3 day';
    end; $$ language plpgsql")
   (let* ((the-date (encode-universal-time 12 3 5 12 11 2007)))
     (multiple-value-bind (co resultsets params)
         (exec-sql con "{call test99(?,?,?,?,?,?)}" "abc" 
                   (list 12 :integer ) (list the-date :date)
                   '(nil :string :out) '(nil :integer :out) '(nil :date :out))

         (assert (equal (first params) "abcx"))
         (assert (equal (second params) 15))
         (assert (equal (third params) (+ the-date (* 3 86400))))
         )
       ))



  

  
