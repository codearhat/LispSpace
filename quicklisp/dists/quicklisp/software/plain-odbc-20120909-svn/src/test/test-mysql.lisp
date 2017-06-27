;;; -*- Mode: lisp -*-


; to run these tests you need to use a schema/database with "use schema".



(in-package :test-plain-odbc)

(export '(run-mysql-tests))

(defun run-mysql-tests (con)
  (dolist (sym '(
                 mysql-type-test
                 mysql-test1 
                 mysql-test2 
                 mysql-test3 
                 mysql-test4 
                 mysql-test5 
                 mysql-test6a 
                 mysql-test6b 
                 mysql-test7 
                 mysql-test9 
                 mysql-test10 
                 mysql-test15 
                 mysql-test20 
                 mysql-test22 
                 mysql-test23 
                 mysql-test23b 
                 mysql-test24
                 mysql-test25
                 mysql-test26
                 mysql-test27
                 mysql-test28
                 ))
    (pprint sym)
    (funcall sym con)))

(defparameter *mysql-type_test-ddl* "  

create table type_test
 (id int,
t_TINYINT TINYINT,
t_SMALLINT  SMALLINT,
t_MEDIUMINT mediumint,
t_INT int,
t_BIGINT bigint,
t_float float,
t_DOUBLE double,
t_decimal40_20 DECIMAL(40,20) ,

t_dATE date,

t_DATETIME datetime,
t_TIMESTAMP timestamp,
t_TIME 	time,
t_YEAR 	year,
T_char char(255),
t_varchar varchar(255),

t_TINYBLOB tinyblob, 
t_TINYTEXT tinytext,
t_BLOB blob,
t_TEXT text,
t_MEDIUMBLOB mediumblob, 
t_MEDIUMTEXT mediumtext,
t_LONGBLOB longblob, 
t_LONGTEXT longtext

)
")
(defun mysql-type-test (con)
  (ignore-errors (exec-command con "drop table type_test"))
  (exec-command con *mysql-type_test-ddl*)
  (exec-update con "insert into type_test (id) values(1)")
  (exec-update con "
  update type_test set
    t_tinyint =1,
    t_smallint =255,
   t_MEDIUMINT =256*256*127,
  t_INT =987,
  t_BIGINT =256*256*256*256 *256*256*256*127,
  t_float =1.0e0/7.0e0,
  t_DOUBLE = 1.0e0/7.0e0,
  t_decimal40_20 = '12345678901234567890.1234567890123456789',
  t_dATE ='2004-6-25',
  t_DATETIME = '2004-5-13 13:56:34',
  /* t_TIMESTAMP timestamp,  */
  t_TIME 	= '12:56',
  t_YEAR 	=1967,
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
              con "update type_test set t_longblob =?, t_longtext=? where id =1" 
              '(:blob :in) '(:clob :in))))
     (exec-prepared-update stm 
                           (make-array 10000 :element-type '(unsigned-byte 8) 
                                       :initial-element 33)
                           (make-string 100001 :initial-element #\o)))
   (commit con)
   (let ((res (exec-query con "
    select t_tinyint,
           t_smallint,
           t_mediumint,
           t_int,
           t_bigint,
           t_float,
           t_double,
           t_decimal40_20,
           t_date,
           t_datetime,
           t_time,
           t_year,
           t_char,
           t_varchar,
           t_longblob,
           t_longtext
        from type_test where id=1")))
     (assert (= (length res) 1))
     (setf res (first res))
     (assert (equal (nth 0 res) 1))
     (assert (equal (nth 1 res) 255))
     (assert (equal (nth 2 res) (* 256 256 127)))
     (assert (equal (nth 3 res) 987))
     (assert (equal (nth 4 res) (* 256 256 256 256  256 256 256 127)))
     ;; these are skipped since support for double in myslq is bad 
     ;;(assert (equal (nth 5 res) (/  1 7.0)))
     ;;(assert (equal (nth 6 res) (/  1 7d0)))
     ;; skipped because we do not have decimal as a datatype
     (assert (equal (nth 7 res) 12345678901234567890.1234567890123456789d0))
     (assert (equal (nth 8 res) (encode-universal-time  0 0 0 25 6 2004)))
     (assert (equal (nth 9 res) (encode-universal-time  34 56 13 13 5 2004)))
     (assert (equal (nth 10 res)  "12:56:00"))
     ;; year is returend as  SQL_SMALLINT
     (assert (equal (nth 11 res)  1967))
     (assert (equal (nth 12 res) "abcdefghijkmlnop"))
     (assert (equal (nth 13 res) "abcdefghijklmnopqrstuvw"))
     (assert (equal (coerce (nth 14 res) 'list) 
                    (coerce (make-array 10000 :element-type '(unsigned-byte 8) 
                                        :initial-element 33 :adjustable t) 'list)))
     (assert (equal (nth 15 res) (make-string 100001 :initial-element #\o)))
     ))



(defun mysql-test1 (con)
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


(defun mysql-test2 (con)
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


(defun mysql-test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (exec-update con "delete from type_test where id=99")
    (with-prepared-statement (stm con 
                                  "insert into type_test (id,t_datetime) values(99,?)"
                                  '(:date :in))
      (exec-prepared-update stm "3323283742"))
    (let ((res (exec-query con "select  DATE_ADD(t_datetime, INTERVAL 1 DAY) 
                                from type_test where id =99")))
      (assert (equal (parse-integer (caar res)) (+ 86400 3323283742))))))

(defun mysql-test4 (con)
  (exec-update con "delete from type_test where id=99")
  (with-prepared-statement (stm con "insert into type_test (id,t_double) values(99,?)" 
                                '(:double :in))
    (exec-prepared-update stm 1.8d0))
  (let ((res (exec-query con "select t_double+1 from type_test where id=99")))
    (assert (<= (abs (- (caar res) 2.8d0)) 1d-7))))

                                         

(defun mysql-test5 (con)
  (exec-update con "delete from type_test")
  (commit con)
  (with-prepared-statement (stm con "insert into type_test (id,t_LONGTEXT) values(?,?)" 
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
          (let ((res (exec-query con (format nil "select t_longtext from type_test where id=~A" len))))
          (assert (equal res
                         (list (list string)))))))))
    (commit con)
    )


(defun mysql-drop-test-proc (con proc)
  (unless (zerop (caar (exec-query con (format nil 
                                               "
    select count(*) 
    from information_schema.routines 
    where routine_name ='~A'
    and routine_schema='test'" proc))))
    (exec-command con (format nil "drop procedure ~A" proc))))

(defun mysql-drop-test-table (con proc)
  (unless (zerop (caar (exec-query con (format nil 
                                               "
    select count(*) 
    from information_schema.tables 
    where table_name ='~A'
    and table_schema='test'" proc))))
    (exec-command con (format nil "drop table ~A" proc))))




;; this fails since myodbc can not deal with out parameters
(defun mysql-test6 (con)
  (mysql-drop-test-proc con "test99")
  (exec-command con "
CREATE PROCEDURE test99 (in p1 int, out p2 INT)
     BEGIN
       set p2=p1+5;
     END;")
  (commit con)
  (let ((stm (prepare-statement con "{call test.test99(?,?)}" 
                                '(:integer :in) 
                                '(:integer :out))))
    (assert (= 6 (first (exec-prepared-command stm 1))))
    (free-statement stm)))


(defun mysql-test6a (con)
  (mysql-drop-test-proc con "test99")
  (exec-command con "
CREATE PROCEDURE test99 (in p1 int, out p2 INT)
     BEGIN
       set p2=p1+5;
     END;")
  (commit con)
  (plain-odbc:exec-command con "set @p2=1000")
  (let ((stm (prepare-statement con "call test.test99(?,@p2)" 
                                '(:integer :in))))
    (exec-prepared-command stm 1) 
    (assert (= 6 (first (first     (exec-query con "select cast(@p2 as signed)"))))) 
    (free-statement stm)))

(defun mysql-test6b (con)
  (mysql-drop-test-proc con "test99")
  (exec-command con "
CREATE PROCEDURE test99 (in p1 int, out p2 INT)
     BEGIN
       set p2=p1+5;
     END;")
  (commit con)
  (plain-odbc:exec-command con "set @p2=1000")
  (plain-odbc:exec-command con "set @p1=1")
  (let ((stm (prepare-statement con "call test.test99(@p1,@p2)" )))
    (exec-prepared-command stm) 
    (assert (= 6 (first (first  (exec-query con "select cast(@p2 as signed)"))))) 
    (free-statement stm)))

(defun mysql-test7 (con)
   (let ((*universal-time-to-date-dataype* 'universal-time-list)
         (*date-datatype-to-universal-time* 'list-universal-time)
         (*date-type-predicate* 'date-lisp-p))
     (let ((res (exec-query con "select date_add(cast(? as date),interval 1 day)" 
                            '((:date 2005 4 5) :date))))
           (assert (equal res '(((:date 2005 4 6 0 0 0))))))))



;;; 
(defun mysql-test9 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b LONGTEXT)")
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


(defun mysql-test10 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b longblob)")
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


(defun mysql-test15(con)
  (let ((res (exec-query con "select (cast('2005-6-7 13:04:45' as datetime)) as a")))
    (assert (= (encode-universal-time 45 4 13 7 6 2005) (caar res)))))


(defun mysql-test20 (con)
  (ignore-errors (exec-command con  "drop table type_test"))
  (exec-command con *mysql-type_test-ddl*)
  (dotimes (i 100)
    (let* ((str (make-string 100 :initial-element #\p))
           (binary (make-array 1000 :initial-element (random 256)))
           (id (random 1000)))
      (exec-update con "delete from type_test where id =?" id)
      (exec-update con "insert into type_test (t_longblob,id,t_longtext) values(?,?,?)" 
                   (list binary :blob) id (list str :clob))
      (multiple-value-bind (r1 m1)
          (exec-query con 
                      "select id aaa,t_longblob bbb,t_longtext ccc from type_test where id=?"
                      id)
        (assert (equalp r1 (list (list id (coerce binary '(vector (unsigned-byte 8))) str))))
        (assert (equal m1 '("aaa" "bbb" "ccc")))
        (commit con)))))


(defun mysql-test22 (con)
  (let ((res (first 
              (exec-query con "
          select 
           ? as t_double,
           ? as t_integer,
           ? as t_varchar,
           cast(? as binary) as t_varbinary"
                          1223455.334 12345 "blablablub" #(1 2 3 4)))))
    (assert (equal 
             '(1223455 12345 "blablablub" (1 2 3 4))
             (list (truncate (first res))
                   (second res)
                   (third res)
                   (coerce (fourth res) 'list))))))


;; be carefull with double and float in lisp to!
(defun mysql-test23 (con)
  (mysql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (a double)")
  (mysql-drop-test-proc con "test99")
  (exec-command con "
   create procedure test99 (a double)
    begin 
    insert into testtab99 (a) values(a+1);
    end;")
  (with-prepared-statement 
   (stm con "{call test99(?)}" 
        '(:double :in))
    (let ((nix (exec-prepared-command stm 1.8d0))
          (nix2  (commit con))
          (res (exec-query con "select * from testtab99")))
      (assert (= (caar res) 2.8d0)))))

(defun mysql-test23b (con)
  (mysql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (a int)")
  (mysql-drop-test-proc con "test99")
  (exec-command con "
   create procedure test99 (a int)
    begin 
    insert into testtab99 (a) values(a+1);
    end;")
  (with-prepared-statement 
   (stm con "{call test99(?)}" 
        '(:integer :in))
    (let ((nix (exec-prepared-command stm 7))
          (res (exec-query con "select * from testtab99")))
      (assert (= (caar res) 8)))))


(defun mysql-test24 (con)
  (mysql-drop-test-table con "testtab99")
  (exec-command con "create table testtab99 (a decimal)")
  (mysql-drop-test-proc con "test99")
  (exec-command con "
   create procedure test99 (a decimal)
    begin 
    insert into testtab99 (a) values(a+1);
    end;")
  (with-prepared-statement 
   (stm con "{call test99(?)}" 
        '(:integer :in))
    (let ((nix (exec-prepared-command stm 7))
          (res (exec-query con "select * from testtab99")))
      (assert (= (caar res) 8)))))


(defun mysql-test25 (con)
  (mysql-drop-test-proc con "test99")
  (exec-command con "
   create procedure test99 (a decimal,b varchar(200))
    begin 
      select 3*a as a, concat('x' , b ,'y') as b, cast('2003-4-1' as datetime) as c;
      select uuid() as x,cast(123456789012345678 as signed) as y,cast('abcdefghijklmnop' as binary) as z;
    end;")
  (multiple-value-bind (r1 c1 r2 c2) (exec-query con "{call test99(?,?)" 123456789 "abcdefghijklmn")
     
     (assert (equal c1 '("a" "b" "c")))
     (assert (equal c2 '("x" "y" "z")))
     (assert (equal (first r1) (list (* 3.0d0   123456789) "xabcdefghijklmny" 
                             (encode-universal-time 0 0 0 1 4 2003))))
     (assert (equal (second (first r2 )) 123456789012345678 ))
     (assert (equal (coerce (third (first r2)) 'list) (coerce (map 'vector 'char-code "abcdefghijklmnop") 'list)))))

(defun mysql-test26 (con)
  (mysql-drop-test-table con "testtab99")
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

(defun mysql-test27 (con)
  (mysql-drop-test-proc con "test99")
  (exec-command con "
    create procedure test99(a1 varchar(200),b1 int,c1 datetime) 
    begin
    declare a2 varchar(200);
    declare b2 int;
    declare c2 datetime;
     set a2=concat(a1,'x');
      set b2=b1+3;
      set c2=c1+ interval 3 day;
      select a2 as a2,b2 as b2, c2 as c2;
    end;")
  (let* ((the-date (encode-universal-time 12 3 5 12 11 2007))
         (res (exec-query con "call test99(?,?,?)" "abc" 
                          (list 12 :integer ) (list the-date :date)))
         (row (first res)))
    (assert (equal (first row) "abcx"))
    (assert (equal (second row) 15))
    (assert (equal (third row) (+ the-date (* 3 86400))))))

(defun mysql-test28 (con)
  (mysql-drop-test-proc con "test99")
   (exec-command con "
    create procedure test99(a1 varchar(200),b1 int,c1 datetime) 
    begin
    declare a2 varchar(200);
    declare b2 int;
    declare c2 datetime;
     set a2=concat(a1,'x');
      set b2=b1+3;
      set c2=c1+ interval 3 day;
      select a2 as \"a2\",b2 as \"b2\", c2 as \"c2\";
      select c2 as \"c2\", b2 as \"b2\",a2 as \"a2\";

    end;")
   (let* ((the-date (encode-universal-time 12 3 5 12 11 2007)))
     (multiple-value-bind (co resultsets params)
         (exec-sql con "call test99(?,?,?)" "abc" 
                   (list 12 :integer ) (list the-date :date))
       (let* ((rs1 (first resultsets))
              (rs2 (second resultsets))
              (rows1 (first rs1))
              (rows2 (first rs2))
              (cols1 (second rs1))
              (cols2 (second rs2))
              (row1 (first rows1))
              (row2 (first rows2)))
         (assert (equal (first row1) "abcx"))
         (assert (equal (second row1) 15))
         (assert (equal (third row1) (+ the-date (* 3 86400))))

         (assert (equal (third row2) "abcx"))
         (assert (equal (second row2) 15))
         (assert (equal (first row2) (+ the-date (* 3 86400))))
         
         (assert (equal cols1 (list "a2" "b2" "c2")))
         (assert (equal cols2 (list "c2" "b2" "a2")))
         )
       )))



  

  
