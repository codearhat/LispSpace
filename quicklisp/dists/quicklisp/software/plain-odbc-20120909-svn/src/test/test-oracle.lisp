;;; -*- Mode: lisp  -*-


(in-package :test-plain-odbc)

(export '(run-oracle-tests))

(defun run-oracle-tests (con)
  (flet ((doit ()
           (dolist (sym '(oracle-type-test
                          ora-test1 
                          ora-test2 
                          ora-test3 
                          ora-test4 
                          ora-test5 
                          ora-test6 
                          ora-test7 
                          ora-test8 
                          ora-test9 
                          ora-test10 
                          ora-test11 
                          ora-test12
                                        ; metadata
                          ora-test13
                          ora-test14
                          ora-test15
                          ora-test16
                          ora-test17
                          ora-test18
                          ora-test19
                          ))
             (pprint sym)
             (funcall sym con))))
    (format t "with use-bind~%")
    (setf (use-bind-column con) t)
    (doit)
    (format t "~%~%no use-bind~%")
    (setf (use-bind-column con) nil)
    (doit)
    ))
  
 
; this function replaces in a string every (code-char 13)  
; (code-char 10)by #\space
; this is needed for oracle PL/SQL statements
(defun fix13 (str)
  (substitute #\space (code-char 10) (substitute #\space (code-char 13) str)))

       

(defparameter *oracle-type_test-ddl* "
create table type_test (
  id integer,
  t_integer integer,
  t_number number,
  t_char char(2000) ,
  t_varchar varchar2(4000),
  t_nvarchar NVARCHAR2(200),
  t_date date,
  t_raw raw(2000),
  t_blob blob,
  t_clob clob,
  t_nclob nclob
  )
")



(defun ora-drop-test-proc (con proc)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from user_objects where object_name='~A'" proc))))
    (exec-command con (format nil "drop procedure ~A" proc))))

(defun ora-drop-test-table (con proc)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from user_objects where object_name='~A'" proc))))
    (exec-command con (format nil "drop table ~A" proc))))


(defun oracle-type-test (con)
  (ora-drop-test-table con "TYPE_TEST")
  (exec-command con *oracle-type_test-ddl*)
  (exec-update con "insert into type_test (id) values(1)")
  (exec-update con "
   update type_test set
      t_integer= 12345678901234,
      t_number = 1.0/3.0,
      t_char=rpad('1',1999),
      t_varchar =lpad('1',39),
      t_nvarchar = lpad(nchr(1234),200,nchr(1234)),
      t_date = to_date('1.3.2005 12:23:14','dd.mm.yyyy hh24:mi:ss'),
      t_raw =hextoraw('11223344556677889900')
    where id =1")
  (let ((stm (prepare-statement 
              con "update type_test set t_blob=?,t_clob=?,t_nclob=? where id =1" 
              '(:blob :in) '(:clob :in) '(:UNICODE-CLOB :in))))
  (exec-prepared-update 
   stm ;; sizes were 100000 and 100001
   (make-array 100000 :element-type '(unsigned-byte 8) :initial-element 33)
   (make-string 1000001 :initial-element #\o)
   (make-string 1234567 :initial-element (code-char 3217))))
  (let ((res (exec-query con "
      select id,t_integer,t_number,t_char,t_varchar,t_date,t_raw, t_nvarchar from type_test where id=1")))
    (assert (= (length res) 1))
    (setf res (first res))
    (assert (equal (nth 0 res) 1d0))
    (assert (equal (nth 1 res) 12345678901234d0))
    (assert (> 1d-15 (abs (- (nth 2 res) (/ 1d0 3)))))
    ;;  t_char is of type char and therefor has always length 2000, 
    ;; although we inserted a string of length 1999
    (assert (equal (nth 3 res) 
                   (let ((a (make-string 2000 :initial-element #\space)))
                     (setf (schar a 0) #\1)
                     a)))
    (assert (equal (nth 4 res) 
                   (let ((a (make-string 39 :initial-element #\space)))
                     (setf (schar a 38) #\1)
                     a)))
    (assert (equal (nth 5 res)
                   (encode-universal-time 14 23 12 1 3 2005)))
    (assert (equal (coerce (nth 6 res) 'list)
                   '(#x11 #x22 #x33 #x44 #x55 #x66 #x77 #x88 #x99 #x00)))
    (assert (equal (nth 7 res) (make-string 200 :initial-element (code-char 1234))))
    (let ((a (caar (exec-query con "select t_blob from type_test where id =1"))))
      (assert (equalp a (make-array 100000 :element-type '(unsigned-byte 8) :initial-element 33))))
    (let ((b (caar (exec-query con "select t_clob from type_test where id=1"))))
      (assert (equal b (make-string 1000001 :initial-element #\o))))

    (let ((c (caar (exec-query con "select t_nclob from type_test where id=1"))))
      (assert (equal c (make-string 1234567 :initial-element (code-char 3217))))))
    
  (commit con))

(defun ora-test1 (con)
  (ora-drop-test-proc con "TEST99")
  (exec-command con (fix13 "
  create procedure TEST99 (a integer,b out integer) as 
  begin 
    b:=a+1; 
  end;
 "))
  (commit con)
  (let ((stm (prepare-statement con "{call TEST99(?,?)}" 
                                '(:integer :in) 
                                '(:integer :out))))
    (assert (= 2 (first (exec-prepared-command stm 1))))
    (free-statement stm)))


(defun ora-test2 (con)
  (ora-drop-test-proc con "TEST99")
  (exec-command con (fix13 "
  create procedure TEST99 (a varchar2,b out varchar2) as
  begin
    b:=a;
  end;
 "))
  (commit con)
  (let ((stm (prepare-statement con "{call TEST99(?,?)}" 
                                '(:string :in) 
                                '(:string :out))))
    (let ((str "lolkalkxylkzlhjajhgfsjgakjhgfjfjhgffdtrtreztr"))
      (assert (equal str (first (exec-prepared-command stm str))))
      (free-statement stm)))
  (commit con))



(defun ora-test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (ora-drop-test-proc con "TEST99")
    (let ((a (caar (exec-query con "select sysdate from dual"))))
      (exec-command con (fix13 "
  create procedure TEST99 (a date,b out date ) as 
  begin
    b:=a+1; 
  end;
  "))  
      (with-prepared-statement 
          (stm con "{call TEST99(?,?)}" 
               '(:date :in) 
               '(:date :out))
        (let ((res (exec-prepared-command stm "3323283742")))
          (assert (equal res (list (write-to-string (+ 3323283742 86400))))))))
    (commit con)))


(defun ora-test4 (con)
  (ora-drop-test-proc con "TEST99")
  (exec-command con (fix13 "
   create procedure TEST99 (a in out varchar2, b in out varchar2) as
    x varchar2(1000); begin x:=a;a:=b;b:=x; end;"))
  (with-prepared-statement (stm con "{call TEST99(?,?)}" 
                                '(:string :inout) 
                                '(:string :inout))
    (let ((res (exec-prepared-command stm "abc" "xyz")))
      (assert (equal res (list "xyz" "abc"))))))
 

(defun ora-test5 (con)
  (ora-drop-test-proc con "TEST99")
  (exec-command con (fix13 "
   create procedure TEST99 (a raw,b out raw) as 
   begin 
     b:=a;
   end;
   "))
  (with-prepared-statement (stm con "{call TEST99(?,?)}" 
                                '(:binary :in) 
                                '(:binary :out))
    (let* ((guid (caar (exec-query con "select sys_guid() from dual")))
           (res (exec-prepared-command stm guid)))
      (assert (equalp guid (first res))))
    (commit con)))



(defun ora-test6 (con)
  (let ((*universal-time-to-date-dataype* 'universal-time-list)
        (*date-datatype-to-universal-time* 'list-universal-time))

    (ora-drop-test-proc con "TEST99")
    (exec-command con (fix13 "
     create procedure TEST99 (a date, b out date) as 
    begin
     b:=a+2;
    end;
    "))
    (with-prepared-statement (stm con "{call TEST99(?,?)}" ':date '(:date :out))
      (let ((res (exec-prepared-command stm '(:date 2003 3 4))))
        (assert (equal res '((:date 2003 3 6 0 0 0))))))
    (let ((res (exec-query con "
      select to_date('8.6.2005','dd.mm.yyyy') -1.0 / (86400-1) from dual")))
      (assert (equal res '(((:date 2005 6 7 23 59 59))))))))


(defun ora-test7 (con)
  (let ((filename (namestring (merge-pathnames "odb-trace-test.log" *test-temp-dir*))))
    (when (probe-file filename)
      (DELETE-FILE filename))
    (assert (not (probe-file filename)))
    (trace-connection con filename)
    (dotimes (i 5) (exec-query con "select * from dual"))
    (with-open-file (f filename :direction :input)
      (assert (> (file-length f) 500)))
    (untrace-connection con)
    ;(break)
    (DELETE-FILE filename)
    (exec-query con "select * from dual")
    (assert (not (probe-file filename)))
    ))

;; works only with oracle 9 ?
;; this does not work. mybe with the oracle odbc driver?
;; it works with oracle 10gr2
(defun ora-test8 (con)
  (ignore-errors (exec-command con "drop table testtab99"))
  (exec-command con "create table testtab99 (id integer, txt nvarchar2(2000))")
  (let ((str (coerce (list #\a #\j (code-char 1000) (code-char 2000) #\o) 'string)))
  (with-prepared-statement (stm con "insert into testtab99 (id,txt) values(?,?)"
                                '(:integer :in) '(:unicode-string :in))
    (exec-prepared-update stm 1 str))
  (let ((res (exec-query con "select txt from testtab99 where id =1")))
    (assert (equal (list str) (first res))))))


(defun ora-test9(con)
  (let ((res (exec-query con "select to_date('2005-6-7 13:04:45','yyyy-mm-dd hh24:mi:ss' ) as a from dual")))
    (assert (= (encode-universal-time 45 4 13 7 6 2005) (caar res)))))

(defun ora-test10(con)
  (with-prepared-statement (stm con "
          select to_char(?,'yyyy-mm-dd hh24:mi:ss')
          from dual" 
                                '(:date :in))
    (let ((res (exec-prepared-query stm (encode-universal-time 1 2 3 13 10 2005))))
      (assert (equalp "2005-10-13 03:02:01" (caar res))))))

(defun ora-test11(con)
  (exec-command con (fix13 "
     create or replace package test99_pkg as
       type refcursor is ref cursor;
       procedure test_cursor(v varchar2,c in out refcursor);
     end;"))
  (exec-command con (fix13 "
     create or replace package body test99_pkg as
      procedure test_cursor(v varchar2,c in out refcursor) is
      begin
        open c for select v as a,'1234567890' as b from dual;
      end;
     end;"))
  (with-prepared-statement (stm con 
                                "{call test99_pkg.test_cursor(?,?)}" 
                                '(:string :in ))
    (let ((str "just a string"))
      (let ((res (exec-prepared-query stm str)))
        (assert (equal res (list (list str "1234567890"))))))))

       
(defun ora-test12 (con)
  (ora-drop-test-proc con "TEST99")
  (exec-command con  (fix13 "create procedure test99 (a in out nvarchar2 ,b in out nvarchar2) as
        x  nvarchar2(1000);
        begin x:=a; a:=b; b:=x; end;"))
  (with-prepared-statement (stm con "{call test99(?,?)}" 
                                '(:unicode-string :inout) 
                                '(:unicode-string :inout))
    (let ((str1 (make-funny-string 700  (coerce (list (code-char 2341) (code-char 2347) #\a) 'vector )  ))
          (str2 (make-funny-string 900   (coerce (list (code-char 2341) (code-char 2347) #\a) 'vector ))))
      
      (let ((res (exec-prepared-command stm str1 str2)))
        (assert (equal res (list str2 str1)))))))
 
(defun ora-test13 (con)
  (ora-drop-test-table con "TESTTAB99")
  (exec-command con "create table testtab99 (x integer,y varchar2(200))")
  (multiple-value-bind (rc res params)
      ;; if columns x and y are set by triggers they can also be retrieved by this method
      ;; fixme /bug : it is not possible to retrieve the varchar value
      ;;> Error: [Oracle][ODBC][Ora]ORA-01461: Ein LONG-Wert kann nur zur Einfügung in eine LONG-Spalte gebunden werden
      ;;>        , error code 1461, State: S1000.
      (exec-sql con "insert into testtab99 (x,y) values(?+12,?||'a') returning x into ?" 13 "a" '(nil :integer :out))
    (assert (= rc 1))
    (assert (equal res nil))
    (assert (equal (list 25) params))))


;;; metdaten tests

(defun ora-mk-metadatatest (con)
  (ora-drop-test-table con "METADATATEST")
  (exec-command con "
    CREATE TABLE metadatatest(
	x int NOT NULL,
        y varchar2(10),
        z date,
       CONSTRAINT metadatatest_pk PRIMARY KEY (x,y)) "))

(defun ora-test14 (con)
  (ora-mk-metadatatest con)
  (let ((user (first (first (exec-query con "select user from dual")))))
     (multiple-value-bind
         (res cols) 
         (get-primary-keys con nil user "METADATATEST")
       (assert (= 2 (length res)))
       (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "COLUMN_NAME" "KEY_SEQ" "PK_NAME"))))))

(defun ora-test15 (con) 
  (ora-mk-metadatatest con)
  (let ((user (first (first (exec-query con "select user from dual")))))
  (multiple-value-bind 
      (res cols)
      (get-columns con nil user "METADATATEST" nil)
    (assert (= 3 (length res)))
    (assert (equal (subseq cols 0 18) 
                   '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "COLUMN_NAME" "DATA_TYPE" "TYPE_NAME" 
                     "COLUMN_SIZE" "BUFFER_LENGTH" "DECIMAL_DIGITS" "NUM_PREC_RADIX" "NULLABLE" 
                     "REMARKS" "COLUMN_DEF" "SQL_DATA_TYPE" "SQL_DATETIME_SUB" "CHAR_OCTET_LENGTH" 
                     "ORDINAL_POSITION" "IS_NULLABLE"))))))

(defun ora-test16 (con)
  (ora-mk-metadatatest con)
  (let ((user (first (first (exec-query con "select user from dual")))))
    (multiple-value-bind 
        (res cols)
        (get-tables con nil user "METADATATEST" nil)
      (assert (= 1 (length res)))
      (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "TABLE_TYPE" "REMARKS"))))))

(defun ora-test17 (con)
  (ora-mk-metadatatest con)
  (ora-drop-test-table con "METADATATEST2")
  (exec-command con "
    CREATE TABLE metadatatest2(
	a int NOT NULL,
        b varchar2(10),
        c date,
       CONSTRAINT metadatatest2_pk PRIMARY KEY (a,b)) ")
  (exec-command con "alter table metadatatest add constraint metadatatest_fk1 foreign key (x,y) references metadatatest2(a,b)")
  (let ((user (first (first (exec-query con "select user from dual")))))
    (multiple-value-bind 
        (res1 cols1)
        (get-foreign-keys con nil user "METADATATEST2"
                          nil nil nil)
      (assert (= 2 (length res1)))
      (assert (equal cols1  '("PKTABLE_CAT" "PKTABLE_SCHEM" "PKTABLE_NAME" "PKCOLUMN_NAME" "FKTABLE_CAT" "FKTABLE_SCHEM" "FKTABLE_NAME" "FKCOLUMN_NAME" "KEY_SEQ"
                              "UPDATE_RULE" "DELETE_RULE" "FK_NAME" "PK_NAME"))) ; "DEFERRABILITY" is missing
      (multiple-value-bind 
          (res2 cols2)
          (get-foreign-keys con nil nil nil
                            nil nil "METADATATEST")
        (assert (= 2 (length res2)))))))



(defun ora-test18 (con)
  (ora-mk-metadatatest con)
  (let ((user (first (first (exec-query con "select user from dual")))))
    (multiple-value-bind 
        (res cols)
        (get-tables con nil user "METADATATEST" "TABLE")
      (assert (= 1 (length res)))
      (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "TABLE_TYPE" "REMARKS"))))))

(defun ora-test19 (con)
  (ignore-errors (exec-command con "drop view metadatatest_vw"))
  (exec-command con "create view metadatatest_vw as select 1 as a from dual")
  (let ((user (first (first (exec-query con "select user from dual")))))
    (dolist (type '("VIEW" nil))
       (multiple-value-bind 
           (res cols)
           (get-tables con nil user "METADATATEST_VW" type)
         (assert (= 1 (length res)))
         (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "TABLE_TYPE" "REMARKS")))))))

