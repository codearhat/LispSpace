;;; -*- Mode:lisp -*-


(in-package :test-plain-odbc)

(export 'run-sql-server-tests)


(defun run-sql-server-tests (con)
  (flet ((doit () 
           (dolist (sym '(ss-type-test 
                          ss-test1 
                          ss-test2 
                          ss-test3 
                          ss-test4 
                          ss-test5 
                          ss-test6   
                          ss-test7 
                          ss-test8 
                          ss-test8a 
                          ss-test9 
                          ss-test10 
                          ss-test11 
                          ss-test12 
                          ss-test13 
                          ss-test14 
                          ss-test15 
                          ss-test16 
                          ss-test17 
                          ss-test18 
                          ss-test19 
                          ss-test20 
                          ss-test21 
                          ss-test22
                          ss-test23
                          ss-test24
                          ss-test25
                          ss-test26
                          ss-test27
                          ss-test28
                          ss-test29
                          ss-test30
                          ss-test31
                          ss-test32
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
    

(defparameter *sql-server-type_test-ddl* "
CREATE TABLE [type_test] (
	[id] [int] NOT NULL ,
	[t_bigint] [bigint] NULL ,
	[t_binary] [binary] (50) NULL ,
	[t_bit] [bit] NULL ,
	[t_char] [char] (10) NULL ,
	[t_datetime] [datetime] NULL ,
	[t_decimal] [decimal](38, 9) NULL ,
	[t_float] [float] NULL ,
	[t_image] [image] NULL ,
	[t_money] [money] NULL ,
	[t_nchar] [nchar] (10)  NULL ,
	[t_ntext] [ntext]  NULL ,
	[t_numeric] [numeric](38, 19) NULL ,
	[t_nvarchar] [nvarchar] (2000)  NULL ,
	[t_varchar] [varchar] (2000)  NULL ,
	[t_real] [real] NULL ,
	[t_smalldatetime] [smalldatetime] NULL ,
	[t_smallint] [smallint] NULL ,
	[t_smallmoney] [smallmoney] NULL ,
--	[t_sql_variant] [sql_variant] NULL ,
	[t_text] [text]  NULL ,
	[t_timestamp] [timestamp] NULL ,
	[t_tinyint] [tinyint] NULL ,
	[t_uniqueidentifier] [uniqueidentifier] NULL ,
	[t_varbinary] [varbinary] (2000) NULL ,
	CONSTRAINT [PK_type_test] PRIMARY KEY  CLUSTERED 
	(
		[id]
	)  
)
")



(defun ss-type-test (con)
  (if (/= 0 (caar (exec-query con "select count(1) from sysobjects where name = 'type_test'")))
    (exec-command con "drop table type_test"))
  (exec-command con  *sql-server-type_test-ddl*)
  (exec-command con "insert into type_test (id) values(1)")
  (exec-update con "
  update type_test set     
    t_bigint=256*256*256*127,
    t_binary = convert(binary(50), newid()),
    t_bit = 1,
    t_char ='1234567890',
    t_datetime = getdate(),
    t_decimal  = 123456789.123456789,
    t_float = 1.0/3.0,
    t_money = 1.0/.70,
    t_nchar = '12356788',
    t_numeric = convert(numeric(38,19),1)/convert(numeric(38,19),11),
    t_nvarchar = replicate('abcdefghi',200),
    t_real = 1.0/99.0,
    t_smalldatetime = '2004-6-7 12:58:59',
    t_smallint = 12,
	  t_smallmoney = 5678,
	  --t_sql_variant= 'a',
	 --[t_text] [text]  NULL ,
	 --[t_timestamp] [timestamp] NULL ,
	  t_tinyint =1,
	  t_uniqueidentifier = newid(),
	  t_varbinary = convert(varbinary(10), 'aabbccddeeff11223344')
   where id = 1
"
               )
  (commit con)
  (exec-query con "
   select t_bigint,
          t_binary,
          t_bit,
          t_char,
          t_datetime,
          t_float,
          t_money,
          t_nchar,
          t_numeric,
          t_nvarchar,
          t_real,
          t_smalldatetime,
          t_smallint,
          t_smallmoney,
          t_tinyint,
          t_uniqueidentifier,
          t_varbinary 
    from type_test where id =1
")
  (let ((stm (prepare-statement con "
   update type_test set t_image =?,t_text=?,t_ntext=? where id  = 1"
                                '(:blob :in) '(:clob :in) '(:clob :in))))
    (exec-prepared-update stm  
                           (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 33)
                           (make-string 10001 :initial-element #\o)
                           (make-string 10001 :initial-element #\o)
                           ))
  (commit con))

(defun ss-drop-test-proc (con proc)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from sysobjects where name='~A'" proc))))
    (exec-command con (format nil "drop procedure ~A" proc))))

(defun ss-drop-test-table (con table)
  (unless (zerop (caar (exec-query con (format nil "select count(*) 
    from sysobjects where name='~A'" table))))
    (exec-command con (format nil "drop table ~A" table))))



(defun ss-test1 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a int,@b int out
  as
   set @b=@a+1
 ")
  (commit con)
  (let ((stm (prepare-statement con "{call test99(?,?)}" 
                                '(:integer :in) 
                                '(:integer :out))))
    (assert (= 2 (first (exec-prepared-command stm 1))))
    (free-statement stm)))


(defun ss-test2 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a varchar(200),@b varchar(200) out
  as
   set @b=@a
 ")
  (commit con)
  (let ((stm (prepare-statement con "{call test99(?,?)}" 
                                '(:string :in) 
                                '(:string :out))))
    (let ((str (make-funny-string 40)))
      (assert (equal str (first (exec-prepared-command stm str))))
      (free-statement stm)))
  (commit con))

(defun ss-test3 (con)
  (let ((*universal-time-to-date-dataype* 'write-to-string)
        (*date-datatype-to-universal-time* 'parse-integer))
    (ss-drop-test-proc con "test99")
    (let ((a (caar (exec-query con "select getdate()"))))
      (exec-command con "create procedure 
    test99 @a datetime,@b datetime out as 
    set @b= dateadd(d,1,@a);return 789 ")
      (with-prepared-statement 
          (stm con "{?=call test99(?,?)}" 
               '(:integer :out) '(:date :in) '(:date :out))
        (let ((res (exec-prepared-command stm "3323283742")))
          (assert (equal res (list 789 (write-to-string (+ 3323283742 86400))))))))
    (commit con)))

(defun ss-test4 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a varchar(1000) out ,@b varchar(1000) out as
    declare @x varchar(1000); set @x=@a;set @a=@b; set @b=@x ;return 99")
  (with-prepared-statement (stm con "{?=call test99(?,?)}" 
                                '(:integer :out) '(:string :inout) 
                                '(:string :inout))
    (let ((res (exec-prepared-command stm "abc" "xyz")))
      (assert (equal res (list 99 "xyz" "abc"))))))
 


(defun ss-test5 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 
    @a uniqueidentifier ,@b uniqueidentifier out as set @b=@a;")
  (with-prepared-statement (stm con "{call test99(?,?)}" 
                                '(:binary :in) '(:binary :out))
    (let* ((guid (caar (exec-query con "select newid()")))
           (res (exec-prepared-command stm guid)))
      (assert (equalp guid (first res))))
    (commit con)))

(defun ss-test6 (con)
  (let ((*universal-time-to-date-dataype* 'universal-time-list)
        (*date-datatype-to-universal-time* 'list-universal-time))

    (ss-drop-test-proc con "test99")
    (exec-command con "create procedure test99
   @a datetime, @b datetime out as set @b=dateadd(d,2,@a)")
    (with-prepared-statement (stm con "{call test99(?,?)}" 
                                  ':date 
                                  '(:date :out))
      (let ((res (exec-prepared-command stm '(:date 2003 3 4))))
        (assert (equal res '((:date 2003 3 6 0 0 0))))))
    (let ((res (exec-query con "select dateadd(s,86399,'2005-6-7')")))
      (assert (equal res '(((:date 2005 6 7 23 59 59))))))))
  
(defun ss-test7 (con)
  (let ((filename (namestring (merge-pathnames "odb-trace-test.log" *test-temp-dir*))))
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
    (assert (not (probe-file filename)))
    ))

(defun ss-test8 (con)
  (let ((res (caar (exec-query con "select nchar(660)"))))
    (assert (equal (string (code-char 660)) res))))

(defun ss-test8a (con)
  (with-prepared-statement (stm 
                            con
                            "select convert(nvarchar(20),?),convert(varchar(20),?)"
                            '(:string :in) 
                            '(:unicode-string :in))
    (let* ((strings '("hjgkhgkzt65646&%2" "nnvfdsfsfz6tztB0#="))
           (res (exec-prepared-query stm (first strings) (second strings))))
      (assert (equal res (list strings))))))



(defun ss-test9 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b text)")
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

(defun ss-test10 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b image)")
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

    

(defun ss-test11 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b ntext)")
  (commit con)
  (with-prepared-statement (stm con "insert into test999 (a,b) values(?,?)" 
                                '(:integer :in) '(:unicode-clob :in))
    (let ((mp plain-odbc::*max-precision*))
      (dolist (len (list 8 1 2 3 4 
                         5 900 9000 8192 8000 
                         (1- mp) 
                         mp 
                         (1+ mp)
                         (* 2 mp)
                         (1- (* 2 mp))
                         (1+ (* 2 mp)))
                        ) 
        ;(pprint len)
        (let ((string (make-funny-string 
                       len 
                       (coerce (list (code-char 2341) (code-char 2347) #\a) 'vector ))))
          (exec-prepared-update stm  len string)
          (let ((res (exec-query con (format nil "select b from test999 where a=~A" len))))
          (assert (equal res
                         (list (list string)))))))))

;    (exec-command con "drop table test999")
    (commit con)
    )


(defun ss-test12 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a nvarchar(1000) out ,@b nvarchar(1000) out as declare @x nvarchar(1000); set @x=@a;set @a=@b; set @b=@x ;return 99")
  (with-prepared-statement (stm con "{?=call test99(?,?)}" 
                                '(:integer :out) '(:unicode-string :inout) 
                                '(:unicode-string :inout))
    (let ((str1 (make-funny-string 700  (coerce (list (code-char 2341) (code-char 2347) #\a) 'vector )  ))
          (str2 (make-funny-string 900   (coerce (list (code-char 2341) (code-char 2347) #\a) 'vector ))))
      
      (let ((res (exec-prepared-command stm str1 str2)))
        (assert (equal res (list 99 str2 str1)))))))
 
(defun ss-test13 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a float,@b float out as 
                     set @b=@a+1 ;return 99")
  (with-prepared-statement (stm con "{?=call test99(?,?)}" 
                                '(:integer :out) '(:double :in) '(:double :out))
    (let ((res (exec-prepared-command stm 1.8)))
      (assert (= (second res) 2.8)))))

    
(defun ss-test14 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "create procedure test99 @a float as 
                     select @a+1 as a")
  (with-prepared-statement (stm con "{call test99(?)}" 
                                '(:double :in))
    (let ((res (exec-prepared-query stm 1.8)))
      (assert (= (caar res) 2.8)))))


(defun ss-test15(con)
  (let ((res (exec-query con "select (convert(datetime,'2005-6-7 13:04:45' )) as a")))
    (assert (= (encode-universal-time 45 4 13 7 6 2005) (caar res)))))

(defun ss-test16(con)
  (with-prepared-statement (stm con "declare @d datetime; set @d=?; select convert(varchar(40),@d,120)"
                                '(:date :in))
    (let ((res (exec-prepared-query stm (encode-universal-time 1 2 3 13 10 2005))))
      (assert (equalp "2005-10-13 03:02:01" (caar res))))))


(defun ss-test17 (con)
  (let ((row
          (first (exec-query con "select ?,?,?,? " 12342 1d3 "bla" #(1 2 3 4)))))
    (assert (equal '(12342 1d3 "bla") (subseq row 0 3)))
    (assert (equal (coerce (fourth row) 'list) '(1 2 3 4)))))


(defun ss-test18 (con)
  (let* ((str1 (make-string 1000 :initial-element #\p))
        (str2 (make-string 1900 :initial-element #\k))
        (id1 (random 10000))
        (id2 (+ id1 (random 10000))))
    ;(pprint 1)
    (exec-update con "delete from type_test where id in (?,?)" id1 id2)
    (exec-update con "insert into type_test (id,t_varchar) values(?,?);
                  insert into type_test (id,t_varchar) values(?,?)" 
                 id1 str1 id2 str2)
    (multiple-value-bind (r1 m1 r2 m2)
        (exec-query con "select id,t_varchar from type_test where id=? ;
                        select id as a,t_varchar as b from type_test where id=?"
                    id1 id2 )
      (assert (and
                (equal r1 (list (list id1 str1)))
                (equal r2 (list (list id2 str2)))
                (equal m1 '("id" "t_varchar"))
                (equal m2 '("a" "b")))))
    (commit con)))

(defun ss-test19 (con)
  (let* ((str1 (make-string 100000 :initial-element #\p))
         (str2 (make-string 190000 :initial-element #\k))
         (id1 (random 10000))
         (id2 (+ id1 (random 10000))))
    (exec-update con "delete from type_test where id in (?,?)" id1 id2)
    (exec-update con "insert into type_test (id,t_text) values(?,?);
                  insert into type_test (id,t_text) values(?,?)" 
                 id1 (list str1 :clob) id2 (list str2 :clob))
    (multiple-value-bind (r1 m1 r2 m2)
         (exec-query con "select id,t_text from type_test where id=? ;
                        select id as a,t_text as b from type_test where id=?"
                    id1 id2 )
      (assert (and
                (equal r1 (list (list id1 str1)))
                (equal r2 (list (list id2 str2)))
                (equal m1 '("id" "t_text"))
                (equal m2 '("a" "b")))))
     (commit con)))

(defun ss-test20 (con)
  (dotimes (i 100)
    (let* ((str (make-string 100 :initial-element #\p))
           (binary (make-array 1000 :initial-element (random 256)))
           (id (random 1000)))
      (exec-update con "delete from type_test where id =?" id)
      (exec-update con "insert into type_test (t_image,id,t_text) values(?,?,?)" 
                   (list binary :blob) id (list str :clob))
      (multiple-value-bind (r1 m1)
          (exec-query con 
                      "select id aaa,t_image bbb,t_text ccc from type_test where id=?"
                      id)
        (assert (equalp r1 (list (list id (coerce binary '(vector (unsigned-byte 8))) str))))
        (assert (equal m1 '("aaa" "bbb" "ccc")))
        (commit con)))))


(defun ss-test21 (con)
   (let ((*universal-time-to-date-dataype* 'universal-time-list)
         (*date-datatype-to-universal-time* 'list-universal-time)
         (*date-type-predicate* 'date-lisp-p))
     (let ((res (exec-query con "select dateadd(d,1,?)" 
                            '((:date 2005 4 5) :date))))
           (assert (equal res '(((:date 2005 4 6 0 0 0))))))))



(defun ss-test22 (con)
  (let ((res (first 
              (exec-query con "
          select 
           ? as t_double,
           ? as t_integer,
           ? as t_varchar,
           ? as t_varbinary"
                          1223455.334 12345 "blablablub" #(1 2 3 4)))))
    (assert (equal 
             '(1223455 12345 "blablablub" (1 2 3 4))
             (list (truncate (first res))
                   (second res)
                   (third res)
                   (coerce (fourth res) 'list))))))


(defun ss-test23 (con)
  (ss-drop-test-proc con "test99")
  (exec-command con "
     create procedure test99 
     @p1 integer,
     @p2 varchar(200),
    @p3 int out,
    @p4  varchar(2000) out as 
   begin
      set @p3=3*@p1;
      set @p4='a'+ @p2 + '#'+ @p2 +'x'
      select @p3 as a,@p4 as b;
      select @p4 as bb,@p3 as aa;
   end")
  (let* ((teststr "abcdefghijklmnopqrstuvwxyz")
        (testint 12345678)
        (p4 (format nil "a~A#~Ax" teststr teststr))
        (p3 (* 3 testint)))
   (multiple-value-bind (c resultsets params)
       (exec-sql con "{call test99 (?,?,?,?)}" 
                 testint teststr '(nil :integer :out) '(nil :string :out))
     (assert (equal params (list p3 p4)))
     (let* ((res1 (first resultsets))
            (res2 (second resultsets))
            (row1 (first (first res1)))
            (row2 (first (first res2))))

       (assert (equal row1 (list p3 p4)))
       (assert (equal row2 (list p4 p3)))
       (assert (equal (second res1) '("a" "b")))
       (assert (equal (second res2) '("bb" "aa")))))))


(defun ss-test24 (con)
  (dolist (x '(234 123 237))
  (let ((res (exec-query con (format nil "select char(~A)as a" x))))
    (assert (= x (char-code (char (first (first res)) 0)))))))

(defun ss-test25 (con)
  (dolist (x '(234 123 237))
  (let ((res (exec-query con (format nil "select ascii('~A') as a" (code-char x)))))
    (assert (= x (first (first res)))))))

(defun ss-test26 (con)
  (dolist (x '(234 123 237))
    (let ((res (exec-query con "select ? as a" (string (code-char x)))))
      (assert (= x (char-code (char (first (first res)) 0)))))))

;; tests for 


(defun ss-mk-metadatatest (con)
  (ss-drop-test-table con "metadatatest")
  (exec-command con "
    CREATE TABLE metadatatest(
	x int NOT NULL,
        y varchar(10),
        z datetime,
       CONSTRAINT metadatatest_pk PRIMARY KEY CLUSTERED (x,y)) "))

; for sql server:  
;  catalog <-> database
;  schema  <-> owner  (z.B. dbo)
;   and "" <> nil/null

(defun schema-loop (con fun)
  (let ((schemainfo (first (exec-query con "SELECT SCHEMA_NAME(),DB_NAME()"))))
    (dolist (schema (list nil (first schemainfo)))
      (dolist (catalog (list nil (second schemainfo)))
        (funcall fun catalog schema)))))
  
(defun ss-test27 (con)
  (ss-mk-metadatatest con)
  (schema-loop
   con 
   (lambda (catalog schema)
     (multiple-value-bind
         (res cols) 
         (get-primary-keys con catalog schema "metadatatest")
       (assert (= 2 (length res)))
       (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "COLUMN_NAME" "KEY_SEQ" "PK_NAME")))))))

(defun ss-test28 (con) 
  (ss-mk-metadatatest con)
   (schema-loop
    con 
    (lambda (catalog schema)
      (multiple-value-bind 
          (res cols)
          (get-columns con catalog schema "metadatatest" nil)
        (assert (= 3 (length res)))
        (assert (equal (subseq cols 0 18) 
                       '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "COLUMN_NAME" "DATA_TYPE" "TYPE_NAME" 
                         "COLUMN_SIZE" "BUFFER_LENGTH" "DECIMAL_DIGITS" "NUM_PREC_RADIX" "NULLABLE" 
                         "REMARKS" "COLUMN_DEF" "SQL_DATA_TYPE" "SQL_DATETIME_SUB" "CHAR_OCTET_LENGTH" 
                         "ORDINAL_POSITION" "IS_NULLABLE")))))))



(defun ss-test29 (con)
  (ss-mk-metadatatest con)
  (schema-loop
   con 
   (lambda (catalog schema)
     (multiple-value-bind 
         (res cols)
         (get-tables con catalog schema "metadatatest" nil)
       (assert (= 1 (length res)))
       (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "TABLE_TYPE" "REMARKS")))))))
  
(defun ss-test30 (con)
  (ss-mk-metadatatest con)
  (ss-drop-test-table con "metadatatest2")
  (exec-command con "
    CREATE TABLE metadatatest2(
	a int NOT NULL,
        b varchar(10),
        c datetime,
       CONSTRAINT metadatatest2_pk PRIMARY KEY CLUSTERED (a,b)) ")
  (exec-command con "alter table metadatatest add constraint metadatatest_fk1 foreign key (x,y) references metadatatest2(a,b)")
  (schema-loop
   con 
   (lambda (catalog schema)
     (multiple-value-bind 
         (res1 cols1)
         (get-foreign-keys con nil nil "metadatatest2"
                           nil nil nil)
       (assert (= 2 (length res1)))
       (assert (equal cols1  '("PKTABLE_CAT" "PKTABLE_SCHEM" "PKTABLE_NAME" "PKCOLUMN_NAME" "FKTABLE_CAT" "FKTABLE_SCHEM" "FKTABLE_NAME" "FKCOLUMN_NAME" "KEY_SEQ"
                               "UPDATE_RULE" "DELETE_RULE" "FK_NAME" "PK_NAME" "DEFERRABILITY")))
       (multiple-value-bind 
           (res2 cols2)
           (get-foreign-keys con nil nil nil
                             nil nil "metadatatest")
         (assert (= 2 (length res2))))))))


(defun ss-test31 (con)
  (ss-mk-metadatatest con)
  (schema-loop
   con 
   (lambda (catalog schema)
     (multiple-value-bind 
         (res cols)
         (get-tables con catalog schema "metadatatest" "TABLE")
       (assert (= 1 (length res)))
       (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "TABLE_TYPE" "REMARKS")))))))

(defun ss-test32 (con)
  (ignore-errors (exec-command con "drop view metadatatest_vw"))
  (exec-command con "create view metadatatest_vw as select 1 as a")
  (schema-loop
   con 
   (lambda (catalog schema)
     (dolist (type '("VIEW" nil))
       (multiple-value-bind 
           (res cols)
           (get-tables con catalog schema "metadatatest_vw" type)
         (assert (= 1 (length res)))
         (assert (equal cols '("TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME" "TABLE_TYPE" "REMARKS"))))))))

