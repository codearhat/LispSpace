;;; -*- Mode: lisp -*-

(in-package :test-plain-odbc)

; 
; (setf *con* (PLAIN-ODBC:CONNECT-GENERIC :dsn "dsn-sqlite" :database "c:/temp/sqllitedb.sqd"))

(export '(run-sqlite-tests))

(defun run-sqlite-tests (con)
  (flet ((doit () 
           (dolist (sym '(
                          sqlite-type-test
                          sqlite-test1
                          sqlite-test2
                          sqlite-test4
                          sqlite-test5
                          sqlite-test10 
                          sqlite-test20 
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


(defparameter *sqlite-type_test-ddl* "
create table type_test
 (id int,
 t_integer integer,
 t_double real,
 t_text text,
 t_blob blob)")




(defun sqlite-type-test (con)
  (ignore-errors (exec-command con "drop table type_test"))
  (exec-command con *sqlite-type_test-ddl*)
  (exec-update con "insert into type_test (id) values(1)")
  (exec-update con "
  update type_test set
    t_integer =1234567890,
    t_double = 1.0/3.0,
   t_text = 'abcdefgh',
  t_blob = '9876543210'
  where id = 1")
  (commit con)
  (let ((res (exec-query con "
    select t_integer,
           t_double,
           t_text,
           t_blob
   from type_test where id =1"))
        ;; the following is not equal to  (/ 1.0d0 3.0d0)
        (a (read-from-string "0.333333333333333d0")))
    (assert (= (length res) 1))
    (setf res (first res))
    (assert (= (nth 0 res) 1234567890))
    (assert (= (nth 1 res) a))
    (assert (equal (nth 2 res) "abcdefgh"))
    (assert (equal (coerce (nth 3 res) 'list) (map 'list 'char-code "9876543210")))))


(defun sqlite-test1 (con)
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

(defun sqlite-test2 (con)
  (let ((str (make-funny-string 245)))
    (exec-update con "delete from type_test where id=99")
    (with-prepared-statement (stm con 
                                  "insert into type_test (id,t_text) values (99,?)"
                                  '(:string :in))
      (exec-prepared-update stm str))
  (with-prepared-statement (stm con 
                                "select t_text from type_test where id=99" )
    (let ((res (exec-prepared-query stm )))
      (assert (string= (caar res) str))))))


(defun sqlite-test4 (con)
  (exec-update con "delete from type_test where id=99")
  (with-prepared-statement (stm con "insert into type_test (id,t_double) values(99,?)" 
                                '(:double :in))
    (exec-prepared-update stm 1.8d0))
  (let ((res (exec-query con "select t_double+1 from type_test where id=99")))
    (assert (<= (abs (- (caar res) 2.8d0)) 1d-15))))


(defun sqlite-test5 (con)
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


(defun sqlite-test10 (con)
  (ignore-errors 
    (exec-command con "drop table test999"))
  (exec-command con "create table test999 (a int,b blob)")
  (commit con)
  (with-prepared-statement (stm con "insert into test999 (a,b) values(?,?)" 
                                '(:integer :in) 
                                '(:blob :in))
    (let* ((mp plain-odbc::*max-precision*)
          (l1 (list 0 1 2 3 4 5 900 9000 8192 8000 
                         (1- mp) 
                         mp ))
          (l2 (list (1+ mp)
                    (* 2 mp)
                    (1- (* 2 mp))
                    (1+ (* 2 mp)))))
      ;; this is why use USE-BIND-COLUMN
      (dolist (len (append l1 (if (use-bind-column con) nil l2)))
        (let ((byte-vec (make-funny-bytes len)))
          (exec-prepared-update stm len byte-vec)
          (let ((res (exec-query con (format nil "select b from test999 where a=~A" len))))
          (assert (equalp res
                         (list (list byte-vec)))))))))
    (exec-command con "drop table test999")
    (commit con)
    )


(defun sqlite-test20 (con)
  (ignore-errors (exec-command con  "drop table type_test"))
  (exec-command con *mysql-type_test-ddl*)
  (dotimes (i 100)
    (let* ((str (make-string 100 :initial-element #\p))
           (binary (make-array 1000 :initial-element (random 256)))
           (id (random 1000)))
      (exec-update con "delete from type_test where id =?" id)
      (exec-update con "insert into type_test (t_blob,id,t_text) values(?,?,?)" 
                   (list binary :blob) id (list str :clob))
      (multiple-value-bind (r1 m1)
          (exec-query con 
                      "select id aaa,t_blob bbb,t_text ccc from type_test where id=?"
                      id)
        (assert (equalp r1 (list (list id (coerce binary '(vector (unsigned-byte 8))) str))))
        (assert (equal m1 '("aaa" "bbb" "ccc")))
        (commit con)))))

