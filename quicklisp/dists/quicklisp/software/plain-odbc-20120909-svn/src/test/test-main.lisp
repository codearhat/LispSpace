;;; -*- Mode: lisp -*-
;;; utility code for testing

(defpackage :test-plain-odbc
  (:use :common-lisp :plain-odbc)
  )

(in-package :test-plain-odbc)

(defparameter *test-temp-dir*
  #+(or mswindows win32 :windows) "c:/temp/"
  "/tmp/"
  )

(defun make-funny-string (len &optional (source "abcdefghijklmnopqrstuvw12345!"))
  (let ((string (make-string len :initial-element (code-char 1000))))
    (dotimes (i len string)
      (setf (char string i) (aref source (random (length source)))))))
 
(defun make-funny-bytes (len)
  (let ((bytes (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len bytes)
      (setf (aref bytes i) (random 256)))))

(defun universal-time-list (time)
  (cons :date (reverse (subseq (multiple-value-list (decode-universal-time time)) 0 6  ))))


(defun date-lisp-p (list)
  (and (list list)
       (eql :date (car list))))

(defun lpad (str len)
  (let ((l (length str)))
    (if (>= l len) str
      (concatenate 'string (make-string (- len l) :initial-element #\space) str))))

(defun rpad (str len)
  (let ((l (length str)))
    (if (>= l len) str
      (concatenate 'string str (make-string (- len l) :initial-element #\space)))))


(defun list-universal-time (list)
  (assert  (date-lisp-p list) ())
  (setf list (cdr list))
  (encode-universal-time         
   (or (sixth list) 0)
   (or (fifth list) 0)
   (or (fourth list) 0)
   (or (third list) 1)
   (or (second list) 1)
   (or (first list) 1900)))

(load (merge-pathnames "test-sql-server.lisp" *load-truename*))
(load (merge-pathnames "test-oracle.lisp" *load-truename*))
(load (merge-pathnames "test-mysql.lisp"  *load-truename*))
(load (merge-pathnames "test-sqlite.lisp"  *load-truename*))
