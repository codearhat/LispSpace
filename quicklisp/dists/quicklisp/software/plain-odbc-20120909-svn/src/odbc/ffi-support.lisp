;;; -*- Mode: lisp -*-

(in-package :plain-odbc)


(defmacro cffi-0.10-or-0.9 (form-0.10 form-0.9)
  (if (BOUNDP 'cffi::*foreign-string-mappings*)
      form-0.10
    form-0.9))

(cffi-0.10-or-0.9
 (defun get-string (ptr length) 
   (cffi:foreign-string-to-lisp ptr :count length :encoding :iso-8859-1)
   )
 (defun get-string (ptr length)
   (cffi:foreign-string-to-lisp ptr length nil)
   )
 )
 
(cffi-0.10-or-0.9
 (defun get-string-nts (ptr)
   (cffi:foreign-string-to-lisp ptr :max-chars MOST-POSITIVE-FIXNUM :encoding :iso-8859-1))
 (defun get-string-nts (ptr)
   (cffi:foreign-string-to-lisp ptr MOST-POSITIVE-FIXNUM t)))

(cffi-0.10-or-0.9
 (defun put-string (ptr vector)
   (cffi:lisp-string-to-foreign vector ptr (1+ (length vector)) :encoding :iso-8859-1))
 (defun put-string (ptr vector)
   (cffi:lisp-string-to-foreign vector ptr (1+ (length vector)))))

(defun %null-ptr () 
  (cffi:null-pointer))


(cffi-0.10-or-0.9
 (defmacro with-foreign-string-alloc ((ptr text) &body body)
   `(cffi:with-foreign-string (,ptr ,text :encoding :iso-8859-1)
                              ,@body))
 
 (defmacro with-foreign-string-alloc ((ptr text) &body body)
   `(cffi:with-foreign-string (,ptr ,text)
                              ,@body)))
                            

(defun get-byte-vector (ptr length)
  (let ((res (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (i length)
        (setf (aref res i) (cffi:mem-aref ptr :uint8 i)))
    res))


(defun put-byte-vector (ptr vector)
  (dotimes (i (length vector))
      (setf (cffi:mem-aref ptr :uint8 i) (aref vector i))))

  
(defun wchar-bytes-to-string (byte-vector)
  (let ((res (make-string (truncate (length byte-vector) 2) :initial-element (code-char 1000))))
    (dotimes (i (truncate (length byte-vector) 2) res)
      (setf (char res i)
              (code-char (+ (aref byte-vector (* 2 i)) (* (aref byte-vector (+ (* 2 i) 1)) 256)))))))

(defun string-to-wchar-bytes (string)
  (let ((vec (make-array (* 2 (length string)) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string))
      (let ((k (char-code (char string i))))
        (setf (aref vec (* 2 i)) (logand k 255)
              (aref vec (1+ (* 2 i))) (ash k -8))))
    vec))

(defun %put-unicode-string (ptr string)
  (put-byte-vector ptr (string-to-wchar-bytes string)))


(defun %get-unicode-string (ptr len)
  (wchar-bytes-to-string (get-byte-vector ptr len)))





(defun alloc-chars (size)
  (cffi:foreign-alloc :char :count (1+ size)))
   

;(defun make-unicode-string (length)
;  #+clisp
;  (make-string length)
;  #+allegro
;  (make-string length )
;  #+lispworks
;  (make-string length :initial-element (code-char 300)))