;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;;;
;;;; Transformers
;;;;
;;;; A transformer is a function. There are reader and writer
;;;; transformers. The slot value refers the value that will be
;;;; returned, or has been passed in to slot-value,
;;;; slot-value-using-class and their friends.  The rdbms values is a
;;;; sequence sent or received from the RDBMS backend, that is a
;;;; tuple.
;;;;
;;;; A reader transformer returns the slot value by reading the
;;;; sequence of rdbms values starting from the given index.  It may
;;;; read one or multiple elements from the sequence according to the
;;;; type mapping, may parse or interpret the received data.  If the
;;;; reader is unable to determine the slot value it must return
;;;; +type-error-marker+.
;;;;
;;;; A writer transformer writes the rdbms values corresponding to a
;;;; slot value into a sequence starting at the given index.  It may
;;;; write one or multiple elements into the sequence according to the
;;;; type mapping. If the writer is unable to determine the rdbms
;;;; values it must return +type-error-marker+.

;;;;;;
;;; Unbound

(def function unbound-reader (rdbms-values index)
  (if (eq :null (elt rdbms-values index))
      +unbound-slot-marker+
      +type-error-marker+))

(def function unbound-writer (slot-value rdbms-values index)
  (if (unbound-slot-marker-p slot-value)
      (setf (elt rdbms-values index) :null)
      +type-error-marker+))

;;;;;;
;;; Null

(def function null-reader (rdbms-values index)
  (if (eq :null (elt rdbms-values index))
      nil
      +type-error-marker+))

(def function null-writer (slot-value rdbms-values index)
  (if (null slot-value)
      (setf (elt rdbms-values index) :null)
      +type-error-marker+))

;;;;;;
;;; Nil

(def function nil-reader (rdbms-values index)
  (declare (ignore rdbms-values index))
  +type-error-marker+)

(def function nil-writer (slot-value rdbms-values index)
  (declare (ignore slot-value rdbms-values index))
  +type-error-marker+)

;;;;;;
;;; Combined

(def function combined-reader (readers)
  (lambda (rdbms-values index)
    (loop
       for reader :in readers
       do (bind ((slot-value (funcall reader rdbms-values index)))
            (unless (eq slot-value +type-error-marker+)
              (return slot-value)))
       finally (return +type-error-marker+))))

(def function combined-writer (writers)
  (lambda (slot-value rdbms-values index)
    (loop
       for writer :in writers
       do (bind ((primary-rdbms-value (funcall writer slot-value rdbms-values index)))
            (unless (eq primary-rdbms-value +type-error-marker+)
              (return primary-rdbms-value)))
       finally (return +type-error-marker+))))

;;;;;;
;;; Tagged

(def function tagged-reader (type-tags readers)
  (lambda (rdbms-values index)
    (loop
       with rdbms-type-tag = (elt rdbms-values index)
       for type-tag :in type-tags
       for reader :in readers
       when (eq type-tag rdbms-type-tag)
       do (bind ((slot-value (funcall reader rdbms-values (1+ index))))
            (unless (eq slot-value +type-error-marker+)
              (return slot-value)))
       finally (return +type-error-marker+))))

(def function tagged-writer (type-tags writers)
  (lambda (slot-value rdbms-values index)
    (loop
       for type-tag :in type-tags
       for writer :in writers
       do (bind ((primary-rdbms-value (funcall writer slot-value rdbms-values (1+ index))))
            (unless (eq +type-error-marker+ primary-rdbms-value)
              (setf (elt rdbms-values index) type-tag)
              (return primary-rdbms-value)))
       finally (return +type-error-marker+))))

;;;;;;
;;; Serialized

(def constant +persistent-object-by-oid-code+ #x60)

(def (function o) deserializer-mapper (code context)
  (if (eq code +persistent-object-by-oid-code+)
      #'read-persistent-object-by-oid
      (hu.dwim.serializer::default-deserializer-mapper code context)))

(def (function o) serializer-mapper (object context)
  (bind (((:values code has-identity writer-function)
          (hu.dwim.serializer::default-serializer-mapper object context)))
    (if (and (eq code hu.dwim.serializer::+standard-object-code+)
             (typep object 'persistent-object))
        (values +persistent-object-by-oid-code+ #t #'write-persistent-object-by-oid)
        (values code has-identity writer-function))))

(def (function o) write-persistent-object-oid (oid context)
  (hu.dwim.serializer::write-integer (oid-class-id oid) context)
  (hu.dwim.serializer::write-integer (oid-instance-id oid) context))

(def (function o) read-persistent-object-oid (context)
  (revive-oid (hu.dwim.serializer::read-integer context)
              (hu.dwim.serializer::read-integer context)))

(def hu.dwim.serializer::serializer-deserializer persistent-object-by-oid +persistent-object-by-oid-code+ persistent-object
  (write-persistent-object-oid (oid-of hu.dwim.serializer::-object-) hu.dwim.serializer::-context-)
  (hu.dwim.serializer::announce-identity (load-instance (read-persistent-object-oid hu.dwim.serializer::-context-) :skip-existence-check #t) hu.dwim.serializer::-context-))

(def function byte-vector->object-reader (rdbms-values index)
  (deserialize (elt rdbms-values index) :deserializer-mapper #'deserializer-mapper))

(def function object->byte-vector-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (serialize slot-value :buffer-size 10240 :serializer-mapper #'serializer-mapper)))

;;;;;;
;;; Identity

(def function identity-reader (rdbms-values index)
  (elt rdbms-values index))

(def function identity-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index) slot-value))

;;;;;;
;;; Number

(def function object->number-reader (rdbms-values index)
  (bind ((rdbms-value (elt rdbms-values index)))
    (if (typep rdbms-value 'number)
        rdbms-value
        (parse-number rdbms-value))))

;;;;;;
;;; Integer

(def function object->integer-reader (rdbms-values index)
  (bind ((rdbms-value (elt rdbms-values index)))
    (if (typep rdbms-value 'integer)
        rdbms-value
        (parse-integer rdbms-value))))

;;;;;;
;;; Symbol

(def function string->symbol-reader (rdbms-values index)
  (canonical-name->symbol (elt rdbms-values index)))

(def function symbol->string-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index) (symbol->canonical-name slot-value)))

;;;;;;
;;; List

;; FIXME these are kinda useless in this form, because their results depend on a dosen of special variables, and they accept non-list values, read-from-string is not safe, etc...
;; at the very least rename them to suggest read-from-string being called
(def function string->list-reader (rdbms-values index)
  (read-from-string (elt rdbms-values index)))

(def function list->string-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index) (write-to-string slot-value)))

;;;;;;
;;; Boolean

(def function char->boolean-reader (rdbms-values index)
  (bind ((rdbms-value (elt rdbms-values index)))
    (cond ((eq #\t rdbms-value) #t)
          ((eq #\f rdbms-value) #f)
          (t +type-error-marker+))))

(def function boolean->char-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (if slot-value
            #\t
            #\f)))

(def function integer->boolean-reader (rdbms-values index)
  (bind ((rdbms-value (elt rdbms-values index)))
    (cond ((= 0 rdbms-value) #t)
          ((= 1 rdbms-value) #f)
          (t +type-error-marker+))))

(def function boolean->integer-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (if slot-value
            1
            0)))

(def function string->boolean-reader (rdbms-values index)
  (bind ((rdbms-value (elt rdbms-values index)))
    (cond ((equal "t" rdbms-value) #t)
          ((equal "f" rdbms-value) #f)
          (t +type-error-marker+))))

(def function boolean->string-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (if slot-value
            "TRUE"
            "FALSE")))

(def function object->boolean-reader (rdbms-values index)
  (bind ((rdbms-value (elt rdbms-values index)))
    (cond ((eq #t rdbms-value) #t)
          ((eq #f rdbms-value) #f)
          ((eq #\t rdbms-value) #t)
          ((eq #\f rdbms-value) #f)
          ((and (typep rdbms-value 'integer)
                (= 0 rdbms-value)) #f)
          ((and (typep rdbms-value 'integer)
                (= 1 rdbms-value)) #t)
          ((equal "t" rdbms-value) #t)
          ((equal "f" rdbms-value) #f)
          ((equal "TRUE" rdbms-value) #t)
          ((equal "FALSE" rdbms-value) #f)
          (t +type-error-marker+))))

;;;;;;
;;; Member

(def function type-member-elements (type)
  (cdr (if (eq 'member (first type))
           type
           (find 'member type
                 :key [when (listp !1)
                        (first !1)]))))

(def function integer->member-reader (type)
  (bind ((member-elements (type-member-elements type)))
    (lambda (rdbms-values index)
      (bind ((rdbms-value (elt rdbms-values index)))
        (aif (nth rdbms-value member-elements)
             it
             +type-error-marker+)))))

(def function member->integer-writer (type)
  (bind ((member-elements (type-member-elements type)))
    (lambda (slot-value rdbms-values index)
      (block found
        (loop for i from 0
              for value in member-elements
              when (eq value slot-value)
              do (progn
                   (setf (elt rdbms-values index) i)
                   (return-from found)))
        +type-error-marker+))))

(def function string->member-reader (type)
  (bind ((member-elements (type-member-elements type)))
    (lambda (rdbms-values index)
      (aprog1 (string->symbol-reader rdbms-values index)
        (assert (member it member-elements))))))

(def function member->string-writer (type)
  (bind ((member-elements (type-member-elements type)))
    (lambda (slot-value rdbms-values index)
      (assert (member slot-value member-elements))
      (setf (elt rdbms-values index) (symbol->string-writer slot-value rdbms-values index)))))

;;;;;;
;;; IP address

(def (function o) unsigned-byte-vector->ip-address-vector-reader (rdbms-values index)
  (bind ((bytes (elt rdbms-values index)))
    (cond ((= (length bytes) 4)
           bytes)
          ((= (length bytes) 16)
           (loop
              :with result = (make-array 8 :element-type '(unsigned-byte 16))
              :for output-idx :from 0 :below 8
              :for input-idx  :from 0 :below 16 :by 2
              :do (let ((part 0))
                    (setf (ldb (byte 8 8) part) (aref bytes input-idx))
                    (setf (ldb (byte 8 0) part) (aref bytes (1+ input-idx)))
                    (setf (aref result output-idx) part))
              :finally (return result)))
          (t +type-error-marker+))))

(def (function o) ip-address-vector->unsigned-byte-vector-writer (slot-value rdbms-values index)
  (check-type slot-value vector)
  (assert (or (and (= (length slot-value) 4)
                   (subtypep (array-element-type slot-value) '(unsigned-byte 8)))
              (and (= (length slot-value) 8)
                   (subtypep (array-element-type slot-value) '(unsigned-byte 16)))))
  (bind ((result))
    (cond ((= (length slot-value) 4)
           (setf result (coerce slot-value 'unsigned-byte-vector)))
          ((= (length slot-value) 8)
           (setf result (make-array 16 :element-type '(unsigned-byte 8)))
           (loop
              :with idx = -1
              :for part :across slot-value
              :do (progn
                    (setf (aref result (incf idx)) (ldb (byte 8 8) part))
                    (setf (aref result (incf idx)) (ldb (byte 8 0) part)))))
          (t +type-error-marker+))
    (setf (elt rdbms-values index) result)))
