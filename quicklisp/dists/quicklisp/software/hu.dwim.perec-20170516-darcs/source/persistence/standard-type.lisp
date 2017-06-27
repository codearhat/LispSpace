;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Or
;;;
;;; May be used to combine null and unbound with a primitive type and may generate an extra column.
;;; See compute-reader and compute-writer generic function definitions

(def (persistent-type e) or (&rest types)
  `(or ,@types))

(def method shared-initialize :around ((type or-type) slot-names &rest args &key types &allow-other-keys)
  (apply #'call-next-method type slot-names :types (mapcar #'parse-type types) (remove-from-plist args :types)))

;;;;;;
;;; And
;;;
;;; Not supported

(def (persistent-type e) and (&rest types)
  `(and ,@types))

(def method shared-initialize :around ((type and-type) slot-names &rest args &key types &allow-other-keys)
  (apply #'call-next-method type slot-names :types (mapcar #'parse-type types) (remove-from-plist args :types)))

;;;;;;
;;; Not
;;;
;;; Not supported

(def (persistent-type e) not (negated-type)
  `(not ,negated-type))

(def method shared-initialize :around ((type not-type) slot-names &rest args &key negated-type &allow-other-keys)
  (apply #'call-next-method type slot-names :negated-type (parse-type negated-type) (remove-from-plist args :negated-type)))

;;;;;;
;;; Satisfies

(def (persistent-type e) satisfies (function)
  `(satisfies ,function))

;;;;;;
;;; Nil
;;;
;;; other -> (type-error)

(def (persistent-type e) nil ()
  nil)

(defmapping nil nil
  'nil-reader
  'nil-writer)

;;;;;;
;;; Member
;;;
;;; not found in members -> (type-error)

(def (persistent-type e) member (&rest members)
  `(member ,@members))

(defmapping member (sql-integer-type :bit-size 16)
  (integer->member-reader normalized-type)
  (member->integer-writer normalized-type))

;; KLUDGE: TODO: remove these when type mapping is refactored to use linear-mapping from util
(def macro def-member-type (name &body members)
  `(def persistent-type ,name ()
    `(member ,@',members)))

(def (definer e :available-flags "e") persistent-member-type (name &body members)
  `(def-member-type ,name ,@members))

;;;;;;
;;; Unbound
;;;
;;; unbound -> NULL
;;; t -> type-error

(def (load-time-constant e) +unbound-slot-marker+
  (progn
    (defstruct unbound-slot-marker
      "This structure is used for the unbound slot value marker. The type for that marker must be a subtype of t and cannot be a subtype of any other type.")
    (def method make-load-form ((self unbound-slot-marker) &optional environment)
      (declare (ignore environment))
      '%%%+unbound-slot-marker+)
    (make-unbound-slot-marker)))

(def (persistent-type e) eql (value)
  `(eql ,value))

;; this type must be used to mark slots which might be unbound (e.g. (or unbound integer))
(def (persistent-type e) unbound ()
  `(eql ,+unbound-slot-marker+))

(defmapping unbound :null
  'unbound-reader
  'unbound-writer)

(def method compute-type-tag ((type (eql 'unbound)))
  1)

;;;;;;
;;; Null
;;;
;;; nil -> NULL
;;; t -> (type-error)

(def (persistent-type e) null ()
  'null)

(defmapping null :null
  'null-reader
  'null-writer)

(def method compute-type-tag ((type (eql 'null)))
  2)

;;;;;
;;; t
;;;
;;; unbound -> NULL, NULL
;;; nil -> true, NULL
;;; other -> true, (byte-vector)

(def (persistent-type e) t ()
  t)

(defmapping t (sql-binary-large-object-type)
  'byte-vector->object-reader
  'object->byte-vector-writer)

;;;;;;
;;; Serialized
;;;
;;; unbound -> (type-error)
;;; nil -> (type-error)
;;; other -> (byte-vector)

(def function maximum-serialized-size-p (serialized)
  (declare (ignore serialized))
  t)

(def (persistent-type e) serialized (&optional byte-size)
  (declare (ignore byte-size))
  '(and (not unbound)
        (not null)
        (satisfies maximum-serialized-size-p)))

(defmapping serialized (if (consp normalized-type)
                           (sql-binary-large-object-type :size (byte-size-of (parse-type normalized-type)))
                           (sql-binary-large-object-type))
  'byte-vector->object-reader
  'object->byte-vector-writer)

;;;;;;
;;; Boolean
;;;
;;; nil -> false
;;; t -> true
;;; other -> (type-error)

(def (persistent-type e) boolean ()
  'boolean)

(defmapping boolean (sql-boolean-type)
  'object->boolean-reader
  'boolean->string-writer)

;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

(def (persistent-type e) integer (&optional minimum-value maximum-value bit-size)
  (declare (ignore bit-size))
  `(integer ,minimum-value ,maximum-value))

(defmapping integer (sql-integer-type :bit-size (when (and (consp normalized-type)
                                                           (> (length normalized-type) 1))
                                                  (flet ((process (bound)
                                                           (if (integerp bound)
                                                               (abs bound)
                                                               0)))
                                                    (bind ((lower-bound (process (second normalized-type)))
                                                           (upper-bound (process (third normalized-type)))
                                                           (bound (max lower-bound upper-bound)))
                                                      (unless (zerop bound)
                                                        ;; KLUDGE we treat sql-integer-type as unsigned... see comment at (def syntax-node sql-integer-type ...)
                                                        (ceiling (log bound 2)))))))
  'object->integer-reader
  'identity-writer)

;;;;;;
;;; Integer-8
;;;
;;; non integer -> (type-error)

(def (persistent-type e) integer-8 ()
  `(integer ,(- (expt 2 7)) ,(1- (expt 2 7))))

(defmapping integer-8 (sql-integer-type :bit-size 8)
  'object->integer-reader
  'identity-writer)

;;;;;;
;;; Integer-16
;;;
;;; non integer -> (type-error)

(def (persistent-type e) integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

(defmapping integer-16 (sql-integer-type :bit-size 16)
  'object->integer-reader
  'identity-writer)

;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(def (persistent-type e) integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(defmapping integer-32 (sql-integer-type :bit-size 32)
  'object->integer-reader
  'identity-writer)

;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(def (persistent-type e) integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(defmapping integer-64 (sql-integer-type :bit-size 64)
  'object->integer-reader
  'identity-writer)

;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

(def (persistent-type e) float (&optional minimum-value maximum-value)
  `(or integer (float ,minimum-value ,maximum-value)))

(defmapping float (sql-float-type :bit-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

;; TODO: minimum-value maximum-value
(def (persistent-type e) float-32 ()
  '(or integer float))

(defmapping float-32 (sql-float-type :bit-size 32)
  'object->number-reader
  'identity-writer)

;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

;; TODO: minimum-value maximum-value
(def (persistent-type e) float-64 ()
  '(or integer float))

(defmapping float-64 (sql-float-type :bit-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

(def (persistent-type e) double ()
  'double-float)

(defmapping double (sql-float-type :bit-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

(def (persistent-type e) number ()
  'number)

(defmapping number (sql-numeric-type)
  'object->number-reader
  'identity-writer)

;;;;;;
;;; String
;;;
;;; non string -> (type-error)

(def (persistent-type e) string (&optional length acceptable-characters)
  (declare (ignore acceptable-characters))
  `(string ,length))

(defmapping string (if (consp normalized-type)
                       (sql-character-type :size (length-of (parse-type normalized-type)))
                       (sql-character-large-object-type))
  'identity-reader
  'identity-writer)

;;;;;;
;;; Text
;;;
;;; non string -> (type-error)

;; TODO:
(def function maximum-length-p (string)
  (declare (ignore string))
  t)

(def (persistent-type e) text (&optional maximum-length minimum-length acceptable-characters)
  (declare (ignore maximum-length minimum-length acceptable-characters))
  '(and string
        (satisfies maximum-length-p)))

(defmapping text (if (consp normalized-type)
                     (sql-character-varying-type :size (maximum-length-of (parse-type normalized-type)))
                     (sql-character-large-object-type))
  ;; TODO do some sanity check for maximum-length when provided
  'identity-reader
  'identity-writer)

;;;;;;
;;; Symbol
;;;
;;; non symbol -> (type-error)

(def (persistent-type e) symbol ()
  'symbol)

(defmapping symbol (sql-character-large-object-type)
  'string->symbol-reader
  'symbol->string-writer)

;; TODO:
(def function maximum-symbol-name-length-p (symbol)
  (declare (ignore symbol))
  t)

(def (persistent-type e) symbol* (&optional maximum-length)
  (declare (ignore maximum-length))
  '(and symbol
        (satisfies maximum-symbol-name-length-p)))

(defmapping symbol* (if (consp normalized-type)
                        (sql-character-varying-type :size (maximum-length-of (parse-type normalized-type)))
                        (sql-character-large-object-type))
  'string->symbol-reader
  'symbol->string-writer)

;;;;;;
;;; Date

;; FIXME this redefines local-time:date
(def (persistent-type e) date ()
  '(and timestamp
        (satisfies local-time::%valid-date?)))

(defmapping date (sql-date-type)
  'identity-reader
  'identity-writer)

;;;;;;
;;; Time

;; FIXME this redefines local-time:time-of-day
(def (persistent-type e) time-of-day ()
  '(and timestamp
        (satisfies local-time::%valid-time-of-day?)))

(defmapping time-of-day (sql-time-type)
  'identity-reader
  'identity-writer)

;;;;;;
;;; Timestamp

(locally #+sbcl(declare (sb-ext:muffle-conditions style-warning))
  (def (persistent-type e) timestamp ()
    'timestamp))

;; TODO should be sql-timestamp-type but then legacy systems would break
(defmapping timestamp (sql-timestamp-with-timezone-type)
  'identity-reader
  'identity-writer)

;;;;;;
;;; Duration
;;;
;;; non string -> (type-error)

(def function duration-p (duration)
  (declare (ignore duration))
  t)

(def (persistent-type e) duration ()
  '(and number
        (satisfies duration-p)))

(defmapping duration (sql-numeric-type)
  'identity-reader
  'identity-writer)

;;;;;;
;;; List
;;;
;;; non form -> (type-error)

(def (persistent-type e) list (&optional byte-size)
  (declare (ignore byte-size))
  'list)

(defmapping list (sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;
;;; Form
;;;
;;; non form -> (type-error)

;; TODO:
(def function form-p (form)
  (declare (ignore form))
  t)

(def (persistent-type e) form (&optional byte-size)
  (declare (ignore byte-size))
  '(and (not unbound)
        (or atom list)
        (satisfies form-p) ;; TODO: this is to avoid subtypep relationship between serialized and form
        (satisfies maximum-serialized-size-p)))

(defmapping form (sql-binary-large-object-type)
  'byte-vector->object-reader
  'object->byte-vector-writer)

;;;;;;
;;; Unsigned byte

(def (persistent-type e) unsigned-byte (&optional size)
  `(unsigned-byte ,size))

;;;;;;
;;; simple-array

(def (persistent-type e) simple-array (&optional (element-type '*) (size '*))
  `(simple-array ,element-type ,size))

;;;;;;
;;; Vector

(def (persistent-type e) vector (&optional (element-type '*) (size '*))
  `(vector ,element-type ,size))

;;;;;;
;;; Unsigned byte vector

(def (persistent-type e) unsigned-byte-vector (&optional maximum-size minimum-size)
  `(vector (unsigned-byte 8) ,@(when (eql maximum-size minimum-size)
                                 (list maximum-size))))

;; TODO assert for the size constraints in the reader/writer
(defmapping unsigned-byte-vector (if (consp normalized-type)
                                     (sql-binary-large-object-type :size (bind ((parsed-type (parse-type normalized-type)))
                                                                           (when (eql (maximum-size-of parsed-type)
                                                                                      (minimum-size-of parsed-type))
                                                                             (maximum-size-of parsed-type))))
                                     (sql-binary-large-object-type))
  'identity-reader
  'identity-writer)

;;;;;;
;;; IP address

(def (persistent-type e) ip-address-vector ()
  '(or (simple-array (unsigned-byte 8) (4))
       (simple-array (unsigned-byte 16) (8))))

(defmapping ip-address-vector (sql-binary-large-object-type :size 16)
  'unsigned-byte-vector->ip-address-vector-reader
  'ip-address-vector->unsigned-byte-vector-writer)
