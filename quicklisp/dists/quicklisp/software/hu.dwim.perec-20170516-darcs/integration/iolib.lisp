;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def (persistent-type e) ip-address ()
  'iolib.sockets:inet-address)

(pushnew 'ip-address *canonical-types*)

;; insert ip-address before ip-address-vector in *mapped-type-precedence-list*
(bind ((cell-index (position 'ip-address-vector *mapped-type-precedence-list*))
       (cell (nthcdr (1- cell-index) *mapped-type-precedence-list*)))
  (setf (cdr cell) (cons 'ip-address (cdr cell))))

(defmapping ip-address (sql-binary-large-object-type :size 16)
  'unsigned-byte-array->ip-address-reader
  'ip-address->unsigned-byte-vector-writer)

(def (function o) unsigned-byte-array->ip-address-reader (rdbms-values index)
  (bind ((address-vector (unsigned-byte-vector->ip-address-vector-reader rdbms-values index)))
    (iolib.sockets:make-address address-vector)))

(def (function o) ip-address->unsigned-byte-vector-writer (slot-value rdbms-values index)
  (check-type slot-value iolib.sockets:inet-address)
  (typecase slot-value
    ((or iolib.sockets:ipv4-address
         iolib.sockets:ipv6-address)
     (ip-address-vector->unsigned-byte-vector-writer (iolib.sockets:address-name slot-value) rdbms-values index))
    (t +type-error-marker+)))
