;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def type oid ()
  'integer)

(def constant +oid-class-id-bit-size+ 16
  "Size of the class id in bits. These are the lower bits in the oid.")

(def constant +oid-maximum-class-id+ (1- (expt 2 +oid-class-id-bit-size+))
  "Maximum class id available.")

(def constant +oid-instance-id-bit-size+ 48
  "Size of the life time unique identifier called instance id in bits. These are the higher bits in the oid. As long as there are not too many instances the oid will be a fixnum.")

(def constant +oid-maximum-instance-id+ (1- (expt 2 +oid-instance-id-bit-size+))
  "Maximum instance id available.")

(def constant +oid-bit-size+ (+ +oid-class-id-bit-size+ +oid-instance-id-bit-size+)
  "Size of the oid in bits.")

(def constant +oid-instance-id-sequence-name+ "_instance_id"
  "The name of the instance id sequence in the relational database used to generate life time unique identifiers for all persistent instances.")

(def constant +oid-column-name+ "_oid"
  "The RDBMS column name for the oid of the instance.")

(def constant +oid-column-names+ (list +oid-column-name+)
  "List of RDBMS column names for the oid.")

(def constant +oid-column-count+ (length +oid-column-names+)
  "The number of oid columns.")

(def special-variable *oid-class-id->class-name-map* (make-hash-table)
  "This map is used to cache class names by class ids. It gets filled when ensure-class is called for the first time and kept up to date.")

;; TODO get rid of this constant and use the normal type mapping machinery for the 'oid type, too?
(def (constant :test (lambda (type-1 type-2) (hu.dwim.rdbms::equal-type-p type-1 type-2 nil))) +oid-sql-type+ (sql-integer-type :bit-size +oid-bit-size+)
  "The RDBMS type for the oid column.")

(def (function io) oid-class-id (oid)
  (logand oid +oid-maximum-class-id+))

(def (function io) oid-instance-id (oid)
  (ash oid (- +oid-class-id-bit-size+)))

(def (function io) oid-class-name (oid)
  (class-id->class-name (oid-class-id oid)))

(def (function o) class-id->class-name (class-id)
  (aprog1
      (gethash class-id *oid-class-id->class-name-map*)
    (assert it nil "Could not find the class name for the class id ~A, probably the class has not yet been exported." class-id)))

(def (function o) (setf class-id->class-name) (class-name class-id)
  (check-type class-name (and symbol (not null)))
  (check-type class-id integer)
  (awhen (gethash class-id *oid-class-id->class-name-map*)
    (assert (eq it class-name) () "Two different class names have the same class id ~A ~A" it class-name))
  (setf (gethash class-id *oid-class-id->class-name-map*) class-name))

(def (function io) class-id-and-instance-id->oid (class-id instance-id)
  (logior (ash instance-id +oid-class-id-bit-size+) class-id))

(def (function o) make-new-oid (class)
  "Creates a fresh and unique oid which was never used before in the relational database."
  (or (oid-instance-id-sequence-exists-p *database*)
      (ensure-instance-id-sequence))
  (bind ((class-id (id-of class))
         (instance-id (next-instance-id)))
    (class-id-and-instance-id->oid class-id instance-id)))

(def (function io) revive-oid (class-id instance-id)
  (class-id-and-instance-id->oid class-id instance-id))

(def (function o) ensure-instance-id-sequence ()
  "Makes sure the instance id sequence exists in the database."
  (unless (sequence-exists-p +oid-instance-id-sequence-name+)
    (create-sequence +oid-instance-id-sequence-name+))
  (setf (oid-instance-id-sequence-exists-p *database*) #t))

(def (function o) next-instance-id ()
  (aprog1 (sequence-next +oid-instance-id-sequence-name+)
    (unless (<= (integer-length it) +oid-instance-id-bit-size+)
      (error "Instance id sequence reached its maximum value ~A" +oid-maximum-instance-id+))))

(def (function io) oid->rdbms-values (oid)
  (aprog1 (make-array +oid-column-count+)
    (oid->rdbms-values* oid it 0)))

(def (function io) oid->rdbms-values* (oid rdbms-values index)
  (setf (elt rdbms-values index) oid))

(def (function io) rdbms-values->oid (rdbms-values)
  (rdbms-values->oid* rdbms-values 0))

(def (function io) rdbms-values->oid* (rdbms-values index)
  (bind ((id (elt rdbms-values index))
         instance-id
         class-id
         class-name)
    (setf instance-id (ldb (byte +oid-instance-id-bit-size+ +oid-class-id-bit-size+) id))
    (setf class-id (ldb (byte +oid-class-id-bit-size+ 0) id))
    (setf class-name (class-id->class-name class-id))
    (debug-only
      (assert (and id instance-id class-name)))
    id))
