;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;; Functions called from compiled queries.

;;;
;;; Lisp implementation of some SQL funtion
;;;
(defun like (string pattern &key (start 0) end (case-sensitive-p #t))
  "Matches STR with PATTERN. In the pattern _ and % wildcards can be used"
  (flet ((like-pattern->regex (pattern)
           (setf pattern (regex-replace-all "([.*+?(){}|^$])" pattern "\\\\\\1"))
           (setf pattern (regex-replace-all "(?<!\\\\)_" pattern "."))
           (setf pattern (regex-replace-all "(?<!\\\\)%" pattern ".*"))))
    (when (and string pattern) ;; FIXME if string or pattern is NIL, the result is NULL in SQL and not FALSE
      (re-like string
               (like-pattern->regex pattern)
               :start start
               :end end
               :case-sensitive-p case-sensitive-p))))

(defun re-like (string pattern &key (start 0) end (case-sensitive-p #t))
  (when (and string pattern) ;; FIXME if string or pattern is NIL, the result is NULL in SQL and not FALSE
    (bind ((end (or end (length string))))
      (generalized-boolean->boolean
       (if case-sensitive-p
           (scan pattern string :start start :end end)
           (scan (create-scanner pattern :case-insensitive-mode #t) string :start start :end end))))))

(def (function e) coalesce (&rest values)
  "Returns the first of its arguments that is not nil. Nil is returned only if all arguments are nil."
  (find nil values :test-not #'eq))

;;;
;;; Markers for partial eval
;;;
(defun volatile (x)
  x)

(defun static (x)
  x)

;;; 
;;; SQL text fragments
;;;
(defun sql-text (string)
  (declare (ignore string))
  (error "SQL text cannot be executed in lisp"))

;;;
;;; Caching
;;;

(def (function o) cache-instance-with-prefetched-slots (row start class prefetched-slots column-counts)
  "Caches the instances whose oid and slots are contained by ROW starting at START."
  (declare (type simple-vector row)
           (type fixnum start))
  (bind ((oid (rdbms-values->oid* row start))
         (instance (cache-instance oid))
         (instance-class (class-of instance)))
    (when *cache-slot-values*
      (iter (for slot :in prefetched-slots)
            (for (the fixnum column-count) :in column-counts)
            (for (the fixnum index) :initially (+ start +oid-column-count+) :then (the fixnum (+ index column-count)))
            (for instance-slot = (if (eq instance-class class)
                                     slot
                                     (find-slot instance-class (slot-definition-name slot))))
            (for value = (restore-slot-value instance instance-slot row index))
            (setf (underlying-slot-boundp-or-value-using-class instance-class instance instance-slot) value)
            ;; TODO: maybe this shoud be a prefetch option?
            (propagate-cache-changes instance-class instance instance-slot value)))
    instance))

(defun column-count-of (slot)
  (length (column-names-of slot)))

(defun invalidate-persistent-flag-of-cached-instances (class)
  "Sets the persistent slot to unbound for instances of class in the transaction cache."
  (map-cached-instances
   (lambda (instance)
     (when (typep instance class)
       (slot-makunbound instance 'persistent)))))

;;;
;;; Bracket
;;;
(defun execute-protected (init-command commands cleanup-command)
  (unwind-protect
       (let (result)
         (when init-command (execute init-command))
         (dolist (command commands (values-list result))
               (setf result (multiple-value-list (execute command)))))
       (when cleanup-command (execute cleanup-command))))

;;;
;;; Writer
;;;

(defstruct (type-info (:conc-name ti-))
  rdbms-types
  writer ;; maps a lisp value to rdbms values
  )

(def function compute-type-info (type)
  (when (and (not (eq type +unknown-type+)) (not (contains-syntax-p type)))
    (bind ((set-type-p (set-type-p* type))
           (element-type
            (if set-type-p
                (bind ((set-element (set-type-class-for type)))
                  (if (typep set-element 'standard-class)
                      (class-name set-element)
                      set-element))
                (canonical-type-for type)))
           (mapping (compute-mapping element-type)))
      (make-type-info
       :rdbms-types (compute-kludged-rdbms-types mapping element-type set-type-p)
       :writer (compute-kludged-writer mapping element-type set-type-p)))))

(def function compute-kludged-rdbms-types (mapping type set-type-p)
  (if set-type-p
      (list nil)
      (bind ((rdbms-types (rdbms-types-of mapping)))
        (ecase (length rdbms-types)
          (1 rdbms-types)
          (2 (cond
               ((persistent-class-type-p type) (list (first rdbms-types))) ; only id column used
               ((tagged-p mapping) (reverse rdbms-types)) ; TAG value is the second
               (t (error "unsupported multi-column type: ~A" type))))))))

(def function compute-kludged-writer (mapping type set-type-p)
  (bind ((rdbms-types (rdbms-types-of mapping))
         (column-count (length rdbms-types))
         (writer (writer-of mapping)))
    (if set-type-p
        (bind ((element-writer (compose #'first (coerce (compute-kludged-writer mapping type #f) 'function))))
          (lambda (value)
            (list (mapcar element-writer value))))
        (lambda (value)
          (bind ((rdbms-values (make-array column-count)))
            (declare (dynamic-extent rdbms-values))
            (case (funcall writer value rdbms-values 0)
              (#.+type-error-marker+ (error "~S is not a valid value for type ~S." value type))
              (t (ecase column-count
                   (1 (list (elt rdbms-values 0)))
                   (2 (cond
                        ((persistent-class-type-p type) (list (elt rdbms-values 0))) ; only id column used
                        ((tagged-p mapping) (list (elt rdbms-values 1) (elt rdbms-values 0))) ; TAG value is the second
                        (t (error "unsupported multi-column type: ~A" type))))))))))))

;;;
;;; Conversion between lisp and sql values
;;;
(defgeneric value->sql-literal (value type type-info &optional args)

  ;; Runtime cast error
  
  (:method (value type type-info &optional args)
    (declare (ignore type-info))
    (error "Can not cast ~A to ~A" value (compose-type type args)))

  ;; Compute type-info
  
  (:method (value (type symbol) (type-info null) &optional args)
    (value->sql-literal value type (compute-type-info (compose-type type args)) args))

  ;; Supported types
  
  (:method (value (type symbol) type-info &optional args)
    (declare (ignore args))
    (assert (not (eql type +unknown-type+)))
    (assert type-info)

    (bind ((rdbms-types (ti-rdbms-types type-info))
           (rdbms-values (funcall (ti-writer type-info) value)))
      (values-list
       (mapcar (lambda (value type)
                 (sql-literal :value value :type type))
               rdbms-values rdbms-types))))

  (:method (value (type persistent-class) type-info &optional args)
    (assert (null args))
    (assert (typep value type))
    (value->sql-literal value (class-name type) type-info args))

  (:method (value (type cons) type-info &optional args)
    (assert (null args))
    (value->sql-literal value (first type) type-info (rest type)))

  ;; Infer type from value
  (:method (value (type (eql +unknown-type+)) type-info &optional args)
    (declare (ignore args))
    (error "Could not infer SQL type for literal: ~S" value))

  (:method ((value persistent-object) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (value->sql-literal value (type-of value) type-info))
 
  (:method ((value string) (type (eql +unknown-type+)) type-info &optional args) ; TODO
    (assert (null args))
    (value->sql-literal value 'string type-info))

  (:method ((value integer) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (if (<= (- #.(expt 2 31)) value #.(1- (expt 2 31)))
        (value->sql-literal value 'integer-32 type-info)
        (value->sql-literal value 'integer type-info)))

  (:method ((value number) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (value->sql-literal value 'number type-info))

  (:method ((value timestamp) (type (eql +unknown-type+)) type-info &optional args)
    (assert (null args))
    (value->sql-literal value 'timestamp type-info))

  ;; Iterate on lists

  (:method ((value list) (type (eql +unknown-type+)) type-info &optional args) ; FIXME hopefully not a form
    (assert (null args))
    (sql-literal :value (mapcar [value->sql-literal !1 type type-info] value))))

(defun compose-type (type args)
  (if args (cons type args) type))


