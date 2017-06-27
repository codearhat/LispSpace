;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Constants

(def (special-variable :documentation "True means slot-value-using-class will by default return lazy collections.")
  *lazy-slot-value-collections* #f)

;;;;;;
;;; Utility

(def (function io) object-reader (rdbms-values index)
  (load-instance (rdbms-values->oid* rdbms-values index) :skip-existence-check #t))

(def (function io) object-writer (slot-value rdbms-values index)
  (oid->rdbms-values* (oid-of slot-value) rdbms-values index))

(def (function io) make-oid-matcher-where-clause (instance-or-oid &optional (oid-name +oid-column-name+))
  (sql-binary-operator :name '=
                       :left (sql-identifier :name oid-name)
                       :right (sql-literal :type +oid-sql-type+ :value (if (integerp instance-or-oid)
                                                                           instance-or-oid
                                                                           (oid-of instance-or-oid)))))

(def (function io) make-oid-list-matcher-where-clause (values &optional (oid-name +oid-column-name+))
  (sql-binary-operator :name 'in
                       :left (sql-identifier :name oid-name)
                       :right (mapcar (lambda (value)
                                        (sql-literal :type +oid-sql-type+ :value (oid-of value)))
                                      values)))

;;;;;;
;;; RDBMS slot restorers

(def (function io) restore-slot-value (instance slot rdbms-values index)
  "Provides convenient access to the arguments in the debugger."
  (prog1-bind slot-value (funcall (the function (reader-of slot)) rdbms-values index)
    (when (eq +type-error-marker+ slot-value)
      (error 'slot-type-error :instance instance :slot slot :expected-type (specified-type-of slot) :datum (subseq rdbms-values index (+ index (length (columns-of slot))))))))

(def (function o) restore-slot-set (instance slot)
  "Loads a non-lazy list from the database without doing any side effects in the Lisp VM."
  (bind ((other-association-end (other-association-end-of slot))
         (other-association-end-view (association-end-view-of other-association-end)))
    (when other-association-end-view
      (map 'list [object-reader !1 0]
           (select-records +oid-column-names+
                           (list (name-of other-association-end-view))
                           :where (make-oid-matcher-where-clause instance (oid-column-of slot))
                           :order-by (bind ((type (canonical-type-of slot)))
                                       (if (ordered-set-type-p type)
                                           ;; TODO: use reflection instead of third
                                           (list (sql-identifier :name (rdbms-name-for (third type)))))))))))

(def (function o) restore-1-n-association-end-set (instance slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (restore-slot-set instance slot))

(def (function o) restore-m-n-association-end-set (instance slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (bind ((other-slot (other-association-end-of slot)))
    (map 'list [object-reader !1 0]
         (select-records (columns-of slot)
                         (list (name-of (table-of slot)))
                         :where (make-oid-matcher-where-clause instance (oid-column-of other-slot))))))

(def generic restore-slot (class instance persistent-effective-slot-definition &key &allow-other-keys)
  (:documentation "Restores a single slot without local side effects from the database.")

  (:method ((class persistent-class) (instance persistent-object) (slot persistent-effective-slot-definition) &key)
    (values
     ;; TODO this set-type-p* calls subtypep, which is expensive. search this file for other occurrances, too.
     (if (set-type-p* (canonical-type-of slot))
         (if *lazy-slot-value-collections*
             (make-instance 'persistent-slot-set-container :instance instance :slot slot)
             (restore-slot-set instance slot))
         (bind ((record
                 (first-elt
                  (select-records (columns-of slot)
                                  (list (name-of (table-of slot)))
                                  :where (make-oid-matcher-where-clause instance)))))
           (restore-slot-value instance slot record 0)))
     slot))

  (:method ((class persistent-class) (instance persistent-object) (slot persistent-association-end-effective-slot-definition) &key)
    (values
     (ecase (association-kind-of (association-of slot))
       (:1-1
        (if (secondary-association-end-p slot)
            (bind ((records
                    (select-records +oid-column-names+
                                    (list (name-of (association-end-view-of (other-association-end-of slot))))
                                    :where (sql-= (sql-literal :type +oid-sql-type+ :value (oid-of instance))
                                                  (sql-identifier :name (oid-column-of slot))))))
              (declare (type vector records))
              (unless (zerop (length records))
                (restore-slot-value instance slot (first-elt records) 0)))
            (call-next-method)))
       (:1-n
        (if (eq (cardinality-kind-of slot) :n)
            (if *lazy-slot-value-collections*
                (make-instance 'persistent-1-n-association-end-set-container :instance instance :slot slot)
                (restore-1-n-association-end-set instance slot))
            (call-next-method)))
       (:m-n
        (if *lazy-slot-value-collections*
            (make-instance 'persistent-m-n-association-end-set-container :instance instance :slot slot)
            (restore-m-n-association-end-set instance slot))))
     slot)))

(def generic restore-prefetched-slots (class instance &optional allow-missing)
  (:documentation "Restores all prefetched slots at once without local side effects from the database. Executes a single select statement.")

  (:method ((class persistent-class) (instance persistent-object) &optional (allow-missing #f))
    (when-bind slots (prefetched-slots-of (class-of instance))
      (bind ((records
              (select-records (mappend #'columns-of slots)
                              (list (name-of (direct-instances-data-view-of class)))
                              :where (make-oid-matcher-where-clause instance)))
             (record (unless (and allow-missing
                                  (zerop (length records)))
                       (assert (= 1 (length records)) nil "The persistent instance ~A is missing from the database" instance)
                       (first-elt records))))
        (declare (type vector records))
        (declare (type (or null vector) record))
        (when record
          (values
           (iter (for (the fixnum i) :first 0 :then (the fixnum (+ i (length (columns-of slot)))))
                 (for slot :in slots)
                 (collect (restore-slot-value instance slot record i)))
           slots))))))

(def (function o) restore-all-slots (instance)
  "Restores all slots wihtout local side effects from the database."
  (bind ((class (class-of instance))
         ((:values prefetched-slot-values prefetched-slots) (restore-prefetched-slots class instance))
         (non-prefetched-slots (non-prefetched-slots-of class)))
    (values (append prefetched-slot-values (mapcar [restore-slot class instance !1] non-prefetched-slots))
            (append prefetched-slots non-prefetched-slots))))

;;;;;;
;;; RDBMS slot storers

(def (function io) store-slot-value (instance slot slot-value rdbms-values index)
  "Provides convenient access to the arguments in the debugger."
  (prog1-bind primary-rdbms-value (funcall (the function (writer-of slot)) slot-value rdbms-values index)
    (when (eq +type-error-marker+ primary-rdbms-value)
      (error 'slot-type-error :instance instance :slot slot :expected-type (specified-type-of slot) :datum slot-value))))

(def (function o) delete-slot-set (instance slot)
  (dolist (table (association-end-tables-of slot))
    (update-records (name-of table)
                    (columns-of slot)
                    (make-array +oid-column-count+ :initial-element :null)
                    (make-oid-matcher-where-clause instance (oid-column-of slot)))))

(def (function o) store-slot-set (instance slot values)
  "Stores the non lazy list without local side effects into the database."
  (delete-slot-set instance slot)
  (when values
    #+nil
    (dolist (value values)
      ;; TODO: this is incorrect, add test?
      (check-slot-value-type instance slot value))
    (bind ((rdbms-values (make-array +oid-column-count+)))
      (object-writer instance rdbms-values 0)
      (dolist (table (association-end-tables-of slot))
        (update-records (name-of table)
                        (columns-of slot)
                        rdbms-values
                        (make-oid-list-matcher-where-clause values))))))


(def (function o) store-1-n-association-end-set (instance slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (store-slot-set instance slot value))

(def (function o) delete-m-n-association-end-set (instance slot)
  (delete-records (name-of (table-of slot))
                  (make-oid-matcher-where-clause instance (oid-column-of (other-association-end-of slot)))))

(def (function o) insert-into-m-n-association-end-set (instance slot value)
  (unless (check-slot-value-type instance slot value)
    (bind ((other-slot (other-association-end-of slot))
           (rdbms-values (make-array (* 2 +oid-column-count+))))
      (object-writer value rdbms-values 0)
      (object-writer instance rdbms-values +oid-column-count+)
      (insert-record (name-of (table-of slot))
                     (append (columns-of slot) (columns-of other-slot))
                     rdbms-values))))

(def (function o) store-m-n-association-end-set (instance slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (delete-m-n-association-end-set instance slot)
  (when value
    (mapc [progn
            ;; TODO: this is incorrect, add test?
            (unless (check-slot-value-type instance slot !1)
              (insert-into-m-n-association-end-set instance slot !1))]
          value)))

(def generic store-slot (class instance slot value)
  (:documentation "Stores a single slot without local side effects into the database.")

  (:method ((class persistent-class) (instance persistent-object) (slot persistent-effective-slot-definition) value)
    ;; TODO this set-type-p* calls subtypep, which is expensive. search this file for other occurrances, too.
    (if (set-type-p* (canonical-type-of slot))
        (store-slot-set instance slot value)
        (unless (check-slot-value-type instance slot value)
          (when-bind columns (columns-of slot)
            (bind ((rdbms-values (make-array (length (the list columns)))))
              (store-slot-value instance slot value rdbms-values 0)
              (bind ((count (update-records (name-of (table-of slot))
                                            columns
                                            rdbms-values
                                            (make-oid-matcher-where-clause instance))))
                (assert (eql 1 count))))))))

  (:method ((class persistent-class) (instance persistent-object) (slot persistent-association-end-effective-slot-definition) value)
    (ecase (association-kind-of (association-of slot))
      (:1-1
       (if (secondary-association-end-p slot)
           (unless (check-slot-value-type instance slot value)
             (when-bind other-instance (and (persistent-p instance)
                                            (slot-boundp-using-class (class-of instance) instance slot)
                                            (slot-value-using-class (class-of instance) instance slot))
               (bind ((other-class (class-of other-instance))
                      (other-slot (other-effective-association-end-for other-class slot)))
                 (store-slot other-class other-instance other-slot nil)))
             (when (and value
                        (not (unbound-slot-marker-p value)))
               (bind ((value-class (class-of value))
                      (other-slot (other-effective-association-end-for value-class slot)))
                 (store-slot value-class value other-slot instance))))
           (progn
             (when value
               (update-records (name-of (table-of slot))
                               (columns-of slot)
                               (make-array +oid-column-count+ :initial-element :null)
                               (make-oid-matcher-where-clause value (oid-column-of slot))))
             (call-next-method))))
      (:1-n
       (if (eq (cardinality-kind-of slot) :n)
           (when (or value
                     (persistent-p instance))
             (store-1-n-association-end-set instance slot value))
           (call-next-method)))
      (:m-n
       (when (or value
                 (persistent-p instance))
         (store-m-n-association-end-set instance slot value))))))

(def generic store-prefetched-slots (class instance)
  (:documentation "Stores all prefetched slots without local side effects into the database. Executes one insert statement for each table.")

  (:method ((class persistent-class) (instance persistent-object))
    (bind ((prefetched-slots (prefetched-slots-of (class-of instance)))
           (tables (delete-duplicates (mapcar #'table-of prefetched-slots)))
           (oid (oid-of instance))
           (error? #f))
      (dolist (table tables)
        (bind ((slots (collect-if [eq (table-of !1) table] prefetched-slots))
               (slot-values (mapcar [underlying-slot-boundp-or-value-using-class (class-of instance) instance !1] slots))
               (oid-columns (list (oid-column-of table)))
               (columns (mappend #'columns-of slots))
               (oid-values (oid->rdbms-values oid))
               (rdbms-values (make-array (length columns))))
          (declare (type list slots slot-values columns))
          (iter (for slot :in slots)
                (for slot-value :in slot-values)
                (for index :initially 0 :then (the fixnum (+ index (length (columns-of slot)))))
                (setf error? (or (check-slot-value-type instance slot slot-value) error?))
                (unless error?
                  (store-slot-value instance slot slot-value rdbms-values index)))
          (unless error?
            (if (persistent-p instance)
                (update-records (name-of table) columns rdbms-values (make-oid-matcher-where-clause instance))
                (insert-record (name-of table) (append oid-columns columns) (concatenate 'vector oid-values rdbms-values))))))
      (unless (persistent-p instance)
        (dolist (table (set-difference (data-tables-of (class-of instance)) tables))
          (unless error?
            (insert-record (name-of table) (list (oid-column-of table)) (oid->rdbms-values oid))))))))

(def (function o) store-all-slots (instance)
  "Stores all slots wihtout local side effects into the database."
  (bind ((class (class-of instance)))
    (store-prefetched-slots class instance)
    (mapc [when (slot-value-cached-p instance !1)
            (store-slot class instance !1 (underlying-slot-boundp-or-value-using-class class instance !1))]
          (non-prefetched-slots-of class))))
