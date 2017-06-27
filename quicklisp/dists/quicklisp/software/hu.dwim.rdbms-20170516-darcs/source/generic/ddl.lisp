;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

;;;;;;
;;; Create, drop and alter table primitives (they don't signal for schema change)

;; TODO reorganize to have the primitives first, and the signalling schema laters at the end separated
;; TODO drop constraint-to-action ?

(def (function e) create-table (name columns &key temporary)
  (execute-ddl (make-instance 'sql-create-table :temporary temporary :name name :columns columns)))

(def (function e) create-temporary-table (name &rest columns)
  (execute-ddl (make-instance 'sql-create-table :name name :temporary :drop :columns columns)))

(def (function e) drop-table (name &key cascade)
  (execute-ddl (make-instance 'sql-drop-table :name name :cascade cascade)))

(def (function e) alter-table (name &rest actions)
  (execute-ddl (make-instance 'sql-alter-table :name name :actions actions)))

(def (function e) add-column (name column)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-add-column-action
                                                            :name (name-of column)
                                                            :type (type-of column))))))

(def (function e) drop-column (name column-name &optional (cascade #f))
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-drop-column-action :name column-name :cascade cascade)))))

(def (function e) alter-column-type (name column)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-alter-column-type-action
                                                            :name (name-of column)
                                                            :type (type-of column))))))

(def (function e) add-primary-key-constraint (name columns)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-add-primary-key-constraint-action :columns columns)))))


(def (function e) add-foreign-key-constraint (name add-constraint)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list add-constraint))))

;;;;;;
;;; Query tables and columns

(def (function e) list-tables ()
  (database-list-tables *database*))

(def generic database-list-tables (database)
  (:documentation "Returns the list of table names present in the database."))

(def (function e) list-table-columns (name)
  (database-list-table-columns name *database*))

(def generic database-list-table-columns (name database)
  (:documentation "Returns the list of columns present in the database."))

(def (function e) table-exists-p (name)
  (not (null (member (string-downcase name) (list-tables) :test 'equalp))))

;;;;;;
;;; Update table

(def (function e) continue-with-schema-change (&optional condition)
  (invoke-restart (find-restart 'continue-with-schema-change condition)))

(def (with-macro e) with-confirmed-destructive-schema-changes ()
  (handler-bind ((unconfirmed-destructive-schema-change 'continue-with-schema-change))
    (-body-)))

(def (function e) update-table (name columns)
  (tagbody
   :retry
     (if (table-exists-p name)
         (restart-case
             (update-existing-table name columns)
           (drop-table ()
             :report (lambda (stream)
                       (format stream "DESTRUCTIVE: Drop table ~S and try operation ~S again" name 'update-table))
             (drop-table name :cascade #t)
             (go :retry)))
         (progn
           (when *signal-non-destructive-alter-table-commands*
             (with-simple-restart
                 (continue-with-schema-change "Create the table ~S" name)
               (error 'unconfirmed-schema-change/create-table :table-name name)))
           (create-table name columns)))))

(def generic rdbms-type-for (type database)
  (:documentation "Maps the given type to the smallest matching type.")

  (:method (type database)
           type))

(def generic equal-type-p (type-1 type-2 database)
  (:method (type-1 type-2 database)
           #f))

(defun collect-column-foreign-keys (columns)
  (iter outer
	(for column in columns)
	(iter (for constraint in (constraints-of column))
              ;; FIXME a style-warning because SQL-FOREIGN-KEY-CONSTRAINT is defined in something we are a dependency of
	      (when (typep constraint 'sql-foreign-key-constraint)
		(in outer (collect constraint))))))

(def function update-existing-table (table-name columns)
  (let ((table-columns (list-table-columns table-name)))
    ;; create new columns that are missing from the table
    (dolist (column columns)
      (bind ((column-name (name-of column))
             (table-column (find (string-downcase column-name) table-columns :key #'name-of :test #'equalp)))
        (if table-column
            ;; change column type where needed
            (let ((new-rdbms-type (rdbms-type-for (type-of column) *database*)))
              (unless (equal-type-p (type-of table-column) new-rdbms-type *database*)
                (restart-case
                    ;; open a nested RESTART-CASE so that the outer DROP-COLUMN restart is available in case an error comes from the alter table statement
                    (restart-case
                        (error 'unconfirmed-destructive-schema-change/alter-column-type :table-name table-name :column-name column-name
                               :old-type (type-of table-column)
                               :new-type (type-of column)
                               :new-rdbms-type new-rdbms-type)
                      (continue-with-schema-change ()
                        :report (lambda (stream)
                                  (format stream "DESTRUCTIVE: Alter the table ~S by updating the column ~S from the current rdbms type ~A to new rdbms type ~A (derived from ~A)"
                                          table-name column-name (type-of table-column) new-rdbms-type (type-of column)))
                        (with-transaction
                          (alter-column-type table-name column))))
                  (drop-column ()
                    :report (lambda (stream)
                              (format stream "DESTRUCTIVE: Drop column ~S in table ~S (type ~A) and try adding it brand the new with type ~A"
                                      column-name table-name (type-of table-column) (type-of column)))
                    (with-transaction
                      (drop-column table-name column-name #t)
                      (add-column table-name column))))))
            ;; add missing columns not present in the table
            (progn
              (when *signal-non-destructive-alter-table-commands*
                (with-simple-restart
                    (continue-with-schema-change "Alter the table ~S by adding the new column ~S" table-name column-name)
                  (error 'unconfirmed-schema-change/add-column :table-name table-name :column-name column-name :column-type (type-of column))))
              (add-column table-name column)))))
    ;; drop extra columns that are present in the table
    (dolist (table-column table-columns)
      (let ((column-name (name-of table-column)))
        (unless (find column-name columns :key (compose #'string-downcase #'name-of) :test #'equalp)
          (with-simple-restart
              (continue-with-schema-change "DESTRUCTIVE: Drop column ~S in table ~S" column-name table-name)
            (error 'unconfirmed-destructive-schema-change/drop-column :table-name table-name :column-name column-name))
          (drop-column table-name column-name #t)))))
  (update-existing-table-foreign-keys table-name columns))

(defun fkey-actions-same-p (descriptor constraint)
  (and (eq (update-rule-of descriptor) (update-rule-of constraint))
       (eq (delete-rule-of descriptor) (delete-rule-of constraint))))

(defun update-existing-table-foreign-keys (table-name columns)
  (bind ((table-fkeys (list-table-foreign-keys table-name))
	 (add-actions (mapcar (lambda (c)
				(constraint-to-action c table-name))
			      (collect-column-foreign-keys columns))))
    (dolist (existing table-fkeys)
      (unless (find (name-of existing)
		    add-actions
		    :key #'name-of
		    :test #'string=)
	(with-simple-restart (skip "Skip changing this foreign key")
	  (when *signal-non-destructive-alter-table-commands*
	    (with-simple-restart
		(continue-with-schema-change "Alter the table ~S by dropping the foreign key ~S"
					     table-name
					     (name-of existing))
	      (error 'unconfirmed-schema-change/drop-foreign-key
		     :table-name table-name
		     :column-name (source-column-of existing)
		     :constraint-name (name-of existing))))
	  (drop-foreign-key existing))))
    (dolist (existing table-fkeys)
      (let ((match (find (name-of existing)
			 add-actions
			 :key #'name-of
			 :test #'string=)))
	(when (and match (not (fkey-actions-same-p existing match)))
	  (with-simple-restart (skip "Skip changing this foreign key")
	    (when *signal-non-destructive-alter-table-commands*
	      (with-simple-restart
		  (continue-with-schema-change "Alter the table ~S by replacing the foreign key ~S"
					       table-name
					       (name-of existing))
		(error 'unconfirmed-schema-change/replace-foreign-key
		       :table-name table-name
		       :column-name (source-column-of existing)
		       :constraint-name (name-of existing))))
	    (drop-foreign-key existing)
	    (add-foreign-key-constraint table-name match)))))
    (dolist (action add-actions)
      (unless (find (name-of action)
		    table-fkeys
		    :key #'name-of
		    :test #'string=)
	(with-simple-restart (skip "Skip changing this foreign key")
	  (when *signal-non-destructive-alter-table-commands*
	    (with-simple-restart
		(continue-with-schema-change "Alter the table ~S by adding the foreign key ~S"
					     table-name
					     (name-of action))
	      (error 'unconfirmed-schema-change/add-foreign-key
		     :table-name table-name
		     :column-name (car (source-columns-of action))
		     :constraint-name (name-of action))))
	  (add-foreign-key-constraint table-name action))))))

;;;;;;
;;; Create, drop view

(def (function e) create-view (name columns as &key replace)
  (execute-ddl (make-instance 'sql-create-view
                              :name name
                              :columns columns
                              :as as
                              :replace replace)))

(def (function e) drop-view (name &key ignore-missing)
  (execute-ddl (make-instance 'sql-drop-view
                              :name name
                              :ignore-missing ignore-missing)))

(def (function e) view-exists-p (name)
  (not (null (member (string-downcase name) (list-views) :test 'equalp))))

(def (function e) update-view (name column as)
  (drop-view name :ignore-missing #t)
  (create-view name column as))

(def (function e) list-views ()
  (database-list-views *database*))

(def (function e) list-dependent-views (table column)
  (database-list-dependent-views table column *database*))

(def generic database-list-views (database)
  (:documentation "Returns the list of view names present in the database."))

(def generic database-list-dependent-views (table column database)
  (:documentation "Returns the list of view names that depends on the specified column."))

(def (function e) list-view-definitions ()
  (database-list-view-definitions *database*))

(def (function e) view-definition (name)
  (database-view-definition name *database*))

(def (generic e) database-list-view-definitions (database)
  (:documentation "Returns the list of view names and definitions present in the database."))

(def (generic e) database-view-definition (view-name database)
  (:documentation "Returns the view's definition present in the database."))

;;;;;;
;;; Create, drop sequence

(def (function e) create-sequence (name)
  (execute-ddl (make-instance 'sql-create-sequence :name name)))

(def (function e) drop-sequence (name)
  (execute-ddl (make-instance 'sql-drop-sequence :name name)))

(def (function e) sequence-exists-p (name)
  (not (null (member (string-downcase name) (list-sequences) :test 'equalp))))

(def (function e) list-sequences ()
  (database-list-sequences *database*))

(def generic database-list-sequences (database)
  (:documentation "Returns the list of sequence names present in the database."))

(def (function e) sequence-next (name)
  (first-elt (first-elt (execute (make-instance 'sql-select :columns (list (make-instance 'sql-sequence-nextval-column :name name)))))))

(def (function e) sequence-current (name)
  (first-elt (first-elt (execute (make-instance 'sql-select :columns (list (make-instance 'sql-sequence-currval-column :name name)))))))

(def (function e) set-sequence-value (name value &optional (currentp t))
  (first-elt (first-elt (execute (make-instance 'sql-select :columns (list (make-instance 'sql-sequence-setval :name name :value value :currentp currentp)))))))

;;;;;;
;;; Create, drop index

(def (function e) create-index (name table-name columns &key (unique #f))
  (execute-ddl (make-instance 'sql-create-index
                              :name name
                              :table-name table-name
                              :columns columns
                              :unique unique)))

(def (function e) drop-index (name &key ignore-missing)
  (execute-ddl (make-instance 'sql-drop-index :name name :ignore-missing ignore-missing)))

(def (function e) update-index (name table-name columns &key (unique #f))
  ;; TODO: where clause for unique
  (unless (find name (list-table-indices table-name)
                :key 'name-of
                :test (lambda (o1 o2)
                        (equalp (string-downcase o1)
                                (string-downcase o2))))
    (create-index name table-name columns :unique unique)))

(def (function e) list-table-indices (name)
  (database-list-table-indices name *database*))

(def generic database-list-table-indices (name database)
  (:documentation "Returns the list of table indices present in the database."))

(def (function e) list-table-primary-constraints (name)
  (database-list-table-primary-constraints name *database*))

(def generic database-list-table-primary-constraints (name database))

(def class* foreign-key-descriptor ()
  ((name)
   (source-table)
   (source-column)
   (target-table)
   (target-column)
   (update-rule)
   (delete-rule)))

(def print-object foreign-key-descriptor
  (format t "~_CONSTRAINT ~A~_FOREIGN KEY ~A ~A"
	  (name-of -self-)
	  (source-table-of -self-)
	  (list (source-column-of -self-)))
  (format t " ~_REFERENCES ~A ~A"
	  (target-table-of -self-)
	  (list (target-column-of -self-)))
  (format t " ~_ON DELETE ~S ~_ON UPDATE ~S"
	  (delete-rule-of -self-)
	  (update-rule-of -self-)))

(def (function e) list-table-foreign-keys (name)
  (database-list-table-foreign-keys name *database*))

(def generic database-list-table-foreign-keys (name database))

(defun drop-table-foreign-keys (table)
  (mapc #'drop-foreign-key (list-table-foreign-keys table)))
