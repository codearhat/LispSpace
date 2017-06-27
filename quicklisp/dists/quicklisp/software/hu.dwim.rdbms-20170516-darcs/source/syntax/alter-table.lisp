;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def syntax-node sql-alter-table (sql-ddl-statement)
  ((name
    :type string)
   (actions
    :type list))
  (:documentation "An SQL ALTER TABLE statement.")
  (:format-sql-syntax-node
   (format-string "ALTER TABLE ")
   (format-sql-identifier name)
   (format-char " ")
   (format-comma-separated-list actions)))

(def syntax-node sql-add-column-action (sql-column)
  ((default-value
    :type t))
  (:format-sql-syntax-node
   (format-string "ADD (")
   (format-sql-identifier name)
   (format-char " ")
   (format-sql-syntax-node type)
   (format-char ")")))

(def syntax-node sql-drop-column-action (sql-column)
  ((cascade #f :type boolean))
  (:format-sql-syntax-node
   (format-string "DROP COLUMN ")
   (format-sql-identifier name)
   (when cascade
     (format-string " CASCADE"))))

(def syntax-node sql-alter-column-type-action (sql-column)
  ()
  (:format-sql-syntax-node
   (format-string "ALTER COLUMN ")
   (format-sql-identifier name)
   (format-string " TYPE ")
   (format-sql-syntax-node type)))

(def syntax-node sql-add-constraint-action (sql-syntax-node)
  ((name :initform nil
	 :type (or null string)))
  (:format-sql-syntax-node
   (format-string "ADD ")
   (when name
     (format-string "CONSTRAINT ")
     (format-sql-identifier name)
     (format-string " "))))

(def syntax-node sql-add-primary-key-constraint-action (sql-add-constraint-action)
  ((columns
    :type list))
  (:format-sql-syntax-node
   (call-next-method)
   (format-string "PRIMARY KEY (")
   (format-comma-separated-identifiers columns)
   (format-string ")")))

(def syntax-node sql-add-foreign-key-constraint-action (sql-add-constraint-action)
  ((source-columns :type list)
   (target-columns :type list)
   (target-table :type string)
   (delete-rule :type sql-foreign-key-action)
   (update-rule :type sql-foreign-key-action))
  (:format-sql-syntax-node
   (call-next-method)
   (format-string "FOREIGN KEY (")
   (format-comma-separated-identifiers source-columns)
   (format-string ") REFERENCES ")
   (format-sql-identifier target-table)
   (format-string " (")
   (format-comma-separated-identifiers target-columns)
   (format-string  ") ")
   (format-sql-syntax-node (make-instance 'sql-foreign-key-action
					  :event :delete
					  :action delete-rule))
   (format-string  " ")
   (format-sql-syntax-node (make-instance 'sql-foreign-key-action
					  :event :update
					  :action update-rule))
   ;; TODO these should most probably be boolean parameters...
   (format-string  " DEFERRABLE INITIALLY IMMEDIATE")))

(def syntax-node sql-drop-constraint-action (sql-syntax-node)
  ((name :type string))
  (:format-sql-syntax-node
   (format-string "DROP CONSTRAINT ")
   (format-sql-identifier name)))

(defun drop-foreign-key (descriptor)
  (let ((action
	 (sql-drop-constraint-action :name (name-of descriptor))))
    (execute-ddl (make-instance 'sql-alter-table
				:name (source-table-of descriptor)
				:actions (list action)))))

;; There is a certain amount of duplication between the classes in
;; constraint.lisp and those in alter-table.lisp.  Here's a conversion
;; routine:

(def generic constraint-to-action (constraint table-name))

(def method constraint-to-action ((constraint sql-primary-key-constraint) table-name)
  (declare (ignore table-name))
  (make-instance 'sql-add-primary-key-constraint-action
		 :columns (list (name-of constraint))))

(def method constraint-to-action ((constraint sql-foreign-key-constraint) table-name)
  (assert (name-of constraint))
  ;; Note that SQL-FOREIGN-KEY-CONSTRAINT is a named class, and its NAME
  ;; is the column name, but SQL-ADD-FOREIGN-KEY-CONSTRAINT-ACTION has a
  ;; NAME slot which holds the constraint name (!).
  (make-instance 'sql-add-foreign-key-constraint-action
		 :name (rdbms-name-for
			(concatenate 'string
				    "fkey_"
				    table-name
				    "_"
				    (name-of constraint)
				    "_"
				    (target-table-of constraint)
				    "_"
				    (target-column-of constraint)))
		 :source-columns (list (name-of constraint))
		 :target-table (target-table-of constraint)
		 :target-columns (list (target-column-of constraint))
		 :delete-rule (delete-rule-of constraint)
		 :update-rule (update-rule-of constraint)))
