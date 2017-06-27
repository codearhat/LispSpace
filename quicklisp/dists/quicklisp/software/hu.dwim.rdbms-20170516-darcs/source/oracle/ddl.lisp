;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.oracle)

(macrolet ((defcache (fun (&rest args) &body body)
	     `(defmethod ,fun :around (,@args (database oracle))
		(let ((tab (ddl-query-cache-of database)))
		  (if tab
		      ,(if body
			   `(progn ,@body)
			   `(symbol-macrolet
				((cache (gethash ',fun tab :empty)))
			      (if (eq cache :empty)
				  (setf cache (call-next-method))
				  cache)))
		      (call-next-method))))))
  (defcache database-list-sequences ())
  (defcache database-list-tables ())
  (defcache database-list-views ())
  (defcache database-list-triggers ())
  ;;
  (defcache database-list-tables-and-columns ())
  (defcache database-list-table-columns (name)
    (mapcar #'cdr
	    (remove name
		    (database-list-tables-and-columns database)
		    :key #'car
		    :test-not #'equal)))
  ;;
  (defcache database-list-tables-and-indices ())
  (defcache database-list-table-indices (name)
    (mapcar #'cdr
	    (remove name
		    (database-list-tables-and-indices database)
		    :key #'car
		    :test-not #'equal)))
  ;;
  (defcache database-list-tables-and-primary-constraints ())
  (defcache database-list-table-primary-constraints (name)
    (mapcar #'cdr
	    (remove name
		    (database-list-tables-and-primary-constraints database)
		    :key #'car
		    :test-not #'equal)))
  (defcache database-list-tables-and-foreign-keys ())
  (defcache database-list-table-foreign-keys (name)
    (remove name
	    (database-list-tables-and-foreign-keys database)
	    :key #'source-table-of
	    :test-not #'equal))
  (defcache database-list-view-definitions ())
  (defcache database-view-definition (name)
    (second
     (find name
	   (database-list-view-definitions database)
	   :key #'car
	   :test #'string=))))

;; FIXME?  These functions return only object names accessible without
;; qualification by a schema name.  For use by Perec that is fine,
;; because Perec does not support access to more than one schema anyway.
;; But Rdbms as a general-purpose library should be taught about access
;; to multiple schemas/databases.
;;
(def method database-list-sequences ((database oracle))
  (mapcar #'first (execute (format nil "select sequence_name from all_sequences where sequence_owner='~A'"
				   (database-effective-schema database))
			   :result-type 'list)))

(def method database-list-tables ((database oracle))
  (mapcar #'first (execute (format nil "select table_name from all_tables where owner='~A'"
				   (database-effective-schema database))
			   :result-type 'list)))

(def method database-list-views ((database oracle))
  (mapcar #'first (execute (format nil "select view_name from all_views where owner='~A'"
				   (database-effective-schema database))
			   :result-type 'list)))

(def method database-list-view-definitions ((database oracle))
  (execute (format nil "select view_name, dbms_metadata.get_ddl('VIEW',view_name, owner) from all_views where owner='~A'"
		   (database-effective-schema database))
	   :result-type 'list))

(def method database-view-definition (view-name (database oracle))
  (caar (execute (format nil "select dbms_metadata.get_ddl('VIEW',view_name,owner) from all_views where view_name='~A' and owner='~A'"
			 view-name
		   (database-effective-schema database))
	   :result-type 'list)))

(def method database-list-triggers ((database oracle))
  ;; Must be all_triggers, not all_objects, because triggers in a different
  ;; schema don't get shown in all_objects -- in contrast to tables:
  (mapcar #'first (execute (format nil "select trigger_name from all_triggers where owner='~A'"
				   (database-effective-schema database))
			   :result-type 'list)))

(def method database-list-table-columns (name (database oracle))
  (map 'list
   (lambda (column)
     (make-instance 'sql-column
                    :name (aref column 0)
                    :type (sql-type-for-internal-type
                           (aref column 1)
                           (aref column 2)
                           (aref column 3)
                           (aref column 4))))
   (execute
    (format nil "select column_name, data_type, char_length, data_precision, data_scale from all_tab_columns where table_name = '~A' and owner='~A'"
            name
	    (database-effective-schema database))
    :result-type 'vector)))

(def method database-list-tables-and-columns ((database oracle))
  (map 'list
   (lambda (column)
     (cons (aref column 5)
	   (make-instance 'sql-column
			  :name (aref column 0)
			  :type (sql-type-for-internal-type
				 (aref column 1)
				 (aref column 2)
				 (aref column 3)
				 (aref column 4)))))
   (execute
    (format nil "select column_name, data_type, char_length, data_precision, data_scale, table_name from all_tab_columns where owner='~A' and not (table_name like 'DR$%')"
	    (database-effective-schema database))
    :result-type 'vector)))

(def method database-list-tables-and-indices ((database oracle))
  (mapcar
   (lambda (column)
     (cons (third column)
	   (make-instance 'sql-index
			  :name (first column)
			  :table-name (third column)
			  :unique (equal "UNIQUE" (second column)))))
   (execute
    (format nil "select index_name, uniqueness, table_name from all_indexes where owner='~A'"
	    (database-effective-schema database))
    :result-type 'list)))

(def method database-list-table-indices (name (database oracle))
  (mapcar
   (lambda (column)
     (make-instance 'sql-index
                    :name (first column)
                    :table-name name
                    :unique (equal "UNIQUE" (second column))))
   (execute
    (format nil "select index_name, uniqueness from all_indexes where table_name = '~A' and owner='~A'"
            name
	    (database-effective-schema database))
    :result-type 'list)))

(def method database-list-tables-and-primary-constraints ((database oracle))
  (mapcar
   (lambda (column)
     (cons (second column)
	   (make-instance 'sql-constraint :name (first column))))
   (execute
    (format nil "select constraint_name, table_name from all_constraints where constraint_type='P' and owner='~A'"
	    (database-effective-schema database))
    :result-type 'list)))

(def method database-list-table-primary-constraints (name (database oracle))
  (mapcar
   (lambda (column) (make-instance 'sql-constraint :name (first column)))
   (execute
    (format nil "select constraint_name from all_constraints where constraint_type='P' and table_name='~A' and owner='~A'"
            name
	    (database-effective-schema database))
    :result-type 'list)))

(defun sql-rule-name-to-lisp (str)
  (let ((sym (find-symbol (string-upcase str) :keyword)))
    ;; Oracle doesn't have RESTRICT, only NO ACTION, so for compatibility
    ;; with the PostgreSQL backend, let's pretend that NO ACTION is
    ;; the same as RESTRICT rather than  :DEFER-RESTRICT.
    (case sym
      (:cascade :cascade)
      (:set\ null :set-null)
      (:no\ action :restrict)
      (t (error "invalid action: ~A" str)))))

(def method database-list-tables-and-foreign-keys ((database oracle))
  (map 'list
       (lambda (row)
	 (make-instance 'foreign-key-descriptor
			:name (elt row 0)
			:source-table (elt row 1)
			:source-column (elt row 2)
			:target-table (elt row 3)
			:target-column (elt row 4)
			:delete-rule (sql-rule-name-to-lisp (elt row 5))
			;; pretend that update and delete are the same:
			:update-rule (sql-rule-name-to-lisp (elt row 5))))
       (execute
	(format nil "SELECT c3.constraint_name,
                            c1.table_name, c1.column_name,
                            c2.table_name, c2.column_name,
                            c4.delete_rule
                       FROM all_tab_columns c1
                         JOIN all_cons_columns c3
                           ON c1.table_name = c3.table_name
                              AND c3.owner='~A'
                              AND c1.column_name = c3.column_name
                         JOIN all_constraints c4
                           ON c3.constraint_name = c4.constraint_name
                              AND c4.owner='~:*~A'
                         JOIN all_cons_columns c2
                           ON c4.r_constraint_name = c2.constraint_name
                              AND c2.owner='~:*~A'
                              AND c3.position = c2.position
                     WHERE c1.owner='~:*~A'
                           AND c4.constraint_type = 'R'"
		(database-effective-schema database)))))

(def method database-list-table-foreign-keys (table-name (database oracle))
  (map 'list
       (lambda (row)
	 (make-instance 'foreign-key-descriptor
			:name (elt row 0)
			:source-table (elt row 1)
			:source-column (elt row 2)
			:target-table (elt row 3)
			:target-column (elt row 4)
			:delete-rule (sql-rule-name-to-lisp (elt row 5))
			;; pretend that update and delete are the same:
			:update-rule (sql-rule-name-to-lisp (elt row 5))))
       (execute
	(format nil "SELECT c3.constraint_name,
                            c1.table_name, c1.column_name,
                            c2.table_name, c2.column_name,
                            c4.delete_rule
                       FROM all_tab_columns c1
                         JOIN all_cons_columns c3
                           ON c1.table_name = c3.table_name
                              AND c3.owner='~A'
                              AND c1.column_name = c3.column_name
                         JOIN all_constraints c4
                           ON c3.constraint_name = c4.constraint_name
                              AND c4.owner='~:*~A'
                         JOIN all_cons_columns c2
                           ON c4.r_constraint_name = c2.constraint_name
                              AND c2.owner='~:*~A'
                              AND c3.position = c2.position
                     WHERE c1.owner='~:*~A'
                           AND c1.table_name='~A'
                           AND c4.constraint_type = 'R'"
		(database-effective-schema database)
		(string-downcase table-name)))))
