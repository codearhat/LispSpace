(in-package :hu.dwim.rdbms)

(def syntax-node sql-constraint (named-sql-syntax-node)
  ()
  (:documentation "An SQL constraint."))

(def syntax-node sql-constraint-with-tablespace (sql-constraint)
  ((tablespace
    nil
    :type string)))

(def syntax-node sql-primary-key-constraint (sql-constraint-with-tablespace)
  ()
  (:format-sql-syntax-node
   (format-string " PRIMARY KEY")))

(def syntax-node sql-foreign-key-constraint (sql-constraint-with-tablespace)
  ((target-table :type string)
   (target-column :type string)
   (delete-rule :type sql-foreign-key-action)
   (update-rule :type sql-foreign-key-action))
  (:format-sql-syntax-node
   (format-string " REFERENCES ")
   (format-sql-identifier target-table)
   (format-string " (")
   (format-sql-identifier target-column)
   (format-string  ") ")
   (format-sql-syntax-node (make-instance 'sql-foreign-key-action
					  :event :delete
					  :action delete-rule))
   (format-string  " ")
   (format-sql-syntax-node (make-instance 'sql-foreign-key-action
					  :event :update
					  :action update-rule))))

(def syntax-node sql-foreign-key-action (sql-syntax-node)
  ((action :type sql-foreign-key-action)
   (event :type (member :delete :update)))
  (:format-sql-syntax-node
   (format-string
    (ecase event
      (:delete "ON DELETE ")
      (:update "ON UPDATE ")))
   (format-string (ecase action
		    (:defer-restrict "NO ACTION")
		    (:restrict "RESTRICT")
		    (:set-null "SET NULL")
		    (:set-default "SET DEFAULT")
		    (:cascade "CASCADE")))))

(def syntax-node sql-null-constraint (sql-constraint)
  ()
  (:format-sql-syntax-node
   (format-string " NULL")))

(def syntax-node sql-not-null-constraint (sql-constraint)
  ()
  (:format-sql-syntax-node
   (format-string " NOT NULL")))

(def syntax-node sql-unique-constraint (sql-constraint-with-tablespace)
  ()
  (:format-sql-syntax-node
   (format-string " UNIQUE")))

(def method format-sql-syntax-node :before ((constraint sql-constraint) database)
  (awhen (name-of constraint)
    (format-string " CONSTRAINT")
    (format-char " ")
    (format-sql-syntax-node it database)))
