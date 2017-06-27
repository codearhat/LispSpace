;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.postgresql)

(def (constant e) +default-postgresql-database-server-port+ 5432)

(def class* postgresql-transaction (transaction)
  ((connection
    nil
    :documentation "The Postgresql connection")
   (muffle-warnings (muffle-warnings? *database*) :type boolean :accessor muffle-warnings?)))

(def method transaction-mixin-class list ((db postgresql))
  'postgresql-transaction)

(def method prepare-command ((db postgresql) (tr postgresql-transaction) (command string)
                            &key (name (generate-unique-postgresql-name "prepared_")))
  (cl-postgres:prepare-query (connection-of tr) name command)
  (make-instance 'prepared-statement :name name :query command))

(def special-variable *cl-postgres-sql-readtable* (cl-postgres:copy-sql-readtable))

(local-time:set-local-time-cl-postgres-readers *cl-postgres-sql-readtable*)

(def function execute-prepared-statement (connection statement-name &key binding-types binding-values visitor result-type &allow-other-keys)
  (bind ((cl-postgres:*sql-readtable* *cl-postgres-sql-readtable*))
    (cl-postgres:exec-prepared
     connection statement-name
     ;; TODO could use a vector to send the bindings to cl-postgres
     (iter (for type :in-vector binding-types)
           (for value :in-vector binding-values)
           (collect (cond
                      ;; be careful when reordering stuff here! the order of these nested conds
                      ;; and etypecase'es is important.
                      ((eq value :null)
                       :null)
                      ((typep type 'sql-boolean-type)
                       (if (stringp value)
                           value
                           (if value "TRUE" "FALSE")))
                      ((eq value nil)
                       :null)
                      (t
                       (etypecase type
                         ((or sql-timestamp-type
                              sql-date-type
                              sql-time-type)
                          (if (stringp value)
                              ;; we let the user talk to PostgreSQL directly using strings
                              value
                              (etypecase type
                                (sql-timestamp-type
                                  ;; PostgreSQL for timestamp columns sends back exactly the same value that was given at insert,
                                  ;; so we are safe to send down UTC timestamps if we will also parse them back in UTC.
                                  ;; Random useful and somewhat related info: http://forums.mor.ph/forums/8/topics/189
                                  (local-time:format-rfc3339-timestring nil value :timezone local-time:+utc-zone+))
                                (sql-date-type
                                  (unless (and (zerop (local-time:sec-of value))
                                               (zerop (local-time:nsec-of value)))
                                    (cerror "continue"
                                            "Binding a local-time date value as ~S with non-zero time values; time values will be silently dropped! The bound value in question is: ~A"
                                            'sql-date-type value))
                                  (local-time:format-rfc3339-timestring nil value :omit-time-part #t :timezone local-time:+utc-zone+))
                                (sql-time-type
                                  (unless (zerop (local-time:day-of value))
                                    (cerror "continue"
                                            "Binding a local-time time value as ~S with non-zero day value; day value will be silently dropped! The bound value in question is: ~A"
                                            'sql-time-type value))
                                  (local-time:format-rfc3339-timestring nil value :omit-date-part #t :timezone local-time:+utc-zone+)))))
                         ((or sql-simple-type
                              sql-string-type
                              sql-float-type
                              sql-integer-type
                              sql-binary-large-object-type)
                          value))))))
     (if visitor
         (cl-postgres:row-reader (fields)
           (ecase result-type
             (vector (loop
                        with row = (make-array (length fields))
                        while (cl-postgres:next-row)
                        do (progn
                             (loop
                                for field :across fields
                                for next-field = (cl-postgres:next-field field)
                                for idx :upfrom 0
                                do (setf (aref row idx) next-field))
                             (funcall visitor row))))
             (list (loop
                      while (cl-postgres:next-row)
                      do (let ((row (loop
                                       for field :across fields
                                       collect (cl-postgres:next-field field))))
                           (funcall visitor row))))))
         (ecase result-type
           (list #'cl-postgres:list-row-reader)
           (vector #'cl-postgres:vector-row-reader))))))

(def method execute-command :around ((db postgresql) (tr postgresql-transaction) command &key &allow-other-keys)
  (if (muffle-warnings? tr)
      (handler-bind ((cl-postgres:postgresql-warning #'muffle-warning))
        (call-next-method))
      (call-next-method)))

(def method execute-command ((db postgresql) (tr postgresql-transaction) (command string)
                            &rest args)
  (let ((connection (connection-of tr))
        (statement-name "")) ; unnamed prepared statement
    (cl-postgres:prepare-query connection statement-name command)
    (handler-case
        (apply #'execute-prepared-statement connection statement-name args)
      (cl-postgres-error:lock-not-available (error)
        (unable-to-obtain-lock-error error))
      (cl-postgres-error:deadlock-detected (error)
        (deadlock-detected-error error)))))

(def method execute-command ((db postgresql) (tr postgresql-transaction) (prepared-statement prepared-statement) &rest args)
  (apply #'execute-prepared-statement (connection-of tr) (name-of prepared-statement) args))

(def method connection-of :around ((tr postgresql-transaction))
  (aif (call-next-method)
       it
       (let ((db (database-of tr)))
         (rdbms.debug "Opening Postgresql connection the first time it is needed, using ~S" (remove-from-plist (connection-specification-of db) :password))
         (aprog1
             (loop
               (with-simple-restart (retry "Retry connecting using ~S" (connection-specification-of db))
                 (return
                   (setf (connection-of tr)
                         (apply #'cl-postgres:open-database
                                (bind (((&key (host "localhost") (port +default-postgresql-database-server-port+) database user-name (password ""))
                                        (connection-specification-of db)))
                                  (list database user-name password host port)))))))
           (rdbms.debug "Successfully opened Postgresql connection ~A for transaction ~A in database ~A"
                        it tr db)))))

(def method cleanup-transaction :after ((tr postgresql-transaction))
  (awhen (slot-value tr 'connection)
    (rdbms.debug "Closing Postgresql connection ~A of transaction ~A in database ~A" it tr (database-of tr))
    (cl-postgres:close-database it)
    (setf (connection-of tr) nil)))

(def method backend-release-savepoint (name (db postgresql))
  (execute (format nil "RELEASE SAVEPOINT ~a" name)))
