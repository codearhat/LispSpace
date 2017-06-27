;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def class* prepared-statement ()
  ((name)
   (query :documentation "The query passed in when this statement was prepared.")))

(def (class* e) transaction ()
  ((database
    :type database)
   (default-result-type
    :type (member vector list cursor))
   (timestamp
    nil
    :type integer)
   (command-counter
    (make-instance 'command-counter)
    :type command-counter)
   (begin-was-executed
    #f
    :type boolean)
   (state
    :uninitialized
    :type (member :uninitialized :committed :rolled-back :in-progress))
   (terminal-action
    :commit
    :type (member :commit :rollback :marked-for-commit-only :marked-for-rollback-only)
    :documentation "Used by with-transaction to decide what to do when the with-transaction body finishes without any errors.")
   (break-on-next-command
    #f
    :type boolean))
  (:documentation "An object representing a transaction context. The actual backend connection/transaction is usually lazily created."))

(def print-object transaction
  (princ ":begin-executed-p ")
  (princ (if (begin-was-executed-p -self-)
             "#t" "#f")))

(def (class* a) command-counter ()
  ((insert-counter 0 :type integer)
   (select-counter 0 :type integer)
   (update-counter 0 :type integer)
   (delete-counter 0 :type integer)))

(def print-object command-counter
  (write-string "insert: ")
  (write (insert-counter-of -self-))
  (write-string ", select: ")
  (write (select-counter-of -self-))
  (write-string ", update: ")
  (write (update-counter-of -self-))
  (write-string ", delete: ")
  (write (delete-counter-of -self-)))

(def (function e) current-insert-counter ()
  (insert-counter-of (command-counter-of *transaction*)))

(def (function e) current-select-counter ()
  (select-counter-of (command-counter-of *transaction*)))

(def (function e) current-update-counter ()
  (update-counter-of (command-counter-of *transaction*)))

(def (function e) current-delete-counter ()
  (delete-counter-of (command-counter-of *transaction*)))

(def (function e) report-transaction-state ()
  (cerror "Confirm transaction state and go on" "Reporting ~A with ~A terminal action~%statistics: ~A" *transaction* (terminal-action-of *transaction*) (command-counter-of *transaction*))
  (values))

(def (function e) break-on-next-command ()
  (setf (break-on-next-command-p *transaction*) #t))

(def (macro e) assert-single-select-statement (&body forms)
  (with-unique-names (command-counter counter)
    `(let* ((,command-counter (command-counter-of *transaction*))
            (,counter (select-counter-of ,command-counter)))
       (multiple-value-prog1
           (progn
             ,@forms)
         (assert (= (1+ ,counter) (select-counter-of ,command-counter)))))))

(def (macro e) with-readonly-transaction (&body forms)
  "Evaluates FORMS within the dynamic scope of a new TRANSACTION that will unconditionally ROLLBACK."
  `(with-transaction* (:default-terminal-action :rollback)
     (mark-transaction-for-rollback-only)
     ,@forms))

(def (macro e) with-transaction (&body forms)
  "Evaluates FORMS within the dynamic scope of a new TRANSACTION."
  `(with-transaction* ()
    ,@forms))

(def (generic e) call-in-transaction (database transaction function)
  (:documentation "Extension point for with-transaction macro.")

  (:method (database transaction function)
    (declare (ignore database transaction))
    (funcall function)))

(def (function e) abandon-and-recreate-transaction ()
  (declare (special *transaction-creator*))
  (hu.dwim.rdbms::assert-transaction-in-progress)
  (setq hu.dwim.rdbms::*transaction*
        (funcall hu.dwim.rdbms::*transaction-creator*))
  (values))

(def (function e) commit-and-recreate-transaction ()
  (declare (special *transaction-creator*))
  (commit)
  (setq hu.dwim.rdbms::*transaction*
        (funcall hu.dwim.rdbms::*transaction-creator*))
  (values))

(def (with-macro* e) with-transaction*
  (&rest args &key (default-terminal-action :commit) database take-over
         &allow-other-keys)
  "Evaluates FORMS within the dynamic scope of a new TRANSACTION."
  (unless (or database (boundp '*database*))
    (error "Cannot start transaction because database was not provided, either use WITH-DATABASE or provide a database to WITH-TRANSACTION*"))
  (bind ((*database* (or database *database*))
         (*transaction*)
         (body-finished?)
         (*transaction-creator*
          (let ((db *database*)
                (ta default-terminal-action)
                (ar (remove-from-plist args
                                       :database
                                       :default-terminal-action
                                       :take-over)))
            (lambda ()
              (apply 'make-transaction db :terminal-action ta ar)))))
    (declare (special *transaction-creator*))
    (iter restart-transaction-loop
       (with-simple-restart (restart-transaction "rollback the transaction by unwinding the stack and restart the WITH-TRANSACTION block in a new database transaction")
         (unwind-protect
              (progn
                (setf body-finished? #f)
                (setq *transaction* (or take-over (funcall *transaction-creator*)))
                (return-from restart-transaction-loop
                  (multiple-value-prog1
                      (restart-case
                          (call-in-transaction *database* *transaction* #'-body-)
                        (terminate-transaction ()
                          :report (lambda (stream)
                                    (format stream "return (values) from the WITH-TRANSACTION block executing the current terminal action ~S" (terminal-action-of *transaction*)))
                          (values))
                        (commit-transaction ()
                          :report (lambda (stream)
                                    (format stream "mark transaction for commit only and return (values) from the WITH-TRANSACTION block"))
                          (mark-transaction-for-commit-only)
                          (values))
                        (rollback-transaction ()
                          :report (lambda (stream)
                                    (format stream "mark transaction for rollback only and return (values) from the WITH-TRANSACTION block"))
                          (mark-transaction-for-rollback-only)
                          (values)))
                    (setf body-finished? #t)
                    ;; TODO user code may run after this point, which may try to call MARK-TRANSACTION-FOR-ROLLBACK-ONLY and stuff like that, which should fail because it's too late for that...
                    ;; current example: perec::check-slot-value-type
                    (ecase (terminal-action-of *transaction*)
                      ((:commit :marked-for-commit-only)
                       (commit-transaction *database* *transaction*))
                      ((:rollback :marked-for-rollback-only)
                       (rollback-transaction *database* *transaction*))))))
           (when *transaction*
             (unwind-protect
                  (unless body-finished?
                    (block try-to-rollback
                      (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                                      (rdbms.error (build-error-log-message :error-condition error :message "Error while trying to rollback transaction in a failed WITH-TRANSACTION block")))
                                                    (lambda (&key &allow-other-keys)
                                                      (return-from try-to-rollback)))
                        (rollback-transaction *database* *transaction*))))
               (cleanup-transaction *transaction*))))))))

(def method (setf terminal-action-of) :before (new-value (transaction transaction))
  (when (and (member (terminal-action-of *transaction*) '(:marked-for-rollback-only :marked-for-commit-only))
             (not (eq new-value (terminal-action-of *transaction*))))
    (error "You can not set the terminal action of the transaction ~A from ~A to ~A"
           *transaction* (terminal-action-of *transaction*) new-value)))

(def function mark-transaction-for-commit-only ()
  (assert-transaction-in-progress)
  (setf (terminal-action-of *transaction*) :marked-for-commit-only))

(def function mark-transaction-for-rollback-only ()
  ;; TODO add assert that this marking will have an effect (not yet in the commit phase...)
  (assert-transaction-in-progress)
  (setf (terminal-action-of *transaction*) :marked-for-rollback-only))

(def function transaction-timestamp ()
  (or (timestamp-of *transaction*)
      (setf (timestamp-of *transaction*) (first-elt (first-elt (execute "select now()"))))))

;; TODO rename to... err... something else
(def (function io) in-transaction-p ()
  (and (boundp '*transaction*)
       *transaction*
       (transaction-in-progress-p *transaction*)))

(def function transaction-in-progress-p (&optional (transaction *transaction*))
  (eq (state-of transaction) :in-progress))

(def function transaction-valid-p (&optional (transaction *transaction*))
  "Returns true if we have a running transaction and its terminal action will be a commit."
  (and (transaction-in-progress-p transaction)
       (not (eq (terminal-action-of transaction) :marked-for-rollback-only))))

(def (function io) assert-transaction-in-progress ()
  (unless (in-transaction-p)
    (error 'transaction-error :format-control "No transaction in progress and implicit transactions are turned off.~%Please either use WITH-TRANSACTION, or BEGIN/COMMIT/ROLLBACK, or set *IMPLICIT-TRANSACTION* and *IMPLICIT-TRANSACTION-DEFAULT-TERMINAL-ACTION* accodring to your needs.")))

(def (function ioe) begin (&rest args)
  "Starts a new transaction. This transaction must be closed by an explicit call to either ROLLBACK or COMMIT. See with-transaction for convenience and safety. This is for debug purposes."
  (assert (not (boundp '*transaction*)))
  (setf *transaction* (apply #'make-transaction *database* args)))

(def (function ioe) commit ()
  "Commits the current transaction. The transaction must be started by an explicit call to BEGIN. This is for debug purposes."
  (assert-transaction-in-progress)
  (unwind-protect
       (commit-transaction *database* *transaction*)
    (cleanup-transaction *transaction*)
    (makunbound '*transaction*))
  (values))

(def (function ioe) rollback ()
  "Rolls back the current transaction. The transaction must be started by an explicit call to BEGIN. This is for debug purposes."
  (assert-transaction-in-progress)
  (unwind-protect
       (rollback-transaction *database* *transaction*)
    (cleanup-transaction *transaction*)
    (makunbound '*transaction*))
  (values))

;; TODO rename :with-transaction to :ensure-transaction
(def (function ioe) execute (command &rest args &key visitor bindings result-type (with-transaction *implicit-transaction*)
                                     (implicit-transaction-terminal-action *implicit-transaction-default-terminal-action*) &allow-other-keys)
  "Executes an SQL command. If VISITOR is not present the result is returned in a sequence. The type of the sequence is determined by RESULT-TYPE which is either LIST or VECTOR. When VISITOR is present it will be called for each row in the result."
  (declare (ignore visitor bindings result-type))   ; for slime to bring up the arguments
  (check-type with-transaction (member nil :new :ensure))
  (flet ((%execute-command ()
           (apply 'execute-command *database* *transaction* command args)))
    (if (or (eq :new with-transaction)
            (and (not (in-transaction-p))
                 (eq :ensure with-transaction)))
        (with-transaction* (:default-terminal-action implicit-transaction-terminal-action)
          (%execute-command))
        (progn
          (assert-transaction-in-progress)
          (%execute-command)))))

(def (function e) execute-ddl (command &rest args &key implicit-transaction-terminal-action &allow-other-keys)
  "A DDL statement is executed in a separate transaction if there is no active transaction."
  (apply #'execute command
         :with-transaction :ensure
         :implicit-transaction-terminal-action (or implicit-transaction-terminal-action :commit)
         args))

(def method transaction-mixin-class list (database)
  'transaction)

(def (generic e) make-transaction (database &key &allow-other-keys)
  (:documentation "Extension point for with-transaction.")

  (:method :before (database &key &allow-other-keys)
    (rdbms.debug "About to BEGIN transaction in database ~A" database))

  (:method (database &rest args &key default-result-type &allow-other-keys)
    (apply #'make-instance (transaction-class-of database)
           :database database
           :state :in-progress
           :default-result-type (or default-result-type (default-result-type-of database))
           args)))

(def (generic e) begin-transaction (database transaction)
  (:documentation "Extension point for with-transaction and begin.")

  (:method (database transaction)
    (execute-command database transaction "BEGIN")))

(def (generic e) commit-transaction (database transaction)
  (:documentation "Extension point for with-transaction and commit.")

  (:method :around (database transaction)
    (rdbms.debug "About to COMMIT transaction ~A" transaction)
    (when (begin-was-executed-p transaction)
      (call-next-method))
    (setf (state-of transaction) :committed)
    (values))

  (:method (database transaction)
    (execute-command database transaction "COMMIT")))

(def (generic e) rollback-transaction (database transaction)
  (:documentation "Extension point for with-transaction and rollback.")

  (:method :around (database transaction)
    (rdbms.debug "About to ROLLBACK transaction ~A" transaction)
    (when (begin-was-executed-p transaction)
      (call-next-method))
    (setf (state-of transaction) :rolled-back)
    (values))

  (:method (database transaction)
    (execute-command database transaction "ROLLBACK")))

(def (generic e) cleanup-transaction (transaction)
  (:documentation "Extension point with-transaction and commit/rollback.")
  (:method ((transaction t))
    ;; nop
    ))

(def generic prepare-command (database transaction command &key name)
  (:documentation "Sends a query to the database for parsing and returns a handler (a prepared-statement CLOS object) that can be used as a command for EXECUTE-COMMAND."))

(def function update-binding-values (binding-variables binding-types binding-values name-value-bindings)
  (iter (for variable :in-vector binding-variables)
        (for type :in-vector binding-types)
        (for index :from 0)
        (assert type nil "The type of literals and binding variables must be defined because they are transmitted through the binding infrastructure")
        (when variable
          (bind ((name (name-of variable))
                 (value (getf name-value-bindings name 'not-specified)))
            (assert name nil "Binding variables must have a name")
            (when (eq value 'not-specified)
              (error 'unbound-binding-variable-error :variable-name name))
            (setf (aref binding-values index) value)))))

(def generic notify-transaction-event (transaction event)
  (:method (transaction event)
    (values)))

(def function %generate-log-description-for-command (command binding-types binding-values)
  (format nil "~@<~@;~{~A~^, ~}~%~A~@:>"
          (iter (for i :upfrom 1)
                (for type :in-vector binding-types)
                (for value :in-vector binding-values)
                (collect (format nil "$~A = ~A as ~A" i value (format-sql-to-string type))))
          command))

(def generic execute-command (database transaction command &key visitor bindings result-type &allow-other-keys)
  (:method (database transaction command &key &allow-other-keys)
    (error "Default method should not be reached"))

  (:method :before (database transaction command &key binding-types binding-values &allow-other-keys)
    (declare (ignorable binding-values))
    (unless (begin-was-executed-p transaction)
      (setf (begin-was-executed-p transaction) #t)
      (begin-transaction database transaction))
    (when (stringp command)
      (bind ((*print-length* 128)
             (*print-level* 3)
             (*print-pretty* #f)
             (*print-circle* #f))
        (if (zerop (length binding-types))
            (sql.dribble "~A" command)
            (sql.dribble (%generate-log-description-for-command command binding-types binding-values))))))

  (:method :around (database transaction command &rest args &key (result-type (default-result-type-of transaction)) &allow-other-keys)
    (when (break-on-next-command-p *transaction*)
      (setf (break-on-next-command-p *transaction*) #f)
      (cerror "Continue transaction" "Break requested on rdbms command ~S" command))
    (apply #'call-next-method database transaction command :result-type result-type args))

  (:method :around (database transaction (command string) &key binding-types binding-values &allow-other-keys)
    (with-error-log-decorator (make-error-log-decorator
                                (format t "~%Current SQL command:~%~S" (%generate-log-description-for-command command binding-types binding-values)))
      (call-next-method)))

  (:method :after (database transaction (command string) &key &allow-other-keys)
    (let ((command-counter (command-counter-of transaction)))
      (cond ((starts-with-subseq "INSERT" command :test #'equalp)
             (incf (insert-counter-of command-counter))
             (notify-transaction-event transaction :insert))
            ((starts-with-subseq "SELECT" command :test #'equalp)
             (incf (select-counter-of command-counter))
             (notify-transaction-event transaction :select))
            ((starts-with-subseq "UPDATE" command :test #'equalp)
             (incf (update-counter-of command-counter))
             (notify-transaction-event transaction :update))
            ((starts-with-subseq "DELETE" command :test #'equalp)
             (incf (delete-counter-of command-counter))
             (notify-transaction-event transaction :delete))))))

(def class* transaction-with-hooks-mixin ()
  ((hooks nil :type list)))

(def cl:type transaction-hook-invocation-time ()
  `(member :before :after))

(def cl:type transaction-hook-action ()
  `(member :commit :rollback :always))

(def class* transaction-hook ()
  ((function :type (or symbol function) :accessor function-of)
   (when :type transaction-hook-invocation-time :accessor when-of)
   (action :type transaction-hook-action)))

(def constructor (transaction-hook when action)
  (check-type when transaction-hook-invocation-time)
  (check-type action transaction-hook-action))

(def function call-transaction-hooks (transaction when action)
  (loop for hook :in (hooks-of transaction) do
        (let ((hook-action (action-of hook)))
          (when (and (eq when (when-of hook))
                     (or (eq :always hook-action)
                         (eq action hook-action)))
            (funcall (function-of hook))))))

(def method commit-transaction :around (database (transaction transaction-with-hooks-mixin))
  ;; this must be an around method because the default around does not
  ;; do call-next-method when begin-transaction was not executed
  (progn
    (call-transaction-hooks transaction :before :commit)
    (multiple-value-prog1
        (call-next-method)
      (call-transaction-hooks transaction :after :commit))))

(def method rollback-transaction :around (database (transaction transaction-with-hooks-mixin))
  ;; this must be an around method because the default around does not
  ;; do call-next-method when begin-transaction was not executed
  (progn
    (call-transaction-hooks transaction :before :rollback)
    (multiple-value-prog1
        (call-next-method)
      (call-transaction-hooks transaction :after :rollback))))

(def macro register-transaction-hook (when action &body forms)
  `(register-hook-in-transaction *transaction* ,when ,action
                                 (lambda () ,@forms)))

(def (generic e) register-hook-in-transaction (transaction when action function)
  (:method ((transaction transaction-with-hooks-mixin) when action (function function))
    (check-type when transaction-hook-invocation-time)
    (check-type action transaction-hook-action)
    (aprog1
        (make-instance 'transaction-hook
                       :function function
                       :when when
                       :action action)
      (push it (hooks-of transaction)))))

(def (function e) savepoint (name)
  (execute (format nil "SAVEPOINT ~a" name)))

(def generic backend-release-savepoint (name database))

(def (function e) release-savepoint (name)
  (backend-release-savepoint name *database*))

(def (function e) rollback-to-savepoint (name)
  (execute (format nil "ROLLBACK TO SAVEPOINT ~a" name)))
