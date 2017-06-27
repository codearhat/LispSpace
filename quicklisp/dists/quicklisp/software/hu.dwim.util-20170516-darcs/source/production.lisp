;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; some utils

(def (function e) ensure-default-external-format-is-utf-8 ()
  #+sbcl
  (unless (eq (sb-impl::default-external-format) :utf-8)
    (cerror "Ignore" "The default external format is ~S, but UTF-8 is strongly advised! Check your $LANG env variable..."
            (sb-impl::default-external-format))))

(def (function e) load-and-eval-config-file (system-name)
  (bind ((pathname (merge-pathnames (string+ (string-downcase system-name) ".lisp") "config/"))
         (config-file-name (system-relative-pathname system-name pathname)))
    (if (uiop:file-exists-p config-file-name)
        (with-local-readtable
          (bind ((*package* (find-package :hu.dwim.common)))
            ;; load using the special in-package symbol that sets up the readtable based on what the package says
            (load config-file-name))
          config-file-name)
        nil)))

(def (with-macro* e) with-pid-file (pathname)
  (check-type pathname (or pathname string))
  (bind ((pid-file-has-been-created? #f))
    (unwind-protect
         (progn
           (when (uiop:file-exists-p pathname)
             (bind ((other-pid (parse-integer (read-file-into-string pathname))))
               (if (posix-process-exists? other-pid)
                   (error "PID file ~S already exists and points to a running process ~S" pathname other-pid)
                   (progn
                     (format *debug-io* "Deleting stale PID file ~S pointing to non-existent PID ~S~%" pathname other-pid)
                     (delete-file pathname)))))
           (bind ((pid (isys:getpid)))
             (format *debug-io* "Writing PID file ~S, PID is ~S~%" pathname pid)
             (ensure-directories-exist pathname)
             (with-open-file (pid-stream pathname :direction :output
                                         :element-type 'character
                                         :if-exists :error)
               (princ pid pid-stream))
             (setf pid-file-has-been-created? #t)
             (-with-macro/body-)))
      (when pid-file-has-been-created?
        (unless (ignore-errors
                  (delete-file pathname)
                  #t)
          (print-error-safely "Failed to remove pid file ~S~%" pathname))))))

(def (with-macro* e) with-temporary-directory (&key (cleanup #t))
  (when cleanup
    (cleanup-temporary-directories))
  (directory-for-temporary-files)
  (multiple-value-prog1
      (-with-macro/body-)
    (delete-directory-for-temporary-files)))

(def (with-macro e) with-save-core-and-die-restart ()
  (restart-case
      (-body-)
    #+sbcl
    (save-core-and-die ()
      :report "Save image to /tmp/sbcl.core and die"
      (mapcar
       (lambda (thread)
         (unless (eq thread sb-thread:*current-thread*)
           (sb-thread:terminate-thread thread)))
       (sb-thread:list-all-threads))
      (sb-ext:save-lisp-and-die "/tmp/sbcl.core"))))

(def (with-macro e) with-standard-toplevel-restarts ()
  (restart-case
      (with-save-core-and-die-restart
        (-body-))
    (abort nil
      :report (lambda (stream)
                (format stream "Give up starting the image and quit the VM process with exit code 2"))
      (quit 2))))

;;;;;;
;;; database integration

(def class* perec-on-postgresql (hu.dwim.perec:database-mixin
                                 hu.dwim.rdbms.postgresql:postgresql)
  ())

;;;;;;
;;; starting up

(def (constant e) +generic-command-line-options+
  '((("verbose" #\Space)
     :type boolean
     :optional #t
     :documentation "Try to provide more information about what's happening.")
    (("http-server-port" #\Space)
     :type integer
     :initial-value #.hu.dwim.web-server::+default-http-server-port+
     :documentation "The HTTP port where the server will be listening for incoming requests")
    (("pid-file" #\Space)
     :type string
     :documentation "The PID file is created when the server starts. The file will be deleted when the server stops.")
    (("swank-port" #\Space)
     :type integer
     :initial-value 0
     :documentation "On which port Swank (SLIME) should listen. By default Swank is not started.")
    (("swank-address" #\Space)
     :type string
     :initial-value "127.0.0.1"
     :documentation "On what address/interface Swank (SLIME) should listen.")
    (("disable-debugger" #\Space)
     :type boolean
     :optional #t
     :documentation "Disable the debugger, so that in case of unhandled toplevel errors the process quits. True by default unless in --repl mode.")
    (("repl" #\Space)
     :type boolean
     :optional #t
     :documentation "If provided then instead of starting the server only a REPL will be started. This might be useful for mainenance, testing and bug fixing.")
    (("test-mode" #\Space)
     :type boolean
     :optional #t
     :documentation "If provided then the server starts up in test mode. This allows to login with the same password for any subject.")
    (("export-model" #\Space)
     :type boolean
     :optional #f
     :documentation "When started in REPL mode, skip exporting the model into the RDBMS")))

(def hu.dwim.logger:logger production ())

(def (function e) run-production-server (command-line-arguments project-system-name hdws-server hdws-application &key
                                         (log-directory #P"/var/log/")
                                         (default-http-port hu.dwim.web-server::+default-http-server-port+)
                                         (database 'perec-on-postgresql))
  (labels ((console (format &rest args)
             (apply 'hu.dwim.logger:log-to-console format args))
           (ready-to-quit? (hdws-server)
             (not (or (hu.dwim.web-server:is-server-running? hdws-server)
                      #+nil (is-persistent-process-scheduler-running?)
                      #+nil (hu.dwim.model:is-cluster-node-running?))))
           ;; KLUDGE this is fragile, and the whole thing is much more complicated...
           (process-http-server-port-command-line-argument (arguments server)
             (when-bind http-server-port (getf arguments :http-server-port)
               (awhen (find default-http-port (hu.dwim.web-server::listen-entries-of server) :key #'hu.dwim.web-server::port-of)
                 (setf (hu.dwim.web-server::port-of it) http-server-port)))))
    (console "~A: Starting up server, PID is ~S" (local-time:now) (isys:getpid))
    (process-http-server-port-command-line-argument command-line-arguments hdws-server)
    (hu.dwim.logger:setup-logging-for-production (merge-pathnames (pathname (string+ (string-downcase project-system-name) "/"))
                                                                  log-directory))
    (production.info "~S speaking, starting up in production mode ~S" 'run-production-server project-system-name)
    (bind ((project-system (asdf:find-system project-system-name))
           (project-package (find-package (system-package-name project-system)))
           (database (if (typep database 'hu.dwim.rdbms:database)
                         database
                         (aprog1
                             (make-instance database)
                           (check-type it hu.dwim.rdbms:database)
                           (assert (null (hu.dwim.web-server::database-of hdws-application)) () "You gave only a database type (as opposed to an instance) to ~S, but the provided web-application instance already holds a database!" 'run-production-server)
                           (setf (hu.dwim.web-server::database-of hdws-application) it)))))
      (unless (member (hu.dwim.web-server::database-of hdws-application) (list nil database))
        (error "The database given to ~S and stored in the slot of the web-application instance is inconsistent!" 'run-production-server))
      (check-type project-package package)
      (setf *package* project-package)
      (production.debug "*package* was set ~A" *package*)
      (setf *random-state* (make-random-state t))
      (ensure-default-external-format-is-utf-8)
      ;; TODO what about *terminal-io*? maybe: (setf *terminal-io* *standard-output*)
      ;; TODO: factor out the database arguments into rdbms
      (bind (((&key database-host database-port database-name database-user-name database-password
                    pid-file test-mode swank-port swank-address repl verbose (disable-debugger #t disable-debugger-provided?) (export-model #t)
                    &allow-other-keys) command-line-arguments)
             (connection-specification `(:host ,database-host :port ,database-port :database ,database-name :user-name ,database-user-name :password ,database-password))
             (loggable-connection-specification (remove-from-plist connection-specification :password)))
        (when (and swank-port
                   (not (zerop swank-port)))
          (start-swank-server swank-port :bind-address swank-address))
        (when (and disable-debugger
                   (or (not repl)
                       disable-debugger-provided?))
          (disable-debugger))
        (when verbose
          (bind ((root-logger (hu.dwim.logger:find-logger 'hu.dwim.logger:root-logger)))
            (appendf (hu.dwim.logger::appenders-of root-logger)
                     (list (hu.dwim.logger:make-thread-safe-stream-appender '*standard-output*)))
            (setf (hu.dwim.logger:log-level/runtime root-logger) hu.dwim.logger:+debug+)
            (production.debug "Set loggers to be verbose as requested by --verbose")))
        (when (and (not export-model)
                   (not repl))
          (cerror "Start in repl mode" "Skipping ~S is only allowed in REPL mode" 'export-persistent-classes-to-database-schema)
          (setf repl #t))
        (production.info "Using database connection specification ~S with database ~A" loggable-connection-specification database)
        (unless (and database-host database-port database-name database-user-name database-password)
          (warn "Database connection specification is not fully provided, which will most probably lead to an error (password omitted from logs): ~S" loggable-connection-specification))
        (setf (hu.dwim.rdbms::connection-specification-of database) connection-specification)
        (awhen test-mode
          (production.info "Enabling test mode")
          (setf (hu.dwim.web-server:running-in-test-mode? hdws-application) #t)
          (unless (search "-test" database-name)
            (cerror "Continue"
                    "Do you really want to start up in test mode with a database name that does not contain \"-test\"? (~S)."
                    database-name)))
        (when export-model
          (production.info "Calling EXPORT-PERSISTENT-CLASSES-TO-DATABASE-SCHEMA with database ~A, connection-specification ~A" database loggable-connection-specification)
          (hu.dwim.rdbms:with-database database
            (hu.dwim.perec:with-new-compiled-query-cache
              (hu.dwim.rdbms:with-transaction
                (hu.dwim.perec:export-persistent-classes-to-database-schema)))))
        (awhen (load-and-eval-config-file project-system-name)
          (production.info "Loaded config file ~A" it))
        (if repl
            (sb-impl::toplevel-repl nil)
            (with-layered-error-handlers ((lambda (error)
                                            (print-error-safely (build-error-log-message :error-condition error
                                                                                         :message "Error reached toplevel in the main thread"))
                                            (unless disable-debugger
                                              (invoke-debugger error)))
                                        (lambda (&key &allow-other-keys)
                                          (print-error-safely "Calling QUIT from toplevel error handler")
                                          (quit 3)))
            (with-temporary-directory ()
              (flet ((startup-signal-handler (signal code scp)
                       (declare (ignore signal code scp))
                       (console "SIGTERM/SIGINT was received while starting up; exiting recklessly")
                       (quit 2)))
                #*((:sbcl
                    (sb-sys:enable-interrupt sb-unix:sigterm #'startup-signal-handler)
                    (sb-sys:enable-interrupt sb-unix:sigint #'startup-signal-handler)
                    (console "Temporary startup signal handlers are installed"))
                   (t (warn "No support for installing signal handlers on your implementation, stale PID files may remain"))))
              (surround-body-when pid-file
                  (with-pid-file (pid-file)
                    (-body-))
                (handler-bind ((hu.dwim.rdbms:unconfirmed-schema-change
                                ;; NOTE: this handler is not bound in the started worker threads but EXPORT-PERSISTENT-CLASSES-TO-DATABASE-SCHEMA is explicitly called at startup, so this is not a problem.
                                (lambda (error)
                                  (print-error-safely "Exiting because something was tried to be altered in the RDBMS schema at unattended startup: ~A" error))))
                  #+nil
                  (with-new-compiled-query-cache
                    (hu.dwim.model:startup-cluster-node cluster-name hdws-server))
                  (hu.dwim.web-server:startup-server hdws-server)
                  ;; TODO: put the timer stuff in hu.dwim.web-server and remove dependency
                  (bind ((timer (hu.dwim.web-server::timer-of hdws-server)))
                    (flet ((%register-timer-entry (name time-interval thunk)
                             (hu.dwim.web-server:register-timer-entry timer thunk :interval time-interval :name name))
                           (console-status-printer ()
                             (console "~A: Another heartbeat at request number ~A, used memory ~,2F MiB, application session count ~A"
                                      (local-time:now)
                                      (when hdws-server
                                        (hu.dwim.web-server:processed-request-counter-of hdws-server))
                                      (/ (sb-kernel::dynamic-usage) 1024 1024)
                                      (when hdws-server
                                        (mapcar (compose 'hash-table-count 'hu.dwim.web-server:session-id->session-of)
                                                (collect-if (of-type 'hu.dwim.web-server:application)
                                                            (hu.dwim.web-server:brokers-of hdws-server))))))
                           (session-purge ()
                             (hu.dwim.rdbms:with-database database
                               (hu.dwim.perec:with-new-compiled-query-cache
                                 (hu.dwim.rdbms:with-transaction
                                   (hu.dwim.web-server:purge-sessions hdws-application)))))
                           (quit-request-checker ()
                             (when (ready-to-quit? hdws-server)
                               (hu.dwim.web-server:drive-timer/abort timer))))
                      (%register-timer-entry "Console status printer" (* 60 10) #'console-status-printer)
                      (%register-timer-entry "Session purge" 60 #'session-purge)
                      (%register-timer-entry "Quit request checker" 5 #'quit-request-checker)
                      (%register-timer-entry "Log flusher" 5 'hu.dwim.logger:flush-caching-appenders)
                      #+nil
                      (%register-timer-entry "Status logger" 5
                                             ;; TODO log some useful info like the number of web sessions, etc...
                                             )
                      (flet ((running-signal-handler (signal code scp)
                               (declare (ignore signal code scp))
                               (production.info "SIGTERM/SIGINT was received, initiating shutdown")
                               (console "~%SIGTERM/SIGINT was received, initiating shutdown")
                               (hu.dwim.web-server:drive-timer/abort timer)))
                        (sb-sys:enable-interrupt sb-unix:sigterm #'running-signal-handler)
                        (sb-sys:enable-interrupt sb-unix:sigint #'running-signal-handler)))
                    (production.info "Final signal handlers are installed, everything's started normally. Calling into DRIVE-TIMER now...")
                    (console "~A: Everything's started normally" (local-time:now))
                    (hu.dwim.web-server::drive-timer timer))
                  ;; (hu.dwim.model:shutdown-cluster-node)
                  (hu.dwim.web-server:shutdown-server hdws-server)
                  (iter (until (ready-to-quit? hdws-server))
                        (production.debug "Still not ready to quit, waiting...")
                        (sleep 1))
                  (hu.dwim.logger:flush-caching-appenders))))
            (production.info "Everything's down, exiting normally")
            (console "~A: Everything's down, exiting normally" (local-time:now))))))))
