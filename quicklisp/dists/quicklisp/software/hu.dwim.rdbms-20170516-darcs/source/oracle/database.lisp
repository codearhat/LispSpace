;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.oracle)

(def (class* e) oracle (database)
  ((connection-encoding
    :utf-16
    :type (member :ascii :utf-16))))

(def method transaction-mixin-class list ((db oracle))
  'oracle-transaction)

(def class* oracle-transaction (transaction)
  ((environment-handle-pointer nil :accessor environment-handle-pointer)
   (error-handle nil :accessor error-handle-pointer)
   (server-handle nil :accessor server-handle-pointer)
   (service-context-handle nil :accessor service-context-handle-pointer)
   (session-handle nil :accessor session-handle-pointer)
   (session-schema nil :accessor session-schema)))

(macrolet ((def (&rest names)
               `(progn
                 ,@(loop for name in names
                         collect `(defmacro ,(format-symbol (find-package :hu.dwim.rdbms.oracle) "~A-OF" name) (transaction)
                                   `(cffi:mem-ref (,',(format-symbol (find-package :hu.dwim.rdbms.oracle) "~A-POINTER" name) ,transaction)
                                    '(:pointer :void)))))))
  (def
    environment-handle
    error-handle
    server-handle
    session-handle
    service-context-handle))

(def class* oracle-prepared-statement (prepared-statement)
  ((statement-handle-pointer nil :accessor statement-handle-pointer)
   (bindings nil :type list)))

(def function statement-handle-of (statement)
  (cffi:mem-ref (statement-handle-pointer statement) '(:pointer :void)))


(def function oci-call (code)
  (declare (cl:type fixnum code))
  (handle-oci-result code))

(def function handle-oci-result (result)
  (case result
    (#.oci:+success+
     result)
    (#.oci:+error+
     (handle-oci-error))
    (#.oci:+no-data+
     (simple-rdbms-error "OCI No data found"))
    (#.oci:+success-with-info+
     ;; TODO warn or notify somehow?
     #+nil(simple-rdbms-error "Internal error: unexpected oci:+success-with-info+")
     result)
    (#.oci:+invalid-handle+
     (simple-rdbms-error "OCI Invalid handle"))
    (#.oci:+need-data+
     (simple-rdbms-error "OCI Need data"))
    (#.oci:+still-executing+
     (error 'sql-temporary-error :format-control "OCI Still Executing"))
    (#.oci:+continue+
     (simple-rdbms-error "OCI Continue"))
    (1804
     (simple-rdbms-error "Check ORACLE_HOME and NLS settings"))
    (t
     (simple-rdbms-error "Unknown OCI error, code is ~A" result))))

(def (condition* ea) oci-error (rdbms-error)
  ((oci-error-message nil)
   (oci-error-code nil))
  (:report (lambda (condition stream)
	     (format stream "OCI error: ~@[[~A]~]: ~@[~A~]"
                     (oci-error-code-of condition)
                     (oci-error-message-of condition)))))

(def function oci-error (message &optional code)
  (error 'oci-error :oci-error-message message :oci-error-code code))

(def function handle-oci-error ()
  (unless (error-handle-pointer *transaction*)
    (oci-error "OCI error in initialization stage, too early to query the actual error"))
  (cffi:with-foreign-objects ((error-code 'oci:sb-4))
    (cffi:with-foreign-pointer (error-buffer oci:+error-maxmsg-size+)
      (oci:error-get (error-handle-of *transaction*) 1
                     (cffi:null-pointer)
                     error-code
                     error-buffer
                     oci:+error-maxmsg-size+ oci:+htype-error+)
      (bind ((message (oci-string-to-lisp error-buffer))
             (code (cffi:mem-ref error-code 'oci:sb-4)))
        (rdbms.debug "Signalling OCI error: ~A, ~A" message code)
        (oci-error message code)))))

(def constant +maximum-rdbms-name-length+ 30)

(def method calculate-rdbms-name ((db (eql :oracle)) thing name)
  ;; TODO this may not be neccessary for oracle, or at least not the same way as for the postgres backend.
  ;; table names in oracle queries are unconditionally in quotes (iirc, that is)
  (calculate-rdbms-name-with-utf-8-length-limit name +maximum-rdbms-name-length+ :prefix "_"))

(def method calculate-rdbms-name ((db oracle) thing name)
  (calculate-rdbms-name :oracle thing name))

(defun database-effective-schema (db)
  (destructuring-bind (&key schema user-name &allow-other-keys)
      (connection-specification-of db)
    (string-upcase (or schema user-name))))
