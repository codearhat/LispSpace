;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

;;;;;;
;;; Default logger levels

(def (constant e) +dribble+ 0)
(def (constant e) +debug+   1)
(def (constant e) +info+    2)
(def (constant e) +warn+    3)
(def (constant e) +error+   4)
(def (constant e) +fatal+   5)

(def (constant e :test 'equalp) +log-level-names+ #(+dribble+ +debug+ +info+ +warn+ +error+ +fatal+))

(def (constant e :test 'equalp) +log-level-keywords+ '(:dribble :debug :info :warn :error :fatal))

(def type log-level ()
  `(integer ,+dribble+ ,+fatal+))

(def type logger-name ()
  `(and symbol
        (not (eql t))
        (not null)))

;;;;;;
;;; Logger

(def (class* e) logger ()
  ((parents nil :documentation "The parent logger this logger inherits from.")
   (children nil :documentation "The loggers which inherit from this logger.")
   (appenders nil :documentation "A list of appender objects this logger should send messages to.")
   (runtime-level nil :type (or null integer) :documentation "The runtime log level determines whether an actual log message shows up at runtime.")
   (compile-time-level nil :type (or null integer) :documentation "The compile time log level is a compile time filter. Log expressions below this level will macro-expand to NIL at compile time.")
   (name)
   (documentation nil)))

(def method make-load-form ((self logger) &optional env)
  (declare (ignore env))
  (let ((name (name-of self)))
    `(let ((result (find-logger ',name)))
       (assert result () ,(format nil "There must be some load-order issue with your loggers, because ~S was not found at load-time."
                                  name))
       result)))

(def print-object logger
  (with-keyword-package
    (write (name-of -self-))))

(def method shared-initialize :after ((self logger) slot-names &key parents)
  (declare (ignore slot-names))
  (dolist (parent parents)
    (pushnew self (children-of parent) :test (lambda (a b)
                                               (eql (name-of a) (name-of b))))))

;;;;;;
;;; Namespace

(def (namespace :finder-name %find-logger) logger)

(def (function e) find-logger (name &key (otherwise nil otherwise?))
  (check-type name logger-name)
  (if otherwise?
      (%find-logger name :otherwise otherwise)
      (%find-logger name)))

#-allegro ;; TODO THL why? logger not found when loading from fasl
(def compiler-macro find-logger (&whole whole name &key (otherwise nil otherwise?))
  (if (quoted-symbol? name)
      `(load-time-value (%find-logger ,name ,@(when otherwise? `(:otherwise ,otherwise))))
      whole))

(def (function e) (setf find-logger) (logger name)
  (check-type logger logger)
  (check-type name logger-name)
  (setf (%find-logger name) logger))

;;;;;;
;;; Runtime level

(def function at-runtime-enabled? (logger level)
  (>= level (log-level/runtime logger)))

(def (function e) log-level (logger)
  (log-level/runtime logger))

(def function (setf log-level) (new-value logger)
  (setf (log-level/compile-time logger) new-value)
  (setf (log-level/runtime logger) new-value)
  new-value)

(def (function e) set-log-level (logger level)
  (setf (log-level logger) level)
  level)

(def function validate-log-level (value)
  (etypecase value
    (log-level
     value)
    (keyword
     (or (find value +log-level-keywords+ :test 'eq)
         (cerror "Use +INFO+ instead" "~S is not a valid log level keyword" value)
         +info+))))

(def (function e) log-level/runtime (logger)
  (if (symbolp logger)
      (log-level/runtime (find-logger logger))
      (or (runtime-level-of logger)
          (if (parents-of logger)
              (loop
                :for parent :in (parents-of logger)
                :minimize (log-level/runtime parent))
              (error "Can't determine runtime level for ~S" logger)))))

(def function (setf log-level/runtime) (new-level logger &key recursive)
  (setf new-level (validate-log-level new-level))
  (if (symbolp logger)
      (setf (log-level/runtime (find-logger logger) :recursive recursive) new-level)
      (progn
        (setf (runtime-level-of logger) new-level)
        (when recursive
          (dolist (child (children-of logger))
            (setf (log-level/runtime child) new-level)))
        new-level)))

;;;;;;
;;; Compile time level

;; the following is a bit of a copy-paste, but let's just skip macrology for only two instances...
(def function at-compile-time-enabled? (logger level)
  (>= level (log-level/compile-time logger)))

(def (function e) log-level/compile-time (logger)
  (if (symbolp logger)
      (log-level/compile-time (find-logger logger))
      (or (compile-time-level-of logger)
          (if (parents-of logger)
              (loop
                :for parent :in (parents-of logger)
                :minimize (log-level/compile-time parent))
              (error "Can't determine compile-time level for ~S" logger)))))

(def function (setf log-level/compile-time) (new-level logger &key recursive)
  (setf new-level (validate-log-level new-level))
  (if (symbolp logger)
      (setf (log-level/compile-time (find-logger logger) :recursive recursive) new-level)
      (progn
        (setf (compile-time-level-of logger) new-level)
        (when recursive
          (dolist (child (children-of logger))
            (setf (log-level/compile-time child) new-level)))
        new-level)))

(def (macro e) with-logger-level ((logger-name new-level) &body body)
  "Set the runtime level of the listed logger(s) to NEW-LEVEL and restore the original value in an unwind-protect."
  (cond ((consp logger-name)
         `(with-logger-level (,(pop logger-name) ,new-level)
            ,(if logger-name
                 `(with-logger-level (,logger-name ,new-level)
                    ,@body)
                 `(progn
                    ,@body))))
        ((symbolp logger-name)
         (with-unique-names (logger old-level)
           `(bind ((,logger (find-logger ',logger-name))
                   (,old-level (runtime-level-of ,logger)))
              (setf (runtime-level-of ,logger) ,new-level)
              (unwind-protect
                   (progn ,@body)
                (setf (runtime-level-of ,logger) ,old-level)))))
        (t (error "Don't know how to interpret ~S as a logger name" logger-name))))

;;;;;;
;;; Handling messages

(def with-macro with-logging-io ()
  (let ((*print-right-margin* most-positive-fixnum)
        (*print-readably* nil)
        (*print-length* 32)
        (*print-level* 2)
        (*package* (load-time-value (find-package "COMMON-LISP"))))
    (-with-macro/body-)))

(def (special-variable :documentation "While inside HANDLE-LOG-MESSAGE, this variable is bound to the logger on which HANDLE-LOG-MESSAGE was called first, ignoring logger inheritance and handler delegation.")
  *toplevel-logger*)

(def function note-logging-error (error &key context message-control message-arguments)
  (maybe-invoke-debugger error :context context)
  ;; NOTE don't use ~A, we don't want errors from print-object to interfere when printing the warning...
  (warn "Ignoring the following error coming from ~S: ~S. The context is ~S, the relevant log message is ~S, the arguments were ~S. For more information see ~S and/or ~S. "
        'handle-log-message error context message-control message-arguments '*debug-on-error* 'debug-on-error?))

(def function call-handle-log-message (logger level message-control message-arguments)
  ;; TODO add details on why it's a problem or just delete this assert. comes up from audit log opening transactions...
  #+nil
  (when (boundp '*toplevel-logger*)
    (maybe-invoke-debugger
     (make-condition 'simple-error
                     :format-control "Calling the entry point macros of hu.dwim.logger is not allowed while already inside the logging infrastructure. The recursive call of CALL-HANDLE-LOG-MESSAGE was invoked with the arguments ~A. The toplevel call was invoked on logger ~A."
                     :format-arguments (list (list logger level message-control message-arguments)
                                             *toplevel-logger*))
     :context *toplevel-logger*)
    (throw 'unwind-call-handle-log-message (values)))
  (catch 'unwind-call-handle-log-message
    (bind ((*toplevel-logger* logger))
      ;; The logging facility should not alter the behavior of the application, therefore by default all errors coming from the logging
      ;; infrastructure are masked here.
      (handler-bind
          ((serious-condition (lambda (error)
                                (note-logging-error error :context logger :message-control message-control :message-arguments message-arguments)
                                (throw 'unwind-call-handle-log-message (values)))))
        (with-logging-io
          (handle-log-message logger level message-control message-arguments)))))
  (values))

(def method handle-log-message ((logger logger) level message-control message-arguments)
  (dolist (appender (appenders-of logger))
    (append-message logger appender level message-control message-arguments))
  (dolist (parent (parents-of logger))
    (handle-log-message parent level message-control message-arguments)))

(def function collect-helper-names (loggern-name)
  (flet ((make (suffix)
           (format-symbol (symbol-package loggern-name) "~A.~A" (symbol-name loggern-name) (symbol-name suffix))))
    (list (make '#:dribble)
          (make '#:debug)
          (make '#:info)
          (make '#:warn)
          (make '#:error)
          (make '#:fatal))))

(def (macro e) deflogger (name parents &key accessor-name-prefix compile-time-level runtime-level appender appenders documentation)
  (check-type name logger-name)
  (unless accessor-name-prefix
    (setf accessor-name-prefix (string+ (symbol-name name) ".")))
  (unless (eq (symbol-package name) *package*)
    (simple-style-warning "When defining a logger named ~A, the home package of the symbol is not *package* (not (eq ~A ~A))"
                          (fully-qualified-symbol-name name)
                          (symbol-package name) *package*))
  (when appender
    (setf appenders (append appenders (list appender))))
  (let ((parents (or (mapcar (lambda (parent)
                               `(or (find-logger ',parent :otherwise #f)
                                    (error "Attempt to define a sub-logger of the undefined logger ~S." ',parent)))
                             parents)
                     (unless (eq name 'root-logger) ; special case the chicken-egg issue at the definition of the root logger
                       '((find-logger 'root-logger))))))
    (flet ((make-log-helper (suffix level)
             (let ((logger-macro-name (format-symbol (symbol-package name) "~A~A" (string accessor-name-prefix) (symbol-name suffix))))
               `(progn
                  (setf (get ',logger-macro-name 'logger) ',name)
                  (def macro ,logger-macro-name (message-control &rest message-args)
                    ;; first check at compile time
                    (if (at-compile-time-enabled? (find-logger ',name) ,level)
                        ;; then check at runtime
                        (with-unique-names (logger)
                          `(bind ((,logger (load-time-value (find-logger ',',name))))
                             (when (at-runtime-enabled? ,logger ,',level)
                               ,(if message-args
                                    `(call-handle-log-message ,logger ',',level ,message-control (list ,@message-args))
                                    `(call-handle-log-message ,logger ',',level ,message-control nil)))
                             (values)))
                        `(values)))))))
      (with-unique-names (logger)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (unless (find-logger ',name :otherwise #f)
               (setf (find-logger ',name) (make-instance 'logger
                                                         :name ',name
                                                         :parents (list ,@parents)
                                                         ,@(when compile-time-level
                                                                 `(:compile-time-level ,compile-time-level))))))
           (eval-when (:load-toplevel :execute)
             (let ((,logger (find-logger ',name)))
               ,(when runtime-level
                      `(setf (runtime-level-of ,logger) ,runtime-level))
               ,(when compile-time-level
                      `(setf (compile-time-level-of ,logger) ,compile-time-level))
               (setf (appenders-of ,logger) (remove nil (list ,@appenders)))
               (setf (parents-of ,logger) (list ,@parents))
               (setf (documentation-of ,logger) ,documentation)))
           ,(make-log-helper '#:dribble '+dribble+)
           ,(make-log-helper '#:debug '+debug+)
           ,(make-log-helper '#:info '+info+)
           ,(make-log-helper '#:warn '+warn+)
           ,(make-log-helper '#:error '+error+)
           ,(make-log-helper '#:fatal '+fatal+)
           (values))))))

(def (definer e :available-flags "e") logger (name parents &key accessor-name-prefix compile-time-level runtime-level appender appenders documentation)
  `(progn
     (deflogger ,name ,parents
       :accessor-name-prefix ,accessor-name-prefix
       :runtime-level ,runtime-level
       :compile-time-level ,compile-time-level
       :appender ,appender
       :appenders ,appenders
       :documentation ,documentation)
     ,@(ecase (getf -options- :export)
         (:printers `((export ',(collect-helper-names name))))
         ((nil) (values))
         ((t) `((export ',(cons name (collect-helper-names name))))))))
