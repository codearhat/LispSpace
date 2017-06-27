;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def (generic e) handle-log-message (logger level message-control message-arguments)
  (:documentation "The first line of the log message handling, called by the printer macros. The default implementation if this method delegates the call to all the parents of the logger."))

(def (generic e) append-message (logger appender level message-control message-arguments)
  (:documentation "The second line of the log message handling. Called for each logger-appender pair that receives this log message."))

(def (generic e) format-message (logger appender formatter level stream message-control message-arguments)
  (:documentation "This method is a customization point where subclasses can customize how to format the log message. Subclasses of stream-appender call this method by default, but some other appenders may chose not to call it by customizing APPEND-MESSAGE.")
  (:method :around (logger appender formatter level (output-stream (eql nil)) message-control message-arguments)
    ;; mimic the semantics of CL:FORMAT when called on NIL output stream
    (with-output-to-string (stream)
      (format-message logger appender formatter level stream message-control message-arguments))))

(def (class* e) appender ()
  ((formatter)))

(def class* formatter ()
  ())

(def (function e) log-to-console (format-control &rest format-arguments)
  (apply 'format *standard-output* format-control format-arguments)
  (terpri *standard-output*)
  (finish-output *standard-output*))
