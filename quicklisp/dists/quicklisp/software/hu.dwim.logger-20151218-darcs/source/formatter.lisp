;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def constant +max-logger-name-length+ 15)

;;;;;;
;;; brief-formatter

(def (class* e) brief-formatter (formatter)
  ((last-message-year :initform 0)
   (last-message-month :initform 0)
   (last-message-day :initform 0))
  (:documentation "Minimal overhead text in messages. This amounts to: not printing the package names of loggers and log levels and a more compact printing of the current time."))

(def method format-message ((logger logger) appender (formatter brief-formatter) level stream message-control message-arguments)
  (local-time:with-decoded-timestamp (:minute minute :hour hour :day day :month month :year year)
      (local-time:now)
    (with-slots (last-message-year last-message-month last-message-day) formatter
      (unless (and (= year last-message-year)
                   (= month last-message-month)
                   (= day last-message-day))
        (format stream "--TIME MARK ~4,'0D-~2,'0D-~2,'0D--~%"
                year month day)
        (setf last-message-year year
              last-message-month month
              last-message-day day)))
    (bind ((logger-name (symbol-name (name-of *toplevel-logger*)))
           (logger-name-length (length logger-name))
           (level-name (symbol-name level)))
      (format stream
              #.(concatenate 'string
                             "~2,'0D:~2,'0D ~2A ~"
                             (princ-to-string +max-logger-name-length+)
                             "@A ~7A ")
              hour minute
              (human-readable-thread-id)
              (subseq logger-name
                      (max 0 (- logger-name-length
                                +max-logger-name-length+))
                      logger-name-length)
              (subseq level-name 1 (1- (length level-name)))))
    (format-or-write-string stream message-control message-arguments)
    (terpri stream)))

;;;;;;
;;; verbose-formatter

(def (class* e) verbose-formatter (formatter)
  ()
  (:documentation "Attempts to be as precise as possible, logger names and log level names are printed with a package prefix and the time is printed in long format."))

(def method format-message ((logger logger) appender (formatter verbose-formatter) level stream message-control message-arguments)
  (format stream
          "~A ~S ~S: "
          (local-time:now)
          (logger-name-for-output *toplevel-logger*)
          level)
  (format-or-write-string stream message-control message-arguments)
  (terpri stream))

;;;;;;
;;; parsable-formatter

(def (class* e) parsable-formatter (formatter)
  ()
  (:documentation "The output of the file logger is not meant to be read directly by a human."))

(def method format-message ((logger logger) appender (formatter parsable-formatter) level stream message-control message-arguments)
  (bind ((*package* #.(find-package :hu.dwim.logger)))
    (local-time:format-rfc3339-timestring stream (local-time:now))
    (format stream " ~3S ~9S ~A "
            (human-readable-thread-id)
            level
            (logger-name-for-output *toplevel-logger*))
    (write-char #\❲ stream)
    (format-or-write-string stream message-control message-arguments)
    (write-char #\❳ stream)
    (format stream " ~S~%"
            ;; TODO this should eventually be replaced with some smartness coming from with-activity
            (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))))