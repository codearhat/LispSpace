;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def function logger-name-for-output (logger)
  (fully-qualified-symbol-name (name-of logger) :separator "::"))

(def (function o) format-or-write-string (stream message-control &optional message-arguments)
  (if message-arguments
      (apply #'format stream message-control message-arguments)
      (write-string message-control stream)))

;;;;;;
;;; Stream appender

(def (class* e) stream-appender (appender)
  ((stream '*standard-output*))
  (:documentation "Human readable logger."))

(def method stream-of :around ((self appender))
  (bind ((result (call-next-method)))
    (if (symbolp result)
        (symbol-value result)
        result)))

(def print-object stream-appender
  (prin1 (slot-value -self- 'stream)))

(def (function e) make-stream-appender (&rest args &key (stream '*standard-output*) (verbosity 2) &allow-other-keys)
  (check-type verbosity number)
  (remove-from-plistf args :stream :verbosity)
  (apply #'make-instance 'stream-appender
         :formatter (case verbosity
                      ((0 1) (make-instance 'brief-formatter))
                      (t (make-instance 'verbose-formatter)))
         :stream stream
         args))

(def method append-message (logger (appender stream-appender) level message-control message-arguments)
  (format-message logger appender (formatter-of appender) level (stream-of appender) message-control message-arguments))

(def method append-message :around ((logger logger) (appender stream-appender) level message-control message-arguments)
  (restart-case
      (multiple-value-prog1
          (call-next-method)
        (finish-output (stream-of appender)))
    (use-debug-io ()
      :report (lambda (stream)
                (format stream "Set the output stream of ~A (invoked through ~A) to '*debug-io* and then try again appending this message" appender *toplevel-logger*))
      (setf (stream-of appender) '*debug-io*)
      (append-message logger appender level message-control message-arguments))
    (use-standard-output ()
      :report (lambda (stream)
                (format stream "Set the output stream of ~A (invoked through ~A) to '*standard-output* and then try again appending this message" appender *toplevel-logger*))
      (setf (stream-of appender) '*standard-output*)
      (append-message logger appender level message-control message-arguments))
    (silence-logger ()
      :report (lambda (stream)
                (format stream "Set the output stream of ~A (invoked through ~A) to a deadend" appender *toplevel-logger*))
      (setf (stream-of appender) (make-broadcast-stream)))))

;;;;;;
;;; File appender

(def (special-variable e) *log-directory*)

(def (class* e) file-appender (appender)
  ((log-file :documentation "Name of the file to write log messages to."))
  (:documentation "Logs to a file."))

(def print-object file-appender
  (prin1 (file-appender-output-file -self-)))

(def function file-appender-output-file (appender)
  (bind ((log-file (log-file-of appender)))
    (if (eq (first (pathname-directory log-file)) :absolute)
        log-file
        (merge-pathnames log-file *log-directory*))))

(def (with-macro* :macro-only-arguments (stream-var-name))
    with-output-to-file-appender-file (stream-var-name appender)
  (loop
    (with-simple-restart (retry-writing-log-file "Try to run the entire WITH-OPEN-FILE block again (and potentially emit screwed up or duplicate log entries!)")
      (with-open-file (stream (file-appender-output-file appender) :direction :output :if-exists :append :if-does-not-exist :create)
        (-with-macro/body- (stream stream-var-name)))
      (return))))

(def method append-message ((logger logger) (appender file-appender) level message-control message-arguments)
  (with-output-to-file-appender-file (output appender)
    (format-message logger appender (formatter-of appender) level output message-control message-arguments)))

(def (function e) make-file-appender (file-name &key (formatter (make-instance 'parsable-formatter)))
  (make-instance 'file-appender :log-file file-name :formatter formatter))

;;;;;;
;;; Level filter appender

(def (class* e) level-filtering-appender (stream-appender)
  ((minimum-level +debug+)
   (chained-appenders))
  (:documentation "Drops messages below MINIMUM-LEVEL and forwards the others to CHAINED-APPENDERS."))

(def method append-message ((logger logger) (appender level-filtering-appender) level message-control message-arguments)
  (when (>= (etypecase level
              (number level)
              (symbol (symbol-value level)))
            (minimum-level-of appender))
    (dolist (chained-appender (chained-appenders-of appender))
      (append-message logger chained-appender level message-control message-arguments))))

(def (function e) make-level-filtering-appender (minimum-level &rest chained-appenders)
  (make-instance 'level-filtering-appender :minimum-level minimum-level :chained-appenders chained-appenders))

;;;;;;
;;; caching appender

(def generic flush-caching-appender-messages (appender lines))

(def constant +caching-appender/maximum-cache-size+ 128)

;; we need to keep track of the caching-appender instances to flush their caches...
(def (namespace :weakness :key) caching-appender)

(def (class* e) caching-appender ()
  ((lock (bordeaux-threads:make-lock "a caching-appender of hu.dwim.logger"))
   (last-flushed-at (get-monotonic-time))
   (cache (make-array +caching-appender/maximum-cache-size+ :adjustable #f :fill-pointer 0))
   (async-flushing #f :accessor async-flushing? :type boolean
                   :documentation "Is it ok to call FLUSH-CACHING-APPENDER-MESSAGES without holding the appender lock?")
   (lazy-flushing #t :accessor lazy-flushing? :type boolean
                  :documentation "If an external entity regularly call FLUSH-CACHING-APPENDER on us, then we may be lazy flushing.")))

(def constructor caching-appender
  ;; setf'ing nil here would mean we want the entry removed.
  (setf (find-caching-appender -self-) t))

(def with-macro with-lock-held-on-caching-appender (appender)
  (bordeaux-threads:with-recursive-lock-held ((lock-of appender))
    (-body-)))

(def method flush-caching-appender-messages :after ((appender caching-appender) lines)
  (setf (last-flushed-at-of appender) (get-monotonic-time)))

(def (function e) flush-caching-appenders ()
  (bind ((appenders (collect-namespace-names 'caching-appender)))
    ;; and now flush without holding the namespace lock...
    (dolist (appender appenders)
      (block flushing
        (handler-bind
            ((serious-condition (lambda (error)
                                  (note-logging-error error :context appender)
                                  (return-from flushing))))
          (flush-caching-appender appender))))))

(def (function e) flush-caching-appender (appender)
  (bind ((lines nil)
         (flushed? #f))
    (flet ((ensure-flushed ()
             (when (and lines
                        (not flushed?))
               (setf flushed? #t)
               (flush-caching-appender-messages appender lines))))
      (with-lock-held-on-caching-appender appender
        (bind ((cache (cache-of appender))
               (cache-size (length cache)))
          (unless (zerop cache-size)
            (setf lines (make-array cache-size :initial-contents cache))
            (setf (fill-pointer cache) 0)))
        (unless (async-flushing? appender)
          (ensure-flushed)))
      (ensure-flushed)))
  (values))

(def method append-message ((logger logger) (appender caching-appender) level message-control message-arguments)
  (bind ((formatted-message (format-message logger appender (formatter-of appender) level nil message-control message-arguments)))
    (with-lock-held-on-caching-appender appender
      (bind ((cache (cache-of appender)))
        (vector-push-extend formatted-message cache)
        (when (or (not (lazy-flushing? appender))
                  (>= (length cache)
                      (array-dimension cache 0)))
          (flush-caching-appender appender)
          ;; we have the lock, so it must be empty here
          (assert (zerop (length cache))))))))

;;;;;;
;;; thread safe stream appender

(def (class* e) thread-safe-stream-appender (caching-appender stream-appender)
  ())

(def method flush-caching-appender-messages ((appender thread-safe-stream-appender) lines)
  (loop
    :with stream = (stream-of appender)
    :for line :across lines
    :do (write-string line stream))
  (values))

(def (function e) make-thread-safe-stream-appender (stream &key (formatter (make-instance 'brief-formatter))
                                                           (lazy-flushing #f))
  (check-type stream (or stream symbol))
  (make-instance 'thread-safe-stream-appender :stream stream :formatter formatter :lazy-flushing lazy-flushing))

;;;;;;
;;; thread safe file appender

(def (class* e) thread-safe-file-appender (caching-appender file-appender)
  ())

(def method flush-caching-appender-messages ((appender thread-safe-file-appender) lines)
  (with-output-to-file-appender-file (output appender)
    (loop
      :for line :across lines
      :do (write-string line output)))
  (values))

(def (function e) make-thread-safe-file-appender (file-name &key (formatter (make-instance 'parsable-formatter)))
  (check-type file-name (or string pathname))
  (make-instance 'thread-safe-file-appender :log-file file-name :formatter formatter))
