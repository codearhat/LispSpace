;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (function e) start-swank-server (port &key (bind-address "127.0.0.1"))
  (format *debug-io* "Starting Swank server on address ~A, port ~A...~%" bind-address port)
  (bind (((:values started? error) (ignore-errors
                                     (with-simple-restart (continue "Ok, go on without a Swank server")
                                       (let ((swank::*loopback-interface* bind-address))
                                         (swank:create-server :port port
                                                              :style :spawn
                                                              :dont-close #t)))
                                     #t)))
    (if started?
        (format *debug-io* "Swank server has been started~%")
        (warn "Swank server failed to start due to: ~A" error))))

(def function invoke-slime-debugger (condition &key (otherwise nil))
  (if (or swank::*emacs-connection*
          (swank::default-connection))
      ;; tell the debugger to hide the upcoming (uninteresting) frames...
      (bind (#+sbcl(sb-debug:*stack-top-hint* (sb-di:top-frame)))
        (swank:swank-debugger-hook condition nil))
      (handle-otherwise/value otherwise :default-message "Unable to invoke Slime debugger")))

(unless (fboundp 'collect-backtrace)
  (def function collect-backtrace (&key (start 4) (count 500) &allow-other-keys)
    (bind ((swank::*buffer-package* *package*))
      (swank-backend:call-with-debugging-environment
       (lambda ()
         (iter (for (index description) :in (swank:backtrace start (+ start count)))
               (collect (format nil "~3,'0D: ~A" index description))))))))
