;;;; -*- Mode: Lisp -*-
;;;; $Id: package.lisp 835 2011-03-14 16:39:54Z binghe $

(in-package :cl-user)

(defpackage snmp.test
  (:use :common-lisp :snmp))

(in-package :snmp.test)

(defmacro with-open-snmp-server (() &body body)
  `(progn (enable-snmp-service)
     (sleep 1)
     (unwind-protect
         (progn ,@body)
       (disable-snmp-service))))

(defun do-tests ()
  )
