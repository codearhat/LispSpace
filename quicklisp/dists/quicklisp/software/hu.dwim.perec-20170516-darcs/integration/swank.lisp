;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Slime inspector integration

#+hu.dwim.slime
(progn
(defmethod swank::slot-value-for-inspector ((class persistent-class)
                                            (instance persistent-object)
                                            (slot persistent-effective-slot-definition))
  (cond ((or (not (in-transaction-p))
             (not (instance-in-transaction-p instance)))
         `((:value ,(with-transaction
                     (typecase slot
                       (persistent-effective-slot-definition-d
                        (restore-slot class instance slot :coordinates (collect-coordinates-from-variables (dimensions-of slot))))
                       (persistent-effective-slot-definition
                        (restore-slot class instance slot)))))))
        ((debug-persistent-p instance)
         `(,@(if (slot-value-cached-p instance slot)
                 `((:value ,(standard-instance-access instance (slot-definition-location slot)))
                   " "
                   (:action "[invalidate cache]" ,(lambda () (invalidate-cached-slot instance slot))))
                 `("#<not cached>"
                   " "
                   (:action "[read into cache]" ,(lambda () (setf (underlying-slot-value-using-class class instance slot)
                                                             (slot-value-using-class class instance slot))))))))
        (t (call-next-method))))

(defmethod swank-backend::emacs-inspect ((instance persistent-object))
  (flet ((annotate (content)
           (bind ((persistent (if (or (slot-boundp instance 'persistent)
                                      (instance-in-current-transaction-p instance))
                                  (persistent-p instance)
                                  :unknown)))
             `(,(ecase persistent
                  (:unknown
                   "An instance not yet known to be persistent")
                  ((#f)
                   "A transient instance")
                  (#t
                   "A persistent instance"))
                ,(if (instance-in-transaction-p instance)
                     (if (instance-in-current-transaction-p instance)
                         " in the current transaction."
                         " from another transaction.")
                     ", detached from transactions.")
                (:newline)
                ,@(when (and persistent
                             (instance-in-transaction-p instance))
                   `("Instance transaction = " (:value ,(transaction-of instance)) "." (:newline)))
                ,@(when (and persistent
                             (in-transaction-p)
                             (not (instance-in-current-transaction-p instance)))
                   `("Current transaction  = " (:value ,*transaction*) "." (:newline)))
                (:newline)
                ,@content))))
    (bind ((result (call-next-method)))
      (if (and (consp result)
               (keywordp (first result)))
          (progn
            (setf result (copy-list result))
            (setf (getf result :content) (annotate (getf result :content))))
          (setf result (annotate result)))
      result)))
)
