;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Persistent class d and slot meta objects

(defcclass* persistent-class-d (persistent-class)
  ((persistent-effective-slot-ds
    (compute-as (collect-if [typep !1 'persistent-effective-slot-definition-d] (standard-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition-d))
   (h-class
    (compute-as (find-class (d-class-name->h-class-name (class-name -self-))))
    :type persistent-class
    :documentation "The history class generated for this t class.")
   (parent-slot
    (compute-as (find-persistent-slot (h-class-of -self-) 'd-instance :otherwise nil))
    :type column)
   ;; overrides
   (data-table-slots
    (compute-as (collect-if [and (not (typep !1 'persistent-effective-slot-definition-d)) (data-table-slot-p !1)]
                            (persistent-effective-slots-of -self-))))
   (prefetched-slots
    (compute-as (collect-if [and (not (typep !1 'persistent-effective-slot-definition-d)) (prefetch-p !1)]
                            (persistent-effective-slots-of -self-))))
   (dimensional-slots
    (compute-as (compute-dimensional-slots -self-)))
   (slot-dimensions
    (compute-as (compute-slot-dimensions -self-))
    :documentation "Dimensions of the all normal (not association-end) slots in undefined order. H-instances of the class depends on these dimensions."))
  (:documentation "A dimensional slot value is cached in the underlying slot in d-value."))

(defcclass* persistent-class-h (persistent-class)
  ())

(defcclass* persistent-slot-definition-d (persistent-slot-definition)
  ((dimensions
    nil
    :type (list dimension))
   (inheriting-dimension-index
    (compute-as (position-if (of-type 'inheriting-dimension) (dimensions-of -self-)))
    :type (or null integer))))

(defcclass* persistent-direct-slot-definition-d (persistent-slot-definition-d persistent-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-effective-slot-definition-d (persistent-slot-definition-d persistent-effective-slot-definition)
  ((h-slot
    (compute-as (find-persistent-slot (h-class-of (persistent-slot-definition-class -self-)) (slot-definition-name -self-) :otherwise nil))
    :type persistent-effective-slot-definition)
   (cache
    (compute-as #t))))

(eval-always
  (pushnew :dimensions *allowed-slot-definition-properties*))

;;;;;;
;;; defpclass

(def method expand-defpclass-form :around ((metaclass null) defclass-macro name superclasses slots options)
  (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
         (processed-slots (mapcar 'ensure-list slots))
         (processed-options
          (if (and (not specified-metaclass)
                   (or (find :dimensions processed-slots :test #'member)))
              (append options '((:metaclass persistent-class-d)))
              options)))
    (call-next-method metaclass defclass-macro name superclasses slots processed-options)))

(def function h-class-accessor-name-transformer (name definition)
  "Transform (or h-unused boolean) as boolean, to keep the name consistent with the d-slot accessor."
  (bind ((type (getf definition :type)))
    (when (equal type '(or h-unused boolean))
      (setf definition (copy-list definition)
            (getf definition :type) 'boolean))
    (default-accessor-name-transformer name definition)))

(def method expand-defpclass-form ((metaclass persistent-class-d) defclass-macro name superclasses slots options)
  (flet ((slot-options-of (slot-definition)
           (if (oddp (length slot-definition))
               (cdr slot-definition)   ; no init-form
               (cddr slot-definition))))
    (bind ((h-metaclass-name (d-class-name->h-class-name (class-name (class-of metaclass))))
           (d-class-name name)
           (h-class-name (d-class-name->h-class-name d-class-name))
           (processed-options (remove-if [member (first !1) '(:metaclass :slot-definition-transformer)] options)))
      `(progn
         ,(call-next-method)
         (,defclass-macro ,h-class-name ()
           ,(mapcar (lambda (slot-definition)
                      (bind ((slot-name (car slot-definition))
                             (slot-options (remove-from-plist (slot-options-of slot-definition) :dimensions))
                             (type (getf slot-options :type)))
                        (setf (getf slot-options :type)
                              (if (or-type-p type)
                                  `(or h-unused ,@(cdr type))
                                  `(or h-unused ,type)))
                        (list* slot-name
                               :initform +h-unused-slot-marker+
                               slot-options)))
                    (collect-if [getf (slot-options-of !1) :dimensions] slots))
           (:metaclass ,h-metaclass-name)
           (:accessor-name-transformer #'h-class-accessor-name-transformer)
           ,@processed-options
           )
         (defpassociation
           ((:class ,h-class-name :slot d-instance :type ,d-class-name)
            (:class ,d-class-name :slot h-instances :type (set ,h-class-name))))
         (find-class ',name)))))

;;;;;;
;;; Computed

(def function compute-dimensional-slots (d-class)
  (collect-if (of-type 'persistent-slot-definition-d)
              (persistent-effective-slots-of d-class)))

(def function compute-slot-dimensions (d-class)
  (iter (for slot :in (dimensional-slots-of d-class))
        (unless (typep slot 'persistent-association-end-slot-definition-d)
          (unioning (dimensions-of slot)))))

(def method compute-primary-class ((slot persistent-effective-slot-definition-d))
  nil)

(def method compute-table ((slot persistent-effective-slot-definition-d))
  nil)

(def method compute-columns ((slot persistent-effective-slot-definition-d))
  nil)

(def method compute-data-table-slot-p ((slot persistent-effective-slot-definition-d))
  nil)

;;;;;;
;;; Utility

(def function d-class-name->h-class-name (d-class-name)
  (bind ((name (symbol-name d-class-name))
         (base-name (if (ends-with-subseq "-d" (string-downcase name))
                        (but-last-elt name 2)
                        d-class-name)))
    (concatenate-symbol (symbol-package d-class-name) base-name "-h")))

(def function h-class-name->d-class-name (h-class-name)
  (bind ((name (symbol-name h-class-name)))
    (intern (but-last-elt name 2) (symbol-package h-class-name))))

(def method export-to-rdbms ((class persistent-class-d))
  (call-next-method)
  (export-to-rdbms (h-class-of class)))
