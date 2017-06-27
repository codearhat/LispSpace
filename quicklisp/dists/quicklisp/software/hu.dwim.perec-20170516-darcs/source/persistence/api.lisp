;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; defpclass

(def generic expand-defpclass-form (metaclass defclass-macro name superclasses slots options)
  (:method ((metaclass null) defclass-macro name superclasses slots options)
    (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
           (metaclass (or specified-metaclass 'persistent-class)))
      (expand-defpclass-form (class-prototype (ensure-finalized (find-class metaclass)))
                             defclass-macro name superclasses slots
                             (if specified-metaclass
                                 options
                                 (append options `((:metaclass ,metaclass))))))))

(def macro defpclass (name superclasses slots &rest options)
  "Defines a persistent class. Slots may have an additional :persistent slot option which is true by default. For standard options see defclass."
  (expand-defpclass-form nil 'defclass name superclasses slots options))

(def macro defpclass* (name superclasses slots &rest options)
  "Same as defpclass but uses defclass*."
  (expand-defpclass-form nil 'defclass* name superclasses slots options))

(def (definer e) persistent-class (name superclasses slots &rest options)
  `(defpclass ,name ,superclasses ,slots ,@options))

(def (definer e) persistent-class* (name superclasses slots &rest options)
  `(defpclass* ,name ,superclasses ,slots ,@options))

;;;;;;
;;; defpassociation

(def generic expand-defpassociation-form (metaclass association-ends options)
  (:method ((metaclass null) association-ends options)
    (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
           (metaclass (or specified-metaclass 'persistent-association)))
      (expand-defpassociation-form (class-prototype (ensure-finalized (find-class metaclass)))
                                   association-ends
                                   (if specified-metaclass
                                       options
                                       (append options `((:metaclass ,metaclass))))))))

(def macro defpassociation (&body association-ends)
  (expand-defpassociation-form nil (car association-ends) (cdr association-ends)))

(def macro defpassociation* (&body association-ends)
  (expand-defpassociation-form nil
                               (mapcar (lambda (entry)
                                         (bind ((slot-name (getf entry :slot)))
                                           (unless slot-name
                                             (error "Illegal association end definition ~S: missing :slot" entry))
                                           (append entry
                                                   (unless (getf entry :accessor)
                                                     `(:accessor ,(default-accessor-name-transformer slot-name nil)))
                                                   (unless (getf entry :initarg)
                                                     `(:initarg ,(default-initarg-name-transformer slot-name nil))))))
                                       (car association-ends))
                               (cdr association-ends)))

(def (definer e) persistent-association (&body association-ends)
  `(defpassociation ,@association-ends))

(def (definer e) persistent-association* (&body association-ends)
  `(defpassociation* ,@association-ends))

;;;;;;
;;; types

;;; see types.lisp

;;;;;;
;;; with-transaction

;;; inherited from hu.dwim.rdbms

;;;;;;
;;; with-database

;;; inherited from hu.dwim.rdbms

;;;;;;
;;; persistence

(def function ensure-persistent (instance)
  (unless (persistent-p instance)
    (make-persistent instance)))

(def function ensure-transient (instance)
  (when (persistent-p instance)
    (make-transient instance)))

(def function make-persistent (instance)
  "Makes an instance persistent without making its associated instances persistent."
  (if (persistent-p instance)
      (error "Instance ~A is already persistent, you may want to use ~S instead" instance 'ensure-persistent)
      (make-persistent-using-class (class-of instance) instance)))

(def function make-transient (instance)
  "Makes an instance transient without making its associated instances transient."
  (if (persistent-p instance)
      (make-transient-using-class (class-of instance) instance)
      (error "Instance ~A is already transient, you may want to use ensure-transient instead" instance)))

(def generic make-persistent-using-class (class instance)
  (:documentation "Extension point"))

(def generic make-transient-using-class (class instance)
  (:documentation "Extension point"))

;;;;;;
;;; collection

;;; insert-item, delete-item, empty-p, empty!, find-item are inherited from cl-containers

(def generic ensure-item (persistent-collection fn)
  (:documentation "Ensure that item is present in the container."))

(def generic iterate-items (persistent-collection fn)
  (:documentation "Applies function to each item in the persistent container."))

(def generic list-of (persistent-collection)
  (:documentation "Returns a non lazy list of items present in the persistent collection."))

(def generic (setf list-of) (new-value persistent-collection)
  (:documentation "Returns a non lazy list of items present in the persistent collection."))

;;;;;;
;;; cache

(def macro with-caching-slot-values (&body forms)
  `(bind ((*cache-slot-values* #t))
     ,@forms))

(def macro without-caching-slot-values (&body forms)
  `(bind ((*cache-slot-values* #f))
     ,@forms))

;;;;;;
;;; laziness

(def macro with-lazy-slot-value-collections (&body forms)
  `(bind ((*lazy-slot-value-collections* #t))
     ,@forms))

(def macro without-lazy-slot-value-collections (&body forms)
  `(bind ((*lazy-slot-value-collections* #f))
     ,@forms))
