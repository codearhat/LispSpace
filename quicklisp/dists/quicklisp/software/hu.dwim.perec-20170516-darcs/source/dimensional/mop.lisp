;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; MOP methods 

(def method direct-slot-definition-class ((class persistent-class-d)
                                         &key instance persistent association dimensions &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and persistent
              (not association)
              dimensions)
         (find-class 'persistent-direct-slot-definition-d))
        (t
         (call-next-method))))

(def method direct-slot-definition-class ((class persistent-association-d) &key dimensions &allow-other-keys)
  (if dimensions
      (find-class 'persistent-association-end-direct-slot-definition-d)
      (call-next-method)))

(def method effective-slot-definition-class ((class persistent-class-d)
                                            &key instance persistent association dimensions &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and persistent
              (not association)
              dimensions)
         (find-class 'persistent-effective-slot-definition-d))
        (t
         (call-next-method))))

(def method effective-slot-definition-class ((class persistent-association-d) &key dimensions &allow-other-keys)
  (if dimensions
      (find-class 'persistent-association-end-effective-slot-definition-d)
      (call-next-method)))

(def method compute-persistent-effective-slot-definition-option ((class persistent-class-d) (direct-slot persistent-direct-slot-definition-d)
                                                                 slot-option-name
                                                                 direct-slot-definitions)
  (if (eq slot-option-name 'dimensions)
      (list :dimensions (mapcar 'lookup-dimension (merge-dimensions (mapcar #'dimensions-of direct-slot-definitions))))
      (call-next-method)))

(def method compute-persistent-effective-slot-definition-option ((class persistent-class) (direct-slot persistent-association-end-direct-slot-definition-d)
                                                                 slot-option-name
                                                                 direct-slot-definitions)
  (if (eq slot-option-name 'dimensions)
      (list :dimensions (mapcar 'lookup-dimension (merge-dimensions (mapcar #'dimensions-of direct-slot-definitions))))
      (call-next-method)))

(def method initialize-instance :after ((instance persistent-effective-slot-definition-d) &key &allow-other-keys)
  (assert (dimensions-of instance)))

(def method persistent-class-default-superclasses ((class persistent-class-d) &key direct-superclasses &allow-other-keys)
  (unless (find-if (lambda (direct-superclass)
                     (ignore-errors (subtypep direct-superclass (find-class 'persistent-object-d))))
                   direct-superclasses)
    (list (find-class 'persistent-object-d))))

(def function merge-dimensions (dimensions-list)
  (bind ((dimensions (first dimensions-list)))
    (assert (every [or (null !1) (equal dimensions !1)] dimensions-list)
            nil "Dimensions cannot be overridden. Received: ~S" dimensions-list)
    dimensions))

(def method persistent-class-default-superclasses ((class persistent-class-h) &key name direct-superclasses &allow-other-keys)
  (bind ((d-class-name (h-class-name->d-class-name name))
         (d-class (find-class d-class-name)))
    (append (mapcar (lambda (d-class)
                      (find-class (d-class-name->h-class-name (class-name d-class))))
                    (remove-if-not (of-type 'persistent-class-d)
                                   (class-direct-superclasses d-class)))
            (iter outer
                  (for slot :in (collect-if [typep !1 'persistent-effective-slot-definition-d] (class-slots d-class)))
                  (iter (for dimension :in (dimensions-of slot))
                        (in outer (adjoining (find-class (dependent-object-name-of dimension))))))
            (unless (find-if (lambda (direct-superclass)
                               (ignore-errors (subtypep direct-superclass (find-class 'persistent-object-h))))
                             direct-superclasses)
              (list (find-class 'persistent-object-h))))))
