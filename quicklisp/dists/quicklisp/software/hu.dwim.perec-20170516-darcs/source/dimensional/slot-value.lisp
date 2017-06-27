;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def (special-variable e) *simplify-d-values* #f)

(def (function io) simplify-d-value (d-value)
  (if (and *simplify-d-values* (single-d-value-p d-value))
      (single-d-value d-value)
      d-value))

(def (condition* e) unbound-slot-d (unbound-slot)
  ((coordinates :type list))
  (:report (lambda (condition stream)
             (format stream "The slot ~S is unbound in the object ~S with coordinates ~S."
                     (cell-error-name condition)
                     (unbound-slot-instance condition)
                     (coordinates-of condition)))))

(def function slot-unbound-d (instance slot coordinates)
  (error 'unbound-slot-d
         :name (slot-definition-name slot)
         :instance instance
         :coordinates coordinates))

(def (function o) collect-coordinates-from-variables (dimensions)
  ;; TODO asserts for read/write
  (iter (for dimension :in dimensions)
        (for coordinate = (coordinate dimension))
        (typecase dimension
          (ordering-dimension
           (collect coordinate))
          (t
           (collect (if (whole-domain-marker-p coordinate)
                        coordinate
                        (mapcar
                         ;; make sure that coordinates are in the current transaction
                         ;; because they might be stored in the cache
                         [if (persistent-object-p !1) (load-instance !1) !1]
                         (ensure-list coordinate))))))))

(def (function io) assert-d-value-instance-access (d-value)
  (declare (ignorable d-value))
  (debug-only
    (iter (with dimensions = (dimensions-of d-value))
          (for (coordinates value) :in-d-value d-value)
          (iter (for dimension :in dimensions)
                (for coordinate :in coordinates)
                (when (and (not (typep dimension 'ordering-dimension))
                           (listp coordinate))
                  (mapc (lambda (coordinate-value)
                          (when (persistent-object-p coordinate-value)
                            (bind ((persistent? (persistent-p coordinate-value)))
                              (assert-instance-access coordinate-value persistent?))))
                        coordinate)))
          (when (persistent-object-p value)
            (bind ((persistent? (persistent-p value)))
              (assert-instance-access value persistent?))))))

(def (function io) slot-boundp-or-value-using-class-d (class instance slot coordinates)
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (cache-p (and *cache-slot-values* (cache-p slot))))
    (assert-instance-access instance persistent)
    (bind (((:values slot-value-cached cached-value) (slot-value-cached-p instance slot)))
      (when (or (not persistent)
                (and cache-p slot-value-cached))
        (if (unbound-slot-marker-p cached-value)
            (return-from slot-boundp-or-value-using-class-d +unbound-slot-marker+)
            (bind ((d-value (value-at-coordinates cached-value coordinates)))
              (when (covering-d-value-p d-value coordinates)
                (return-from slot-boundp-or-value-using-class-d (simplify-d-value d-value))))))
      (if persistent
          (bind ((d-value (restore-slot class instance slot :coordinates coordinates))) ;; FIXME returns ii
            (update-cache class instance slot :get d-value)
            (simplify-d-value d-value))
          (return-from slot-boundp-or-value-using-class-d +unbound-slot-marker+)))))

(def (function io) (setf slot-boundp-or-value-using-class-d) (new-value class instance slot)
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (new-d-value (cond
                        ((d-value-p new-value)
                         (assert-d-value-instance-access new-value)
                         new-value)
                        (t
                         (when (persistent-object-p new-value)
                           (bind ((persistent? (persistent-p new-value)))
                             (assert-instance-access new-value persistent?)))
                         (make-single-d-value (dimensions-of slot)
                                              (collect-coordinates-from-variables (dimensions-of slot))
                                              new-value)))))
    (assert-instance-access instance persistent)
    (when persistent
      (store-slot class instance slot new-d-value))
    (update-cache class instance slot :set new-d-value))
  new-value)

(defmethod slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-d))
  "Reads the slot value from the database or the cache."
  (bind ((coordinates (collect-coordinates-from-variables (dimensions-of slot)))
         (value (slot-boundp-or-value-using-class-d class instance slot coordinates)))
    (flet ((check-value (value coordinates)
             (when (unbound-slot-marker-p value)
               (slot-unbound-d instance slot coordinates))))
      (if (d-value-p value)
          (iter (for c-value :in (c-values-of value))
                (check-value (value-of c-value) (coordinates-of c-value)))
          (check-value value coordinates))
      value)))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-d))
  "Writes the new slot value to the database and the cache."
  (setf (slot-boundp-or-value-using-class-d class instance slot) new-value))

(defmethod slot-boundp-using-class ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition-d))
  "Reads boundness from the database or the cache."
  (bind ((coordinates (collect-coordinates-from-variables (dimensions-of slot)))
         (value (slot-boundp-or-value-using-class-d class instance slot coordinates)))
    (if (d-value-p value)
        (iter (for c-value :in (c-values-of value))
              (always (not (unbound-slot-marker-p (value-of c-value)))))
        (not (unbound-slot-marker-p value)))))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition-d))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class-d class instance slot) +unbound-slot-marker+)
  instance)

(def method update-instance-for-different-class :before ((old-instance persistent-object-d) (new-instance persistent-object-d) &key &allow-other-keys)
  (iter (with class = (class-of new-instance))
        (for slot :in (persistent-effective-slots-of class))
        (when (typep slot 'persistent-effective-slot-definition-d)
          (setf (underlying-slot-boundp-or-value-using-class class new-instance slot) +not-cached-slot-marker+))))

(def method update-instance-for-different-class :after ((old-instance persistent-object-d) (new-instance persistent-object-d) &key &allow-other-keys)
  (dolist (h-instance (h-instances-of old-instance))
    (change-class h-instance (h-class-of (class-of new-instance)))))