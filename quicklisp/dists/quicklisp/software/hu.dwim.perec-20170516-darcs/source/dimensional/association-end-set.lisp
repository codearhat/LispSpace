;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;; Store interface

(def generic insert-into-association-end-set-d (instance d-association-end item &key coordinates)
  (:documentation "TODO"))

(def generic delete-from-association-end-set-d (instance d-association-end item &key coordinates)
  (:documentation "TODO"))

;;;;;;
;;; d-s lazy association end set containers

(def class* persistent-association-end-set-container-d (persistent-association-end-set-container)
  ())

(def method check-insert-item ((set persistent-association-end-set-container-d) item)
  nil)

(def method insert-item ((set persistent-association-end-set-container-d) item)
  (bind ((instance (instance-of set))
         (persistent-instance? (persistent-p instance))
         (persistent-item? (persistent-p item)))
    (assert-instance-access instance persistent-instance?)
    (assert-instance-access item persistent-item?)
    (bind ((slot (slot-of set))
           (dimensions (dimensions-of slot))
           (coordinates (collect-coordinates-from-variables dimensions))
           (d-value (make-single-d-value dimensions coordinates item)))
      (insert-into-association-end-set-d instance slot item :coordinates coordinates)
      (update-cache (class-of instance) instance slot :insert d-value))))

(def method check-delete-item ((set persistent-association-end-set-container-d) item)
  nil)

(def method delete-item ((set persistent-association-end-set-container-d) item)
  (bind ((instance (instance-of set))
         (persistent? (persistent-p instance))
         (slot (slot-of set))
         (dimensions (dimensions-of slot))
         (coordinates (collect-coordinates-from-variables dimensions))
         (d-value (make-single-d-value dimensions coordinates item)))
    (assert-instance-access instance persistent?)
    (delete-from-association-end-set-d instance slot item :coordinates coordinates)
    (update-cache (class-of instance) instance slot :delete d-value)))

(def method find-item ((set persistent-association-end-set-container-d) (item persistent-object))
  ;; TODO optimize
  (bind ((instance (instance-of set))
         (persistent? (persistent-p instance)))
    (assert-instance-access instance persistent?)
    (bind ((dimensions (dimensions-of (slot-of set)))
           (d-value (bind ((*lazy-slot-value-collections* #f))
                      (restore-slot (class-of instance) instance (slot-of set)
                                    :coordinates (collect-coordinates-from-variables dimensions)))))
      (iter (for (coordinates set) :in-d-value d-value)
            (collect-d-value (find item set :test #'p-eq) :dimensions dimensions :coordinates coordinates)))))

(def method ensure-item ((set persistent-association-end-set-container-d) (item persistent-object))
  ;; TODO optimize
  (bind ((instance (instance-of set))
         (persistent-instance? (persistent-p instance))
         (persistent-item? (persistent-p item)))
    (assert-instance-access instance persistent-instance?)
    (assert-instance-access item persistent-item?)
    (bind ((association-end (slot-of set))
           (dimensions (dimensions-of association-end))
           (d-value (bind ((*lazy-slot-value-collections* #f))
                      (restore-slot (class-of instance) instance association-end
                                    :coordinates (collect-coordinates-from-variables dimensions)))))
      (iter (for (coordinates set) :in-d-value d-value)
            (unless (find item set :test #'p-eq)
              (insert-into-association-end-set-d instance association-end item :coordinates coordinates))))))

(def method size ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method empty-p ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method empty! ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method list-of ((set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method (setf list-of) (new-value (set persistent-association-end-set-container-d))
  (not-yet-implemented))

(def method iterate-items ((set persistent-association-end-set-container-d) fn)
  (not-yet-implemented))
