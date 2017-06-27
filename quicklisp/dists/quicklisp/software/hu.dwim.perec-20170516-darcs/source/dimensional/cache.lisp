;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(defgeneric update-cache (class instance slot action new-value)
  (:documentation "TODO"))

(defmethod update-cache :around ((class persistent-class)
                                 (instance persistent-object)
                                 (slot persistent-effective-slot-definition-d)
                                 action
                                 new-d-value)
     
  (when (or (not (persistent-p instance))
            (and *cache-slot-values* (cache-p slot)))
    (call-next-method)))

(defmethod update-cache ((class persistent-class-d)
                         (instance persistent-object)
                         (slot persistent-effective-slot-definition-d)
                         (action (eql :get))
                         new-d-value)
  ;; TODO inheriting-dimensions ok?
  (update-cached-value instance slot new-d-value))

(defmethod update-cache ((class persistent-class-d)
                         (instance persistent-object)
                         (slot persistent-effective-slot-definition-d)
                         (action (eql :set))
                         new-d-value)

  (if (inheriting-dimension-index-of slot)
      ;; TODO update the cache (needs selects) instead of overwriting it
      (setf (underlying-slot-value-using-class class instance slot) new-d-value)
      (update-cached-value instance slot new-d-value)))

(defmethod update-cache ((class persistent-class)
                         (instance persistent-object)
                         (slot persistent-association-end-effective-slot-definition-d)
                         (action (eql :get))
                         new-d-value)
  ;; TODO inheriting-dimensions ok?
  (update-cached-value instance slot new-d-value))

(defmethod update-cache ((class persistent-class)
                         (instance persistent-object)
                         (association-end persistent-association-end-effective-slot-definition-d)
                         (action (eql :set))
                         new-d-value)
  
  ;; TODO handle inheriting-dimensions specially
  
  (bind ((association (association-of association-end))
         (other-association-end-name (slot-definition-name (other-association-end-of association-end)))
         ((:values cached-p old-d-value) (slot-value-cached-p instance association-end)))

    (setf cached-p (and cached-p (d-value-p old-d-value)))

    (ecase (association-kind-of association)

      (:1-1
       ;; 1. clear the cache of the old associated instance
       ;; 2. clear the cache of the instance associated with the new value
       ;; 3. set this instance in the cache of the new value
       ;; 4. set new value in the cache of this instance
       (iter (for (coordinates new-value) :in-d-value new-d-value)
             (when cached-p
               (iter (for (old-coordinates old-value)
                          :in-d-value (value-at-coordinates old-d-value coordinates))
                     (when (persistent-object-p old-value)
                       (clear-from-association-end-cache
                        old-value other-association-end-name instance old-coordinates))))
             (when (persistent-object-p new-value)
               (clear-from-other-association-end-caches new-value other-association-end-name coordinates)
               (set-in-association-end-cache new-value other-association-end-name instance coordinates))
             (update-cached-value instance association-end new-d-value)))

      (:1-n
       (ecase (cardinality-kind-of association-end)

         (:1 ;; set parent of a child
          ;; 1. remove the child from the cache of its old parent
          ;; 2. add the child to the cache of its new parent
          ;; 3. set new parent in the cache of this instance
          (iter (for (coordinates new-parent) :in-d-value new-d-value)
                (when cached-p
                  (iter (for (old-coordinates old-parent)
                             :in-d-value (value-at-coordinates old-d-value coordinates))
                        (when (persistent-object-p old-parent)
                          (remove-from-association-end-cache
                           old-parent other-association-end-name instance old-coordinates))))
                (when (persistent-object-p new-parent)
                  (insert-into-association-end-cache
                   new-parent other-association-end-name instance coordinates))
                (update-cached-value instance association-end new-d-value)))

         (:n ;; set children of parent
          ;; 1. clear parent of cached children
          ;; 2. remove new children from the cache of their old parents
          ;; 3. set parent to this instance in the cache of  new children
          ;; 4. set new children in the cache of this instance
          (iter (for (coordinates new-children) :in-d-value new-d-value)
                (when cached-p
                  ;; clear parent of old children
                  (iter (for (old-coordinates old-children)
                             :in-d-value (value-at-coordinates old-d-value coordinates))
                        (dolist (old-child old-children)
                          (clear-from-association-end-cache
                           old-child other-association-end-name instance old-coordinates))))
                (dolist (new-child new-children)
                  ;; remove new child from the cache of its old parents
                  (remove-from-other-association-end-caches
                   new-child other-association-end-name coordinates)
                  ;; set parent to this instance in the cache of  new children
                  (set-in-association-end-cache
                   new-child other-association-end-name instance coordinates))
                (update-cached-value instance association-end new-d-value)))))

      (:m-n
       ;; 1. remove this instance from the cache of its old associated instances
       ;; 2. add this instance the the cache of its new associated instances
       ;; 3. set new associated instances in the cache of this instance
       (iter (for (coordinates new-value) :in-d-value new-d-value)
             (when cached-p
               (iter (for (old-coordinates old-value)
                          :in-d-value (value-at-coordinates old-d-value coordinates))
                     (dolist (other-instance old-value)
                       (remove-from-association-end-cache
                        other-instance other-association-end-name instance old-coordinates))))
             (dolist (other-instance new-value)
               (insert-into-association-end-cache
                other-instance other-association-end-name instance coordinates))
             (update-cached-value instance association-end new-d-value))))))

(defmethod update-cache ((class persistent-class)
                         (instance persistent-object)
                         (association-end persistent-association-end-effective-slot-definition-d)
                         (action (eql :insert))
                         new-d-value)
  
  (bind ((association (association-of association-end))
         (association-end-name (slot-definition-name association-end))
         (other-association-end-name (slot-definition-name (other-association-end-of association-end))))

    (ecase (association-kind-of association)
      (:1-1
       (assert nil () "Inserting into 1-ary association-end?"))

      (:1-n
       (ecase (cardinality-kind-of association-end)

         (:1
          (assert nil () "Inserting into 1-ary association-end?"))

         (:n
          (iter (for (coordinates new-child) :in-d-value new-d-value)
                ;; remove new child from the cache of its old parents
                (remove-from-other-association-end-caches
                 new-child other-association-end-name coordinates)
                ;; set parent to this instance in the cache of  new child
                (set-in-association-end-cache
                 new-child other-association-end-name instance coordinates)
                (insert-into-association-end-cache
                 instance association-end-name new-child coordinates)))))

      (:m-n
       (iter (for (coordinates new-value) :in-d-value new-d-value)
             ;; add this instance the the cache of the new associated instance
             (insert-into-association-end-cache
              new-value other-association-end-name instance coordinates)
             (insert-into-association-end-cache
              instance association-end-name new-value coordinates))))))

(defmethod update-cache ((class persistent-class)
                         (instance persistent-object)
                         (association-end persistent-association-end-effective-slot-definition-d)
                         (action (eql :delete))
                         new-d-value)
  
  (bind ((association (association-of association-end))
         (association-end-name (slot-definition-name association-end))
         (other-association-end-name (slot-definition-name (other-association-end-of association-end))))

    (ecase (association-kind-of association)
      (:1-1
       (assert nil () "Removing from 1-ary association-end?"))

      (:1-n
       (ecase (cardinality-kind-of association-end)

         (:1
          (assert nil () "Removing from 1-ary association-end?"))

         (:n
          (iter (for (coordinates child) :in-d-value new-d-value)
                ;; clear parent from the cache of the removed child
                (clear-from-association-end-cache
                 child other-association-end-name instance coordinates)
                ;; remove child from the cache of this instance
                (remove-from-association-end-cache
                 instance association-end-name child coordinates)))))

      (:m-n
       (iter (for (coordinates value) :in-d-value new-d-value)
             ;; remove this instance from the the cache of the removed associated instance
             (remove-from-association-end-cache
              value other-association-end-name instance coordinates)
             ;; remove value from the cache of this instance
             (remove-from-association-end-cache
              instance association-end-name value coordinates))))))

(def function update-cached-value (instance slot new-d-value)
  (bind (((:values cached-p cached-d-value) (slot-value-cached-p instance slot)))
    (assert (or (not cached-p) (unbound-slot-marker-p cached-d-value) (d-value-p cached-d-value)))
    (if (and cached-p (d-value-p cached-d-value))
        (setf (into-d-value cached-d-value) new-d-value) 
        (setf (underlying-slot-value-using-class (class-of instance) instance slot) new-d-value))))

(def function clear-from-other-association-end-caches (instance association-end-name coordinates)
  (bind ((association-end (find-slot (class-of instance) association-end-name))
         (other-association-end-name (slot-definition-name (other-association-end-of association-end)))
         ((:values cached-p cached-d-value) (slot-value-cached-p instance association-end)))
    (when (and cached-p (d-value-p cached-d-value))
      (iter (for (coordinates-in-old old-value) :in-d-value (value-at-coordinates cached-d-value coordinates))
            (when (persistent-object-p old-value)
              (clear-from-association-end-cache old-value other-association-end-name instance coordinates-in-old))))))

(def function remove-from-other-association-end-caches (instance association-end-name coordinates)
  (bind ((association-end (find-slot (class-of instance) association-end-name))
         (other-association-end-name (slot-definition-name (other-association-end-of association-end)))
         ((:values cached-p cached-d-value) (slot-value-cached-p instance association-end)))
    (when (and cached-p (d-value-p cached-d-value))
      (iter (for (coordinates-in-old old-value) :in-d-value (value-at-coordinates cached-d-value coordinates))
            (when (persistent-object-p old-value)
              (remove-from-association-end-cache old-value other-association-end-name instance coordinates-in-old))))))

(def function set-in-association-end-cache (instance association-end-name value coordinates)
  (bind ((association-end (find-slot (class-of instance) association-end-name))
         ((:values cached-p cached-d-value) (slot-value-cached-p instance association-end)))
    (when (and cached-p (d-value-p cached-d-value))
      (setf (value-at-coordinates cached-d-value coordinates) value))))

(def function clear-from-association-end-cache (instance association-end-name value coordinates)
  (bind ((association-end (find-slot (class-of instance) association-end-name))
         ((:values cached-p cached-d-value) (slot-value-cached-p instance association-end)))
    (when (and cached-p (d-value-p cached-d-value))
      (clear-at-coordinates cached-d-value coordinates value))))

(def function remove-from-association-end-cache (instance association-end-name value coordinates)
  (bind ((association-end (find-slot (class-of instance) association-end-name))
         ((:values cached-p cached-d-value) (slot-value-cached-p instance association-end)))
    (when (and cached-p (d-value-p cached-d-value))
      (delete-at-coordinates cached-d-value coordinates value))))

(def function insert-into-association-end-cache (instance association-end-name value coordinates)
  (bind ((association-end (find-slot (class-of instance) association-end-name))
         ((:values cached-p cached-d-value) (slot-value-cached-p instance association-end)))
    (when (and cached-p (d-value-p cached-d-value))
      (insert-at-coordinates cached-d-value coordinates value))))
