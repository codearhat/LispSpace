;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;
;;;
(def (function e) ensure-cached-tree-d (bulk d-class-name d-association-name)
  (or (cached-bulk-of bulk)
      (setf (cached-bulk-of bulk)
            (bind ((d-class (find-class d-class-name))
                   (d-association (find-persistent-association d-association-name))
                   (parent-association-end (primary-association-end-of d-association))
                   (instances (select (instance)
                                (from instance)
                                (where (typep instance d-class)))))
              (assert (eq :1-n (association-kind-of d-association)))
              (cache-dimensional-slots d-class)
              (cache-dimensional-association d-association)
              (find-root (first instances) d-class parent-association-end)))))

(def function find-root (instance class parent-association-end)
  (declare (ignore class))
  (when instance
    (iter (for current :initially instance :then parent)
          (for parent = (single-d-value (slot-value current (slot-definition-name parent-association-end))))
          (while parent)
          (finally (return current)))))


;;;;;;
;;; Association end cache

(def function cache-to-many-association-ends-d (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class))
           (dimensions (dimensions-of children-slot))
           (coordinates (collect-coordinates-from-variables dimensions)))
      (when (null (underlying-slot-boundp-or-value-using-class class instance children-slot))
        (setf (underlying-slot-boundp-or-value-using-class class instance children-slot)
              (make-single-d-value dimensions coordinates nil)))
      (when-bind parent (funcall parent-accessor instance)
       (bind ((parent-class (class-of parent))
              (parent-children-slot (funcall children-slot-provider parent-class))
              ((:values cached-p cached-children)
               (slot-value-cached-p parent parent-children-slot)))
         (assert cached-p)
         (setf (underlying-slot-boundp-or-value-using-class parent-class parent parent-children-slot)
               (make-single-d-value
                dimensions
                coordinates
                (cons instance (single-d-value cached-children)))))))))

(def function cache-to-many-association-ends-for-tree-d (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class)))
      (setf (underlying-slot-boundp-or-value-using-class class instance children-slot) nil)))
  (cache-to-many-association-ends-d instances children-slot-provider parent-accessor)
  (find-tree-root (first instances) parent-accessor))

(def macro ensure-cached-to-many-association-ends-for-tree-d (bulk instances children-slot-provider parent-accessor)
  `(aif (cached-bulk-of ,bulk)
    it
    (setf (cached-bulk-of ,bulk)
     (cache-to-many-association-ends-for-tree-d ,instances ,children-slot-provider ,parent-accessor))))

(def (function e) cache-dimensional-association (d-association)
  (bind ((dimensions (dimensions-of d-association))
         (coordinates (collect-coordinates-from-variables dimensions))
         (h-instances (select-h-instances-of-d-association d-association dimensions coordinates)))
    (clear-association-end-caches h-instances d-association)
    (set-cached-association-ends-from-h-instances h-instances d-association dimensions coordinates)))

(def function clear-association-end-caches (h-instances d-association)
  (iter (with dimensions = (dimensions-of d-association))
        (with (primary-association-end secondary-association-end) =
              (persistent-effective-association-ends-of d-association))
        (with primary-slot-name = (slot-definition-name primary-association-end))
        (with secondary-slot-name = (slot-definition-name secondary-association-end))
        (for h-instance :in h-instances)
        (for primary-d-instance = (slot-value h-instance (h-slot-name-of primary-association-end)))
        (for secondary-d-instance = (slot-value h-instance (h-slot-name-of secondary-association-end)))
        (when primary-d-instance
          (setf (underlying-slot-value primary-d-instance primary-slot-name) (make-empty-d-value dimensions)))
        (when secondary-d-instance
          (setf (underlying-slot-value secondary-d-instance secondary-slot-name) (make-empty-d-value dimensions)))))

(def function set-cached-association-ends-from-h-instances (h-instances d-association dimensions coordinates)
  (iter (for h-instance :in h-instances)
        (update-cached-association-ends-from-h-instance h-instance d-association dimensions coordinates)))

(def function update-cached-association-ends-from-h-instance (h-instance d-association dimensions coordinates)
  (bind ((association-kind (association-kind-of d-association))
         ((primary-association-end secondary-association-end)
          (persistent-effective-association-ends-of d-association))
         (primary-slot-name (slot-definition-name primary-association-end))
         (secondary-slot-name (slot-definition-name secondary-association-end))
         (primary-h-slot-name (h-slot-name-of primary-association-end))
         (secondary-h-slot-name (h-slot-name-of secondary-association-end))
         (coordinates (coordinates-intersection
                       dimensions
                       coordinates
                       (get-coordinates-from-h-instance h-instance dimensions)))
         (action (unless (eq association-kind :1-1) (action-of h-instance)))
         (primary-instance (slot-value h-instance primary-h-slot-name))
         (secondary-instance (slot-value h-instance secondary-h-slot-name))
         (primary-cached-value (when primary-instance
                                 (underlying-slot-value primary-instance primary-slot-name)))
         (secondary-cached-value (when secondary-instance
                                   (underlying-slot-value secondary-instance secondary-slot-name))))
    (assert coordinates)
    (assert (or (null primary-cached-value) (d-value-p primary-cached-value)))
    (assert (or (null secondary-cached-value) (d-value-p secondary-cached-value)))

    (ecase association-kind
      (:1-1
       (assert (or primary-instance secondary-instance))
       (when primary-instance
         (iter (for (coordinates secondary-instance)
                    :in-d-value (value-at-coordinates primary-cached-value coordinates))
               (when secondary-instance
                 (bind ((secondary-cached-value (underlying-slot-value secondary-instance
                                                                       secondary-slot-name)))
                   (setf (value-at-coordinates secondary-cached-value coordinates) nil))))
         (setf (value-at-coordinates primary-cached-value coordinates) secondary-instance))
       (when secondary-instance
         (iter (for (coordinates primary-instance)
                    :in-d-value (value-at-coordinates secondary-cached-value coordinates))
               (when primary-instance
                 (bind ((primary-cached-value (underlying-slot-value primary-instance
                                                                     primary-slot-name)))
                   (setf (value-at-coordinates primary-cached-value coordinates) nil))))
         (setf (value-at-coordinates secondary-cached-value coordinates) primary-instance)))
      (:1-n
       (bind ((child primary-instance)
              (parent-slot-name primary-slot-name)
              (cached-parent primary-cached-value)
              (parent secondary-instance)
              (children-slot-name secondary-slot-name)
              (cached-children secondary-cached-value))
         (unless (eq :1 (cardinality-kind-of primary-association-end))
             (swap child parent)
             (swap parent-slot-name children-slot-name)
             (swap cached-parent cached-children))
         (ecase action
           (#.+t-clear+
            (assert (or parent child))  ; TODO xor
            (when parent                ; clear children
              (iter (for (coordinates children)
                         :in-d-value (value-at-coordinates cached-children coordinates))
                    (iter (for child :in children)
                          (for cached-parent = (underlying-slot-value child parent-slot-name))
                          (setf (value-at-coordinates cached-parent coordinates) nil)))
              (setf (value-at-coordinates cached-children coordinates) nil))
            (when child                 ; clear parent
              (iter (for (coordinates parent)
                         :in-d-value (value-at-coordinates cached-parent coordinates))
                    (unless parent (next-iteration))
                    (for cached-children = (underlying-slot-value parent children-slot-name))
                    (delete-at-coordinates cached-children coordinates child))
              (setf (value-at-coordinates cached-parent coordinates) nil))
            )
           (#.+t-delete+
            (assert (and parent child))
            (delete-at-coordinates cached-children coordinates child)
            (setf (value-at-coordinates cached-parent coordinates) nil) ;; FIXME clear-at-coordinates
            )
           (#.+t-insert+
            (assert (and parent child))
            (insert-at-coordinates cached-children coordinates child)
            (setf (value-at-coordinates cached-parent coordinates) parent)))))
      (:m-n
       (ecase action
         (#.+t-clear+
          (assert (or primary-instance secondary-instance)) ; TODO xor
          (when primary-instance
            (iter (for (coordinates secondary-instances)
                       :in-d-value (value-at-coordinates primary-cached-value coordinates))
                  (iter (for secondary-instance :in secondary-instances)
                        (for secondary-cached-value = (underlying-slot-value secondary-instance
                                                                             secondary-slot-name))
                        (delete-at-coordinates secondary-cached-value coordinates primary-instance)))
            (setf (value-at-coordinates primary-cached-value coordinates) nil))
          (when secondary-instance
            (iter (for (coordinates primary-instances)
                       :in-d-value (value-at-coordinates secondary-cached-value coordinates))
                  (iter (for primary-instance :in primary-instances)
                        (for primary-cached-value = (underlying-slot-value primary-instance
                                                                           primary-slot-name))
                        (delete-at-coordinates primary-cached-value coordinates secondary-instance)))
            (setf (value-at-coordinates secondary-cached-value coordinates) nil)))
         (#.+t-delete+
          (assert (and primary-instance secondary-instance))
          (delete-at-coordinates primary-cached-value coordinates secondary-instance)
          (delete-at-coordinates secondary-cached-value coordinates primary-instance))
         (#.+t-insert+
          (assert (and primary-instance secondary-instance))
          (insert-at-coordinates primary-cached-value coordinates secondary-instance)
          (insert-at-coordinates secondary-cached-value coordinates primary-instance)))))))

;;;;;;
;;; Cache multiple slot values

(def (function e) cache-dimensional-slots (d-class)
  "Cache the dimensional slots of each instances of D-CLASS using one select."
  (bind ((dimensions (slot-dimensions-of d-class))
         (coordinates (collect-coordinates-from-variables dimensions))
         (h-instances (select-h-instances-of-d-class d-class dimensions coordinates)))
    (set-cached-slot-values-from-h-instances h-instances)))

(def function set-cached-slot-values-from-h-instances (h-instances)
  (iter (for h-instance :in h-instances)
        (for d-instance = (d-instance-of h-instance))
        #| TODO
        (for inheriting-coordinate = (when inheriting-dimension
                                       (coordinate-range-begin
                                        (get-coordinate-from-h-instance h-instance inheriting-dimension))))
        (for prev-inheriting-coordinate :previous inheriting-coordinate)
        ;; check that h-instances are ordered according to the inheriting dimension
        (assert (or (null inheriting-coordinate)
                    (and inheriting-coordinate (coordinate<= prev-inheriting-coordinate
                                                             inheriting-coordinate))))
        |#
        (iter (for slot :in (dimensional-slots-of (class-of d-instance)))
              (unless (typep slot 'persistent-association-end-slot-definition)
                (set-cached-slot-value-from-h-instance d-instance h-instance (slot-definition-name slot))))))

(def function set-cached-slot-value-from-h-instance (d-instance h-instance slot-name)
  (bind ((slot-value (if (slot-boundp h-instance slot-name) (slot-value h-instance slot-name) +unbound-slot-marker+)))
    (unless (eq slot-value +h-unused-slot-marker+)
      (bind ((d-class (class-of d-instance))
             (d-slot (find-slot d-class slot-name))
             (dimensions (dimensions-of d-slot))
             (coordinates (coordinates-intersection
                           dimensions
                           (get-coordinates-from-h-instance h-instance dimensions)
                           (collect-coordinates-from-variables dimensions)))
             ((:values cached-p cached-slot-value) (slot-value-cached-p d-instance d-slot)))
        (if cached-p
            (progn
              (assert (d-value-p cached-slot-value))
              (setf (value-at-coordinates cached-slot-value coordinates) slot-value))
            (setf (underlying-slot-boundp-or-value-using-class d-class d-instance d-slot)
                  (make-single-d-value dimensions coordinates slot-value)))))))

(def (function io) get-coordinates-from-h-instance (h-instance dimensions)
  (mapcar [get-coordinate-from-h-instance h-instance !1] dimensions))

(def function get-coordinate-from-h-instance (h-instance dimension)
  (etypecase dimension
    (inheriting-dimension
     (make-ii-coordinate-range
      (slot-value h-instance (slot-name-of dimension))
      (maximum-coordinate-of dimension)))
    (ordering-dimension
     (make-ie-coordinate-range
      (slot-value h-instance (begin-slot-name-of dimension))
      (slot-value h-instance (end-slot-name-of dimension))))
    (dimension
     (bind ((value (slot-value h-instance (slot-name-of dimension))))
       (if value
           (list value)
           +whole-domain-marker+)))))

;;;
;;; Queries
;;;

(def function select-h-instances-of-d-class (d-class dimensions coordinates)
  (bind ((h-class-name (class-name (h-class-of d-class)))
         (query (make-query `(select (h-instance)
                               (from (h-instance ,h-class-name))
                               (order-by :ascending (oid-of h-instance))))))

    (add-asserts-for-coordinates query dimensions coordinates)
    (apply #'execute-query query coordinates)))

(def function select-h-instances-of-d-association (d-association dimensions coordinates)
  (bind ((h-class-name (class-name (h-class-of d-association)))
         (query (make-query `(select (h-instance)
                               (from (h-instance ,h-class-name))
                               (order-by :ascending (oid-of h-instance))))))

    (add-asserts-for-coordinates query dimensions coordinates)
    (apply #'execute-query query coordinates)))

(def function add-asserts-for-coordinates (query dimensions coordinates)
  (iter (for dimension :in dimensions)
        (for coordinate :in coordinates)
        (for interval-p = (when (typep dimension 'ordering-dimension)
                            (not (coordinate-range-empty-p coordinate))))
        (add-lexical-variable query (name-of dimension))

        (etypecase dimension
          (inheriting-dimension
             
           (add-assert query
                       `(,(if interval-p
                              (dimension-less dimension)
                              (dimension-less-or-equal dimension))
                          (slot-value h-instance ',(name-of dimension))
                          (coordinate-range-end ,(name-of dimension))))

           (setf (order-by-of query)
                 `(,(direction-of dimension) (slot-value h-instance ',(slot-name-of dimension))
                    ,(direction-of dimension) (oid-of h-instance))))

          (ordering-dimension
           (add-assert query
                       `(,(if interval-p (dimension-less dimension) (dimension-less-or-equal dimension))
                          (slot-value h-instance ',(begin-slot-name-of dimension))
                          (coordinate-range-end ,(name-of dimension))))
           (add-assert query
                       `(,(dimension-less dimension)
                          (coordinate-range-begin ,(name-of dimension))
                          (slot-value h-instance ',(end-slot-name-of dimension)))))

          (dimension
           (unless (whole-domain-marker-p coordinate)
             (add-assert query
                         `(or (null (slot-value h-instance ',(slot-name-of dimension)))
                              (member
                               (slot-value h-instance ',(slot-name-of dimension))
                               ,(name-of dimension)))))))))

;;;
;;; Helpers
;;;
(def function persistent-effective-association-ends-of (association)
  (bind ((primary-direct-association-end (primary-association-end-of association))
         (secondary-direct-association-end (secondary-association-end-of association))
         (primary-class (persistent-slot-definition-class primary-direct-association-end))
         (secondary-class (persistent-slot-definition-class secondary-direct-association-end))
         (primary-effective-association-end
          (find-slot primary-class (slot-definition-name primary-direct-association-end)))
         (secondary-effective-association-end
          (find-slot secondary-class (slot-definition-name secondary-direct-association-end))))
    (assert (and primary-effective-association-end secondary-effective-association-end))
    (list primary-effective-association-end secondary-effective-association-end)))