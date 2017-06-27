;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;
;;; Invariants:
;;;
;;;   - Only one dimension can be inheriting-dimension.

;;;
;;; Slot access
;;;
(def method restore-slot ((d-class persistent-class-d) (d-instance persistent-object-d) (d-slot persistent-effective-slot-definition-d)
                          &key (coordinates (collect-coordinates-from-variables (dimensions-of d-slot))))
  (bind (((d-slot-default-value . has-default-p) (default-value-for-type-of d-slot))
         (default-value (if has-default-p d-slot-default-value +unbound-slot-marker+))
         (dimensions (dimensions-of d-slot))
         (records (select-slot-values-with-dimensions d-class d-instance d-slot coordinates)))
    (make-d-value-from-records records default-value dimensions coordinates)))

(def method store-slot ((d-class persistent-class-d) (d-instance persistent-object-d) (d-slot persistent-effective-slot-definition-d) value)
  ;; store-all-slots may pass simple values here
  #+nil(assert (d-value-p value))
  (unless (d-value-p value)
    (setf value (make-single-d-value
                 (dimensions-of d-slot)
                 (collect-coordinates-from-variables (dimensions-of d-slot))
                 value)))

  (iter (for (coordinates v) :in-d-value value)
        (check-slot-value-type d-instance d-slot v))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-slot d-instance d-slot)

  (iter (for (coordinates v) :in-d-value value)
        (store-slot-t* d-class d-instance d-slot v coordinates)))

(def method lock-slot ((d-instance persistent-object) (d-slot persistent-effective-slot-definition-d) &key (wait t))
  (bind ((h-slot (h-slot-of d-slot))
         (table (name-of (table-of h-slot))))
    (sql-lock-table :table table
                    :mode :exclusive
                    :wait wait)))

(def (function io) default-value-of-slot-p (slot value)
  (bind (((slot-default-value . has-default-p) (default-value-for-type-of slot)))
    (and has-default-p (equal value slot-default-value)))) ; FIXME equal

(def function store-slot-t* (d-class d-instance d-slot value coordinates)
  (bind ((dimensions (dimensions-of d-slot))
         (update-count))

    ;; do not store the default value of the slot in a transient instance except if temporal.
    ;; restore-slot interprets missing h-instances as the default value
    (when (and (not (persistent-p d-instance))
               (default-value-of-slot-p d-slot value)
               (null (find-if (of-type 'inheriting-dimension) (dimensions-of d-slot))))
      (return-from store-slot-t*))

    (iterate-with-enumerated-coordinate-sets

     (lambda (coordinates)
      ;; first try to update value in the h-instance having the same coordinates
      (setf update-count (update-h-instance-slot-value d-class d-instance d-slot value coordinates))
      ;; if the update is not succeeded then insert value with t and validity except if it
      ;; is the default value of a non-temporal slot
      (when (zerop update-count)
        (insert-h-instance d-class d-instance d-slot value coordinates)))

     dimensions coordinates)))

;;;
;;; d-value builders
;;;
(def function make-d-value-from-records (records default-value dimensions coordinates)
  (iter (for record :in records)
        (for value = (first record))
        (for coords = (coordinates-intersection ;; NOTE: intersect within the iter
                       dimensions               ;; to avoid (domain ...) calls when the record
                       coordinates              ;; contains whole-domain-marker, but the coordinates not
                       (iter (for dimension :in dimensions) ;; TODO refine intersection in inheriting coord
                             (generating coordinate :in (rest record))
                             (collect (etypecase dimension
                                        (inheriting-dimension
                                         (make-coordinate-range
                                          'ii
                                          (next coordinate)
                                          (maximum-coordinate-of dimension)))
                                        (ordering-dimension
                                         (make-coordinate-range
                                          'ie (next coordinate) (next coordinate)))
                                        (dimension
                                         (next coordinate)))))))
        (assert coords)
        (collect value :into value-list)
        (collect coords :into coordinates-list)
        (finally (return (make-d-value dimensions
                                       (list* coordinates coordinates-list)
                                       (list* default-value value-list))))))

;;;
;;; Association-end access
;;;
(def constant +t-clear+ 0
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-delete+ 1
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-insert+ 2
  "Constant used to mark RDBMS records for association slots.")

(def constant +missing-value+ (cons t t))


(def method restore-slot ((d-class persistent-class-d) (d-instance persistent-object-d) (d-association-end persistent-association-end-effective-slot-definition-d)
                          &key (coordinates (collect-coordinates-from-variables (dimensions-of d-association-end))))
  (restore-d-association-end d-instance d-association-end coordinates))

(def method restore-slot ((class persistent-class) (instance persistent-object) (d-association-end persistent-association-end-effective-slot-definition-d)
                          &key (coordinates (collect-coordinates-from-variables (dimensions-of d-association-end))))
  (restore-d-association-end instance d-association-end coordinates))

(def function restore-d-association-end (instance d-association-end coordinates)
  (bind ((dimensions (dimensions-of d-association-end))
         (default-value (if (null-subtype-p (canonical-type-of d-association-end))
                            nil
                            +unbound-slot-marker+))
         (association (association-of d-association-end)))
    (labels ((unchecked-value (d-association-end instance coordinates)
               (bind ((records (select-association-end-values-with-dimensions instance d-association-end coordinates)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (make-d-value-from-records records default-value dimensions coordinates))
                   ((:1-n :m-n)
                    (make-d-value-from-association-records records default-value
                                                           (cardinality-kind-of d-association-end)
                                                           dimensions coordinates)))))

             (check-result (instance result)
               (bind ((other-association-end (other-association-end-of d-association-end)))
                 (iter outer
                       (for (coords value) :in-d-value result)
                       (cond
                         ((or (null value) (eq value +unbound-slot-marker+))
                          (collect-d-value value :dimensions dimensions :coordinates coords))
                         ((persistent-object-p value)
                          (iter (for (other-coords other-value) :in-d-value
                                     (unchecked-value other-association-end value coords))
                                (if (find instance (ensure-list other-value))
                                    (in outer (collect-d-value value :dimensions dimensions :coordinates other-coords))
                                    (in outer (collect-d-value default-value
                                                               :dimensions dimensions :coordinates other-coords)))))
                         ((listp value)
                          (bind ((d-values (mapcar [unchecked-value other-association-end !1 coords] value)))
                            (iter (for (other-coords values) :in-d-values d-values :unspecified-value +missing-value+)
                                  (in outer (collect-d-value
                                             (iter (for vv :in value)
                                                   (for v :in-sequence values)
                                                   (when (and (not (eq v +missing-value+))
                                                              (find instance (ensure-list v)))
                                                     (adjoining vv)))
                                             :dimensions dimensions :coordinates other-coords)))))
                         (t
                          (error "Bug")))))))

      (if (and *lazy-slot-value-collections*
               (eq (cardinality-kind-of d-association-end) :n))
          (make-instance 'persistent-association-end-set-container-d
                         :instance instance :slot d-association-end)
          (check-result instance (unchecked-value d-association-end instance coordinates))))))

(def function make-d-value-from-association-records (records default-value cardinality
                                                             dimensions coordinates)
  (bind ((d-value (make-single-d-value dimensions coordinates default-value)))
    (iter (for record :in records)
          (for instance = (first record))
          (for action = (second record))
          (for coords = (iter (for dimension :in dimensions)
                              (generating coordinate :in (cddr record))
                              (collect (etypecase dimension
                                         (inheriting-dimension
                                          (make-coordinate-range
                                           'ii
                                           (next coordinate)
                                           (maximum-coordinate-of dimension)))
                                         (ordering-dimension
                                          (make-coordinate-range
                                           'ie (next coordinate) (next coordinate)))
                                         (dimension
                                          (next coordinate))))))
          (ecase action
            (#.+t-clear+ (setf (value-at-coordinates d-value coords) nil))
            (#.+t-insert+ (ecase cardinality
                            (:1 (setf (value-at-coordinates d-value coords) instance))
                            (:n (insert-at-coordinates d-value coords instance))))
            (#.+t-delete+ (ecase cardinality
                            (:1 (clear-at-coordinates d-value coords instance))
                            (:n (delete-at-coordinates d-value coords instance))))))

    (value-at-coordinates d-value coordinates)))

(defmethod store-slot ((d-class persistent-class-d) (d-instance persistent-object-d) (d-slot persistent-association-end-effective-slot-definition-d) value)
  (store-d-association-end d-instance d-slot value))

(defmethod store-slot ((class persistent-class) (instance persistent-object) (d-slot persistent-association-end-effective-slot-definition-d) value)
  (store-d-association-end instance d-slot value))

(def function store-d-association-end (instance d-association-end value)
  ;; FIXME nil is not accepted as (set ...) type ?
  #+nil
  (if (values-having-validity-p value)
      (iter (for (start end v) :in-values-having-validity value)
            (check-slot-value-type instance t-association-end v))
      (check-slot-value-type instance t-association-end value))

  ;; store-all-slots may pass simple values here
  #+nil(assert (d-value-p value))
  (unless (d-value-p value)
    (setf value (make-single-d-value
                 (dimensions-of d-association-end)
                 (collect-coordinates-from-variables (dimensions-of d-association-end))
                 value)))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-slot instance d-association-end)

  (bind ((association-kind (association-kind-of (association-of d-association-end))))
    (iter (for (coordinates v) :in-d-value value)
          (ecase association-kind
            (:1-1 (store-1-1-d-association-end d-association-end instance v coordinates))
            (:1-n (store-1-n-d-association-end d-association-end instance v coordinates))
            (:m-n (store-m-n-d-association-end d-association-end instance v coordinates)))))
  value)

(defmethod lock-slot ((instance persistent-object) (slot persistent-association-end-effective-slot-definition-d) &key (wait t))
  (bind ((h-class (h-class-of slot))
         (table (name-of (primary-table-of h-class))))
    (execute
     (sql-lock-table :table table
                     :mode :exclusive
                     :wait wait))))

(def function store-1-1-d-association-end (d-association-end instance value coordinates)
  (insert-h-association-instance d-association-end instance value coordinates))

(def function store-1-n-d-association-end (d-association-end instance value coordinates)

  (ecase (cardinality-kind-of d-association-end)
    (:1 ;; set parent
     (insert-h-association-instance d-association-end instance nil coordinates :action +t-clear+)
     (when value
       (insert-h-association-instance d-association-end instance value coordinates :action +t-insert+)))
    (:n ;; set children
     (insert-h-association-instance d-association-end instance nil coordinates :action +t-clear+)
     (dolist (other-instance value)
       (insert-h-association-instance (other-association-end-of d-association-end) other-instance nil
                                      coordinates :action +t-clear+)
       (insert-h-association-instance d-association-end instance other-instance
                                      coordinates :action +t-insert+)))))

(def function store-m-n-d-association-end (d-association-end instance value coordinates)
  (insert-h-association-instance d-association-end instance nil coordinates :action +t-clear+)
  (dolist (other-instance value)
    (insert-h-association-instance d-association-end instance other-instance coordinates :action +t-insert+)))

(defmethod insert-into-association-end-set-d ((instance persistent-object)
                                              (d-association-end persistent-association-end-effective-slot-definition-d)
                                              (item persistent-object)
                                              &key coordinates)
  (insert/delete-association-end-set-d d-association-end instance item
                                       +t-insert+ coordinates))



(defmethod delete-from-association-end-set-d ((instance persistent-object)
                                              (d-association-end persistent-association-end-effective-slot-definition-d)
                                              (item persistent-object)
                                              &key coordinates)
  (insert/delete-association-end-set-d d-association-end instance item
                                       +t-delete+ coordinates))

(def function insert/delete-association-end-set-d (d-association-end instance item action coordinates)

  (lock-slot instance d-association-end)

  (when (and (eq action +t-insert+)
             (eq (association-kind-of (association-of d-association-end)) :1-n))
    (insert-h-association-instance (other-association-end-of d-association-end) item nil
                                   coordinates :action +t-clear+))

  (insert-h-association-instance d-association-end instance item coordinates :action action)
  (values))


;;;
;;; Queries
;;;
(def function dependent-object-slot-names (dimension)
  (mapcar #'slot-definition-name
          (class-direct-slots (find-class (dependent-object-name-of dimension)))))

(def (function io) slot-name-of (dimension)
  (assert (or (not (typep dimension 'ordering-dimension)) (typep dimension 'inheriting-dimension)))
  (name-of dimension))

(def (function io) begin-slot-name-of (dimension)
  (assert (and (typep dimension 'ordering-dimension) (not (typep dimension 'inheriting-dimension))))
  (format-symbol (symbol-package (name-of dimension)) "~A-BEGIN" (name-of dimension)))

(def (function io) end-slot-name-of (dimension)
  (assert (and (typep dimension 'ordering-dimension) (not (typep dimension 'inheriting-dimension))))
  (format-symbol (symbol-package (name-of dimension)) "~A-END" (name-of dimension)))

(def (function io) dimension-equal (dimension)
  (case (the-type-of dimension)
    ((time date timestamp) 'timestamp=)
    (t 'equal)))

(def function dimension-less (dimension)
  (case (the-type-of dimension)
    ((time date timestamp) 'timestamp<)
    ;; TODO string<
    (t '<)))

(def function dimension-less-or-equal (dimension)
  (case (the-type-of dimension)
    ((time date timestamp) 'timestamp<=)
    ;; TODO string<=
    (t '<=)))

(def function select-slot-values-with-dimensions (d-class d-instance d-slot coordinates)
  "Returns the values of the slot and the coordinate values. The records are ordered by the coordinates with inherited dimension. When there are inherited dimensions, only the most recent returned. A coordinate is either a (begin . end) pair (ordered dimension) or a set (enumerated dimension) ."
  (bind ((h-class-name (class-name (h-class-of d-class)))
         (h-slot (h-slot-of d-slot))
         (h-slot-name (slot-definition-name h-slot))
         (dimensions (dimensions-of d-slot))
         (query (make-query `(select (
                                      ;;(if (slot-boundp h-instance ',h-slot-name) KLUDGE fix query compiler
                                      ;;    (slot-value h-instance ',h-slot-name)
                                      ;;    ,+unbound-slot-marker+)
                                      (identity ;; this is not mapped to sql
                                       (or (and (not (slot-boundp h-instance ',h-slot-name))
                                                ,+unbound-slot-marker+)
                                           (slot-value h-instance ',h-slot-name))))
                               (from (h-instance ,h-class-name))
                               (where (and (eq (d-instance-of h-instance) d-instance)
                                           (or
                                            (not (slot-boundp h-instance ',h-slot-name))
                                            (not (eq (slot-value h-instance ',h-slot-name)
                                                     ,+h-unused-slot-marker+)))))
                               (order-by :ascending (oid-of h-instance)))
                            `(d-instance))))

    (iter (for dimension :in dimensions)
          (for coordinate :in coordinates)
          (for interval-p = (when (typep dimension 'ordering-dimension)
                              (assert (coordinate<= (coordinate-range-begin coordinate)
                                                    (coordinate-range-end coordinate)))
                              (coordinate< (coordinate-range-begin coordinate)
                                           (coordinate-range-end coordinate))))

          (add-lexical-variable query (name-of dimension))

          (etypecase dimension

            (inheriting-dimension

             (add-collect query `(slot-value h-instance ',(slot-name-of dimension)))

             (add-assert query
                         `(,(if interval-p
                                (dimension-less dimension)
                                (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(name-of dimension))
                            (coordinate-range-end ,(name-of dimension))))

             (when (and (length= 1 dimensions) interval-p)
               (add-assert query
                           `(,(dimension-less-or-equal dimension)
                              (coalesce
                               (select ((max (slot-value h-instance-2 ',(name-of dimension))))
                                 (from (h-instance-2 ,h-class-name))
                                 (where (and
                                         (eq (d-instance-of h-instance-2) d-instance)
                                         (or
                                          (not (slot-boundp h-instance-2 ',h-slot-name))
                                          (not (eq (slot-value h-instance-2 ',h-slot-name)
                                                   ,+h-unused-slot-marker+)))
                                         (,(dimension-less-or-equal dimension)
                                           (slot-value h-instance-2 ',(name-of dimension))
                                           (coordinate-range-begin ,(name-of dimension))))))
                               (coordinate-range-begin ,(name-of dimension)))
                              (slot-value h-instance ',(name-of dimension)))))

             (if (and (length= 1 dimensions) (not interval-p))
                 (bind ((direction (ecase (direction-of dimension)
                                     (:ascending :descending)
                                     (:descending :ascending))))
                   (setf (limit-of query) 1
                         (order-by-of query) `(,direction (slot-value h-instance ',(slot-name-of dimension))
                                                          ,direction (oid-of h-instance))))
                 (setf (order-by-of query)
                       `(,(direction-of dimension) (slot-value h-instance ',(slot-name-of dimension))
                          ,(direction-of dimension) (oid-of h-instance)))))

            (ordering-dimension
             (add-collect query `(slot-value h-instance ',(begin-slot-name-of dimension)))
             (add-collect query `(slot-value h-instance ',(end-slot-name-of dimension)))

             (add-assert query
                         `(,(if interval-p (dimension-less dimension) (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(begin-slot-name-of dimension))
                            (coordinate-range-end ,(name-of dimension))))
             (add-assert query
                         `(,(dimension-less dimension)
                            (coordinate-range-begin ,(name-of dimension))
                            (slot-value h-instance ',(end-slot-name-of dimension)))))

            (dimension
             (add-collect query `(or (and (null (slot-value h-instance ',(slot-name-of dimension)))
                                          +whole-domain-marker+) ; FIXME if
                                     (list (slot-value h-instance ',(slot-name-of dimension)))))
             (unless (whole-domain-marker-p coordinate)
               (if coordinate
                   (add-assert query
                               `(or (null (slot-value h-instance ',(slot-name-of dimension)))
                                    (member
                                     (slot-value h-instance ',(slot-name-of dimension))
                                     ,(name-of dimension))))
                   (add-assert query #f))))))


    (apply #'execute-query query d-instance coordinates)))

(defun update-h-instance-slot-value (d-class d-instance d-slot value coordinates)

  (bind ((h-class (h-class-of d-class))
         (h-slot (h-slot-of d-slot))
         (dimensions (dimensions-of d-slot))
         (query (make-query
                 `(update (h-instance ,(class-name h-class))
                    (set (slot-value h-instance ',(slot-definition-name h-slot)) value)
                    (where (and (eq (d-instance-of h-instance) d-instance))))
                 '(d-instance value))))

    (iter (for dimension :in dimensions)
          (for coordinate :in coordinates)
          (add-lexical-variable query (name-of dimension))
          (etypecase dimension
            (inheriting-dimension
             (collect coordinate :into coordinates*)
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(slot-name-of dimension))
                                  (coordinate-range-begin ,(name-of dimension)))))
            (ordering-dimension
             (collect coordinate :into coordinates*)
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(begin-slot-name-of dimension))
                                  (coordinate-range-begin ,(name-of dimension))))
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(end-slot-name-of dimension))
                                  (coordinate-range-end ,(name-of dimension)))))
            (dimension
             (collect (if (whole-domain-marker-p coordinate) nil coordinate) :into coordinates*)
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(slot-name-of dimension))
                                  ,(name-of dimension)))))

          (finally
           ;; generate condition for version check
           (assert coordinate () "~A coordinate is bound to NIL." (coordinate-name-of dimension))
           (add-assert query `(eq h-instance (select ((max (oid-of h-instance-2)))
                                               (from (h-instance-2 ,(class-name h-class)))
                                               (where (and (eq (d-instance-of h-instance-2) d-instance))))))
           (return (prog1-bind count (apply 'execute-query query d-instance value coordinates*)
                     (assert (<= count 1) nil "Inconsistent database")))))))

(def function insert-h-instance (d-class d-instance d-slot value coordinates)
  (bind ((h-class (h-class-of d-class)))
    (flet ((initarg-of (slot-name)
             (aprog1 (first (slot-definition-initargs (find-persistent-slot h-class slot-name)))
               (assert it))))
      (apply 'make-instance
            h-class
            :d-instance d-instance
            (initarg-of (slot-definition-name d-slot)) value
            (iter (for dimension :in (dimensions-of d-slot))
                  (for coordinate :in coordinates)
                  (etypecase dimension
                    (inheriting-dimension
                     (assert (coordinate-range-empty-p coordinate))
                     (collect (initarg-of (slot-name-of dimension)))
                     (collect (coordinate-range-begin coordinate)))
                    (ordering-dimension
                     (collect (initarg-of (begin-slot-name-of dimension)))
                     (collect (coordinate-range-begin coordinate))
                     (collect (initarg-of (end-slot-name-of dimension)))
                     (collect (coordinate-range-end coordinate)))
                    (dimension
                     (assert coordinate () "~A coordinate is bound to NIL." (coordinate-name-of dimension))
                     (collect (initarg-of (slot-name-of dimension)))
                     (collect (if (whole-domain-marker-p coordinate) nil coordinate)))))))))

(defun select-association-end-values-with-dimensions (instance d-association-end coordinates)
  "Returns the values of the association-end (with validity if time-dependent) in descending t order (if temporal). When temporal, but not time-dependent then only the most recent queried."
  (check-type instance persistent-object)

  (bind ((dimensions (dimensions-of d-association-end))
         (h-class-name (class-name (h-class-of d-association-end)))
         (h-association-end-name (slot-definition-name (h-slot-of d-association-end)))
         (other-h-association-end-name (slot-definition-name (other-end-h-slot-of d-association-end)))
         (association-kind (association-kind-of (association-of d-association-end)))
         (query (make-query `(select
                                 ((slot-value h-instance ',other-h-association-end-name)
                                  ,@(unless (eq association-kind :1-1)
                                            `((action-of h-instance))))
                               (from (h-instance ,h-class-name))
                               (where (eq (slot-value h-instance ',h-association-end-name) instance))
                               (order-by :ascending (oid-of h-instance)))
                            '(instance))))

    (iter (for dimension :in dimensions)
          (for coordinate :in coordinates)
          (for interval-p = (when (typep dimension 'ordering-dimension)
                              (assert (coordinate<= (coordinate-range-begin coordinate)
                                                    (coordinate-range-end coordinate)))
                              (coordinate< (coordinate-range-begin coordinate)
                                           (coordinate-range-end coordinate))))

          (add-lexical-variable query (name-of dimension))

          (etypecase dimension
            (inheriting-dimension
             (add-collect query `(slot-value h-instance ',(slot-name-of dimension)))
             (add-assert query
                         `(,(if interval-p
                                (dimension-less dimension)
                                (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(name-of dimension))
                            (coordinate-range-end ,(name-of dimension))))

             (when (and (length= 1 dimensions) interval-p (eq association-kind :1-1))
               (add-assert query
                           `(,(dimension-less-or-equal dimension)
                              (coalesce
                               (select ((max (slot-value h-instance-2 ',(name-of dimension))))
                                 (from (h-instance-2 ,h-class-name))
                                 (where (and
                                         (eq (slot-value h-instance-2 ',h-association-end-name) instance)
                                         (,(dimension-less-or-equal dimension)
                                           (slot-value h-instance-2 ',(name-of dimension))
                                           (coordinate-range-begin ,(name-of dimension))))))
                               (coordinate-range-begin ,(name-of dimension)))
                              (slot-value h-instance ',(name-of dimension)))))

             (if (and (length= 1 dimensions) (not interval-p) (eq association-kind :1-1))
                 (bind ((direction (ecase (direction-of dimension)
                                     (:ascending :descending)
                                     (:descending :ascending))))
                   (setf (limit-of query) 1
                         (order-by-of query) `(,direction (slot-value h-instance ',(slot-name-of dimension))
                                                          ,direction (oid-of h-instance))))
                 (setf (order-by-of query)
                       `(,(direction-of dimension) (slot-value h-instance ',(slot-name-of dimension))
                          ,(direction-of dimension) (oid-of h-instance)))))

            (ordering-dimension
             (add-collect query `(slot-value h-instance ',(begin-slot-name-of dimension)))
             (add-collect query `(slot-value h-instance ',(end-slot-name-of dimension)))

             (add-assert query
                         `(,(if interval-p (dimension-less dimension) (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(begin-slot-name-of dimension))
                            (coordinate-range-end ,(name-of dimension))))
             (add-assert query
                         `(,(dimension-less dimension)
                            (coordinate-range-begin ,(name-of dimension))
                            (slot-value h-instance ',(end-slot-name-of dimension)))))

            (dimension
             (assert coordinate () "~A coordinate is bound to NIL." (coordinate-name-of dimension))
             (add-collect query `(or (and (null (slot-value h-instance ',(slot-name-of dimension))) ;FIXME if
                                          +whole-domain-marker+)
                                     (list (slot-value h-instance ',(slot-name-of dimension)))))
             (unless (whole-domain-marker-p coordinate)
               (add-assert query
                          `(or (null (slot-value h-instance ',(slot-name-of dimension)))
                               (member
                                (slot-value h-instance ',(slot-name-of dimension))
                                ,(name-of dimension))))))))

    (apply #'execute-query query instance coordinates)))

(defun insert-h-association-instance (d-association-end instance other-instance coordinates &key action)
  (assert (or (null (action-slot-of d-association-end))
              (integerp action)))

  (bind ((h-class (h-class-of d-association-end))
         (dimensions (dimensions-of d-association-end)))
    (flet ((initarg-of (slot-or-slot-name)
             (aprog1 (first (slot-definition-initargs (if (symbolp slot-or-slot-name)
                                                          (find-persistent-slot h-class slot-or-slot-name)
                                                          slot-or-slot-name)))
               (assert it))))
      (iterate-with-enumerated-coordinate-sets
       (lambda (coordinates)
         (apply 'make-instance
                h-class
                (append
                 (list (initarg-of (h-slot-of d-association-end)) instance)
                 (list (initarg-of (other-end-h-slot-of d-association-end)) other-instance)
                 (when (action-slot-of d-association-end)
                   (list :action action))
                 (iter (for dimension :in dimensions)
                       (for coordinate :in coordinates)
                       (etypecase dimension
                         (inheriting-dimension
                          (assert (coordinate-range-empty-p coordinate))
                          (collect (initarg-of (slot-name-of dimension)))
                          (collect (coordinate-range-begin coordinate)))
                         (ordering-dimension
                          (collect (initarg-of (begin-slot-name-of dimension)))
                          (collect (coordinate-range-begin coordinate))
                          (collect (initarg-of (end-slot-name-of dimension)))
                          (collect (coordinate-range-end coordinate)))
                         (dimension
                          (assert coordinate () "~A coordinate is bound to NIL." (coordinate-name-of dimension))
                          (collect (initarg-of (slot-name-of dimension)))
                          (collect (if (whole-domain-marker-p coordinate) nil coordinate))))))))
       dimensions coordinates))))

;;;
;;; Helpers
;;;
(def function iterate-with-enumerated-coordinate-sets (function dimensions coordinates)
  "FUNCTION accepts a coordinate list when each coordinate belongs to an enumerated dimension contains only one value. COORDINATES may contain a set of values in these dimensions, the FUNCTION will be called with each value."
  (assert (length= dimensions coordinates))
  (labels ((recurse (remaining-dimensions remaining-coordinates &optional accumulator)
             (cond
               ((null remaining-dimensions)
                (funcall function (reverse accumulator)))
               ((or (typep (first remaining-dimensions) 'ordering-dimension)
                    (whole-domain-marker-p (first remaining-coordinates))
                    (not (listp (first remaining-coordinates))))
                (recurse (rest remaining-dimensions) (rest remaining-coordinates)
                         (list* (first remaining-coordinates) accumulator)))
               (t
                (assert (consp (first remaining-coordinates)))
                (dolist (coordinate (first remaining-coordinates))
                  (recurse (rest remaining-dimensions) (rest remaining-coordinates)
                           (list* coordinate accumulator)))))))
    (recurse dimensions coordinates)))
