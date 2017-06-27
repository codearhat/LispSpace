;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; C value

(def class* c-value ()
  ((coordinates :reader coordinates-of)
   (value :reader value-of)))

(def (function e) c-value-p (value)
  (typep value 'c-value))

(def print-object c-value ()
  (princ (value-of -self-)))

(def (function e) print-c-value (c-value &optional (stream t))
  (format stream "(~A (~{~A~^ ~}))" (value-of c-value) (coordinates-of c-value)))

(def function pprint-c-value (stream c-value)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (princ (value-of c-value) stream)
    (write-char #\Space stream)
    (pprint-linear stream (coordinates-of c-value))))

(set-pprint-dispatch 'c-value 'pprint-c-value)

(def (function e) make-c-value (coordinates value)
  (make-instance 'c-value
                 :coordinates coordinates
                 :value value))

(def (function e) copy-c-value (c-value)
  (make-instance 'c-value
                 :coordinates (coordinates-of c-value)
                 :value (value-of c-value)))

(def (function e) c-value-equal (dimensions c-value-1 c-value-2 &key (test #'eql))
  (and (funcall test
                (value-of c-value-1)
                (value-of c-value-2))
       (coordinates-equal dimensions
                          (coordinates-of c-value-1)
                          (coordinates-of c-value-2))))

;;;;;;
;;; D value (multi dimensional value)

(def class* d-value ()
  ((dimensions)
   (c-values)
   (index :type hash-table)))

(def (function e) d-value-p (value)
  (typep value 'd-value))

(def (special-variable e) *print-d-value-details* #t)

(def print-object d-value ()
  (format t "(~{~A~^, ~})" (mapcar #'name-of (dimensions-of -self-)))
  (when *print-d-value-details*
    (iter (for c-values-tail :on (c-values-of -self-))
          (princ " ")
          (print-c-value (first c-values-tail))
          (unless (null (rest c-values-tail)) ;last-iteration-p
            (terpri)))))

(def function pprint-d-value (stream d-value)
  (pprint-logical-block (stream nil :prefix "#<D-VALUE " :suffix ">")
    (pprint-fill stream (mapcar #'name-of (dimensions-of d-value)))
    (write-char #\Space stream)
    (pprint-newline :linear stream)
    (pprint-linear stream (c-values-of d-value) #f)))

(set-pprint-dispatch 'd-value 'pprint-d-value)

(def (function e) print-d-value (d-value &optional (stream t))
  (bind ((*print-d-value-details* #t))
    (princ d-value stream)))

(def function assert-valid-d-value (d-value)
  (iter (with dimensions = (dimensions-of d-value))
        (with number-of-dimensions = (length dimensions))
        (for c-value-1-cell :on (c-values-of d-value))
        (for c-value-1 = (car c-value-1-cell))
        (assert (length= number-of-dimensions (length (coordinates-of c-value-1)))
                nil "Invalid number of coordinates in ~A" d-value)
        (iter (for c-value-2 :in (cdr c-value-1-cell))
              (assert (not (coordinates-intersection dimensions (coordinates-of c-value-1) (coordinates-of c-value-2)))
                      nil "Invalid d-value due to overlapping coordinates found in c-values of ~A" d-value)))
  ;; TODO check index
  d-value)

(def function consolidate-c-value (c-value d-value)
  (iter (with dimensions = (dimensions-of d-value))
        (with c-value-coordinates = (coordinates-of c-value))
        (with value = (value-of c-value))
        (for other-c-value :in (gethash value (index-of d-value)))
        (for other-c-value-coordinates = (coordinates-of other-c-value))
        (when-bind coordinates (coordinates-union dimensions c-value-coordinates other-c-value-coordinates)
          (remove-c-value d-value other-c-value)
          (return-from consolidate-c-value (consolidate-c-value (make-c-value coordinates value) d-value)))
        (finally (return c-value))))

(def function add-c-value (d-value c-value)
  (bind ((c-value (consolidate-c-value c-value d-value)))
    (push c-value (c-values-of d-value))
    (push c-value (gethash (value-of c-value) (index-of d-value)))))


(def function remove-c-value (d-value c-value)
  (deletef (c-values-of d-value) c-value)
  (deletef (gethash (value-of c-value) (index-of d-value)) c-value))

(def function build-index (c-values)
  (prog1-bind index (make-hash-table :test #'equal)
    (mapc [push !1 (gethash (value-of !1) index)] c-values)))

(def (function e) make-empty-d-value (dimensions)
  (make-instance 'd-value
                 :dimensions (mapcar #'lookup-dimension dimensions)
                 :c-values nil
                 :index (build-index nil)))

(def (function e) make-single-d-value (dimensions coordinates value)
  (bind ((c-values (list (make-c-value coordinates value))))
    (make-instance 'd-value
                   :dimensions (mapcar #'lookup-dimension dimensions)
                   :c-values c-values
                   :index (build-index c-values))))

(def (function e) make-d-value (dimensions coordinates-list values)
  (labels ((cook-coordinates (coordinates)
             (iter (for dimension :in dimensions)
                   (for coordinate :in coordinates)
                   (etypecase (lookup-dimension dimension)
                     (ordering-dimension (collect coordinate))
                     (dimension (collect (if (whole-domain-marker-p coordinate)
                                             coordinate
                                             (progn
                                               (debug-only (assert (consp coordinate)))
                                               (cook-coordinate coordinate))))))))
           (uncook-coordinates (coordinates)
             (iter (for dimension :in dimensions)
                   (for coordinate :in coordinates)
                   (etypecase (lookup-dimension dimension)
                     (ordering-dimension (collect coordinate))
                     (dimension (collect (if (whole-domain-marker-p coordinate)
                                             coordinate
                                             (uncook-coordinate coordinate)))))))
           (uncook-d-value (d-value)
             (iter (for c-value :in (c-values-of d-value))
                   (setf (slot-value c-value 'coordinates) ;; FIXME mutating coordinates!
                         (uncook-coordinates (coordinates-of c-value))))))

    (prog1-bind d-value (make-empty-d-value dimensions)
      (setf coordinates-list (mapcar #'cook-coordinates coordinates-list))

      (mapc (lambda (coordinates value)
              (setf (value-at-coordinates d-value coordinates) value))
            coordinates-list values)
      (uncook-d-value d-value))))

(def (function e) empty-d-value-p (d-value)
  (null (c-values-of d-value)))

(def (function e) single-d-value-p (d-value)
  (length= 1 (c-values-of d-value)))

(def (function e) single-d-value (d-value)
  (assert (single-d-value-p d-value) () "~S: multiple coordinates in ~A" 'single-d-value d-value)
  (value-of (first (c-values-of d-value))))

(def (function e) dimension-position (d-value dimension)
  (position (lookup-dimension dimension) (dimensions-of d-value)))

(def (function e) single-d-value-coordinate (d-value dimension)
  (assert (single-d-value-p d-value))
  (elt (coordinates-of (first (c-values-of d-value)))
       (dimension-position d-value dimension)))

(def (function e) d-value-dimension-coordinate-list (d-value dimension &key (mode :union))
  (funcall (ecase mode
             (:union 'coordinate-list-union)
             (:intersection 'overlapping-coordinate-list-self-intersection))
           dimension
           (iter (with index = (dimension-position d-value dimension))
                 (for c-value :in (c-values-of d-value))
                 (collect (elt (coordinates-of c-value) index)))))

(def (function e) d-values-have-same-dimensions-p (d-values)
  (bind ((dimensions (dimensions-of (first d-values))))
    (every (lambda (d-value)
             (equal dimensions
                    (dimensions-of d-value)))
           d-values)))

(def (function e) copy-d-value (d-value)
  (debug-only (assert-valid-d-value d-value))
  (bind ((c-values (copy-list (c-values-of d-value))))
    (make-instance 'd-value
                  :dimensions (dimensions-of d-value)
                  :c-values c-values
                  :index (build-index c-values))))

(def (function e) d-value-equal (d-value-1 d-value-2 &key (test #'eql))
  (debug-only (and (assert-valid-d-value d-value-1)
                   (assert-valid-d-value d-value-2)))
  (iter (with dimensions-1 = (dimensions-of d-value-1))
        (with dimensions-2 = (dimensions-of d-value-2))
        (unless (equal dimensions-1 dimensions-2)
          (return-from d-value-equal #f))
        (when (empty-d-value-p d-value-1)
          (return-from d-value-equal (empty-d-value-p d-value-2)))
        (for c-value-1 :in (c-values-of d-value-1))
        (for d-value-1-part = (make-single-d-value dimensions-1 (coordinates-of c-value-1) (value-of c-value-1)))
        (for d-value-2-part = (value-at-coordinates d-value-2 (coordinates-of c-value-1)))
        (always (or (and (single-d-value-p d-value-2-part)
                         (c-value-equal dimensions-1 c-value-1 (first (c-values-of d-value-2-part)) :test test))
                    (and
                     (not (single-d-value-p d-value-2-part))
                     (d-value-equal d-value-2-part
                                    d-value-1-part
                                    :test test))))))

(def (function e) covering-d-value-p (d-value coordinates)
  (debug-only (assert-valid-d-value d-value))
  (bind ((remaining-coordinates (list coordinates)))
    (iter (with dimensions = (dimensions-of d-value))
          (for c-value :in (c-values-of d-value))
          (setf remaining-coordinates
                (iter (for remaining-coordinate :in remaining-coordinates)
                      (appending (coordinates-difference dimensions remaining-coordinate (coordinates-of c-value))))))
    (null remaining-coordinates)))

(def function consolidate-d-value (d-value)
  (debug-only (assert-valid-d-value d-value))
  (bind ((dimensions (dimensions-of d-value))
         (index (index-of d-value)))
    (flet ((consolidate-c-values (d-value value)
            (tagbody
             :restart
               (dopairs (c-value-1 c-value-2 (gethash value index))
                 (when-bind coordinates (coordinates-union dimensions (coordinates-of c-value-1) (coordinates-of c-value-2))
                             (remove-c-value d-value c-value-1)
                             (remove-c-value d-value c-value-2)
                             (add-c-value d-value (make-c-value coordinates value))
                             (go :restart))))))
      (maphash-keys [consolidate-c-values d-value !1] index)
      d-value)))

(def (function e) single-value-at-coordinates (d-value coordinates &key (otherwise :error))
  (debug-only (assert-valid-d-value d-value))
  (iter (with dimensions = (dimensions-of d-value))
        (for c-value :in (c-values-of d-value))
        (when (covering-coordinates-p dimensions (coordinates-of c-value) coordinates)
          (return-from single-value-at-coordinates (value-of c-value)))
        (finally
         (return (handle-otherwise/value otherwise :default-message `("~S: Covering c-value not found for ~A in ~A" 'single-value-at-coordinates ,coordinates ,d-value))))))

(def (function e) value-at-coordinates (d-value coordinates)
  (debug-only (assert-valid-d-value d-value))
  (progn ;;consolidate-d-value
   (prog1-bind result-d-value (make-empty-d-value (dimensions-of d-value))
     (iter (with dimensions = (dimensions-of d-value))
           (for c-value :in (c-values-of d-value))
           (for intersection = (coordinates-intersection dimensions (coordinates-of c-value) coordinates))
           (when (or intersection (null dimensions)) ; FIXME
             (add-c-value result-d-value
                          (make-c-value intersection
                                        (value-of c-value))))))))

(def (function e) (setf value-at-coordinates) (new-value d-value coordinates)
  (debug-only (assert-valid-d-value d-value))
  (iter (with dimensions = (dimensions-of d-value))
        (for c-value :in (c-values-of d-value))
        (for old-coordinates = (coordinates-of c-value))
        (when (or (coordinates-intersection dimensions old-coordinates coordinates) (null dimensions)) ;; FIXME
          (remove-c-value d-value c-value)
          (mapc [add-c-value d-value (make-c-value !1 (value-of c-value))]
                (coordinates-difference dimensions old-coordinates coordinates))))
  (add-c-value d-value (make-c-value coordinates new-value))
  ;;(consolidate-d-value d-value)
  d-value)

(def (function e) (setf into-d-value) (new-d-value d-value)
  (debug-only (and (assert-valid-d-value new-d-value)
                   (assert-valid-d-value d-value)))
  (iter (for c-value :in (c-values-of new-d-value))
        (setf (value-at-coordinates d-value (coordinates-of c-value))
              (value-of c-value)))
  (debug-only (assert-valid-d-value d-value))
  d-value)

;;;;;;
;;; Iteration support

;; TODO make the coordinates variable optional. the order should (?) be (value coordinates), check all usages, rethink in-d-values also based on this
#+nil
(defmacro-clause (for variables :in-d-value d-value)
  (setf variables (ensure-list variables))
  (assert (<= (length variables) 2))
  (bind ((value-variable (first variables))
         (coordinates-variable (second variables)))
    (with-unique-names (d-value-variable c-value-variable)
      `(progn
         (with ,d-value-variable = ,d-value)
         (for ,c-value-variable :in (c-values-of ,d-value-variable))
         (for ,value-variable = (value-of ,c-value-variable))
         ,@(when coordinates-variable
             `((for ,coordinates-variable = (coordinates-of ,c-value-variable))))))))

(defmacro-clause (for variables :in-d-value d-value)
  (assert (length= 2 variables))
  (with-unique-names (d-value-variable c-value-variable)
    `(progn
       (with ,d-value-variable = ,d-value)
       (for ,c-value-variable :in (c-values-of ,d-value-variable))
       (for ,(first variables) = (coordinates-of ,c-value-variable))
       (for ,(second variables) = (value-of ,c-value-variable)))))

(defmacro-clause (for variables :in-d-values d-values :unspecified-value unspecified-value)
  (with-unique-names (d-values-variable coordinates-variable)
    `(progn
       (with ,d-values-variable = ,(if (listp d-values)
                                       `(list ,@d-values)
                                       d-values))
       (for ,coordinates-variable :in (split-d-values-coordinates-lists ,d-values-variable))
       (for ,(first variables) = ,coordinates-variable)
       (for ,(second variables) = (mapcar (lambda (d-value)
                                            (single-value-at-coordinates d-value ,coordinates-variable :otherwise ,unspecified-value))
                                          ,d-values-variable)))))

(iter::defclause (collect-d-value expr &optional dimensions dimensions coordinates coordinates into variable)
  (bind ((collect-variable-spec (or variable iter::*result-var*))
         (collect-variable (iter::extract-var collect-variable-spec)))
    (iter::make-accum-var-binding collect-variable-spec nil :collect-d-value)
    (iter::return-code
     :initial `((setf ,collect-variable (make-empty-d-value ,dimensions)))
     :body `((progn
               (setf (value-at-coordinates ,collect-variable ,coordinates) ,expr))))))

(export 'collect-d-value)

(def function coordinate-list-difference (dimension coordinate-list-1 coordinate-list-2)
  (when (null coordinate-list-2)
    (return-from coordinate-list-difference coordinate-list-1))
  (iter outer
        (for coordinate-1 :in coordinate-list-1)
        (for differences = (iter (for coordinate-2 :in coordinate-list-2)
                                 (collect (coordinate-difference dimension coordinate-1 coordinate-2))))
        (when differences
          (appending (reduce (lambda (coordinate-list-1 coordinate-list-2)
                               (coordinate-list-intersection dimension coordinate-list-1 coordinate-list-2))
                             differences)))))

(def function coordinate-list-intersection (dimension coordinate-list-1 coordinate-list-2)
  (iter outer
        (for coordinate-1 :in coordinate-list-1)
        (iter (for coordinate-2 :in coordinate-list-2)
              (for intersection = (coordinate-intersection dimension coordinate-1 coordinate-2))
              (unless (empty-set-p intersection)
                (in outer (collect intersection))))))

(def function split-coordinate-lists (dimension coordinate-list-1 coordinate-list-2)
  (bind ((intersections (coordinate-list-intersection dimension coordinate-list-1 coordinate-list-2)))
    (append intersections
            (coordinate-list-difference dimension coordinate-list-1 intersections)
            (coordinate-list-difference dimension coordinate-list-2 intersections))))

(def (function e) coordinate-list-union (dimension coordinate-list)
  (iter (for coordinate :in coordinate-list)
        (unless (iter inner
                      (for coordinate-cell :on result)
                      (awhen (coordinate-union dimension coordinate (car coordinate-cell))
                        (setf (car coordinate-cell) it)
                        (return-from inner #t)))
          (collect coordinate :into result))
        (finally (return result))))

(def (function e) overlapping-coordinate-list-self-intersection (dimension coordinate-list)
  (reduce (lambda (&optional coordinate-list-1 coordinate-list-2)
            (split-coordinate-lists dimension coordinate-list-1 coordinate-list-2))
          (mapcar 'list coordinate-list)))

(def function coordinates-list-difference (dimensions coordinates-list-1 coordinates-list-2)
  (when (null coordinates-list-2)
    (return-from coordinates-list-difference coordinates-list-1))
  (iter outer
        (for coordinates-1 :in coordinates-list-1)
        (for differences = (iter (for coordinates-2 :in coordinates-list-2)
                                 (collect (coordinates-difference dimensions coordinates-1 coordinates-2))))
        (when differences
          (appending (reduce (lambda (coordinates-list-1 coordinates-list-2)
                               (coordinates-list-intersection dimensions coordinates-list-1 coordinates-list-2))
                             differences)))))

(def function coordinates-list-intersection (dimensions coordinates-list-1 coordinates-list-2)
  (iter outer
        (for coordinates-1 :in coordinates-list-1)
        (iter (for coordinates-2 :in coordinates-list-2)
              (awhen (coordinates-intersection dimensions coordinates-1 coordinates-2)
                (in outer (collect it))))))

(def function split-coordinates-lists (dimensions coordinates-list-1 coordinates-list-2)
  (bind ((intersections (coordinates-list-intersection dimensions coordinates-list-1 coordinates-list-2)))
    (append intersections
            (coordinates-list-difference dimensions coordinates-list-1 intersections)
            (coordinates-list-difference dimensions coordinates-list-2 intersections))))

(def (function e) overlapping-coordinates-list-self-intersection (dimensions coordinates-list)
  (reduce (lambda (&optional coordinates-list-1 coordinates-list-2)
            (split-coordinates-lists dimensions coordinates-list-1 coordinates-list-2))
          (mapcar 'list coordinates-list)))

(def function split-d-values-coordinates-lists (d-values)
  (bind ((dimensions (dimensions-of (first d-values))))
    (reduce (lambda (coordinates-list-1 coordinates-list-2)
              (split-coordinates-lists dimensions coordinates-list-1 coordinates-list-2))
            d-values
            :key (lambda (d-value)
                   (mapcar #'coordinates-of (c-values-of d-value))))))

;;;;;;
;;; Set-valued d-value operations

(def (function e) insert-at-coordinates (d-value coordinates value &key (test #'eql))
  (debug-only (assert-valid-d-value d-value))
  (iter (for (coordinates set) :in-d-value (value-at-coordinates d-value coordinates))
        (setf (value-at-coordinates d-value coordinates) (adjoin value set :test test)))
  (debug-only (assert-valid-d-value d-value))
  d-value)

(def (function e) delete-at-coordinates (d-value coordinates value &key (test #'eql))
  (debug-only (assert-valid-d-value d-value))
  (iter (for (coordinates set) :in-d-value (value-at-coordinates d-value coordinates))
        (setf (value-at-coordinates d-value coordinates) (remove value set :test test)))
  (debug-only (assert-valid-d-value d-value))
  d-value)

(def (function e) clear-at-coordinates (d-value coordinates value &key (test #'eql))
  (debug-only (assert-valid-d-value d-value))
  (iter (for (coordinates value1) :in-d-value (value-at-coordinates d-value coordinates))
        (when (funcall test value1 value)
          (setf (value-at-coordinates d-value coordinates) nil)))
  (debug-only (assert-valid-d-value d-value))
  d-value)

;;;;;;
;;; D operations

(def (function e) print-d-value-differences (d-value-1 d-value-2 &key (test #'eql) unspecified-value)
  (iter (with difference-count = 0)
        (for (coordinates values) :in-d-values (d-value-1 d-value-2) :unspecified-value unspecified-value)
        (for value-1 = (first values))
        (for value-2 = (second values))
        (for count :from 0)
        (unless (funcall test value-1 value-2)
          (incf difference-count)
          (format t "The values ~A and ~A are different for coordinates ~A~%" value-1 value-2 coordinates))
        (finally
         (format t "Checked ~A values and found ~A differences~%" count difference-count))))

(def (function e) map-d-value (d-value function)
  (mapc (lambda (c-value)
          (funcall function (coordinates-of c-value) (value-of c-value)))
        (c-values-of d-value)))

(def (function e) mapcar-d-value (d-value function)
  (mapcar (lambda (c-value)
            (funcall function (coordinates-of c-value) (value-of c-value)))
          (c-values-of d-value)))

(def (function e) map-d-values (function d-values &key unspecified-value)
  (assert (d-values-have-same-dimensions-p d-values))
  (mapc (lambda (coordinates)
          (apply function
                 coordinates
                 (mapcar (lambda (d-value)
                           (single-value-at-coordinates d-value coordinates :otherwise unspecified-value))
                         d-values)))
        (split-d-values-coordinates-lists d-values)))

(def (function e) d-apply (function d-values &key (unspecified-value :signal-default-error))
  (assert (d-values-have-same-dimensions-p d-values))
  (iter (with dimensions = (dimensions-of (first d-values)))
        (for (coordinates values) :in-d-values d-values :unspecified-value unspecified-value)
        (collect-d-value (apply function values)
                         :dimensions dimensions
                         :coordinates (if (length= 1 dimensions)
                                          (list coordinates)
                                          coordinates))))

(def (function e) d-project (function projection-dimensions d-value)
  (debug-only (assert (every (of-type 'dimension) projection-dimensions)))
  (bind ((dimensions (dimensions-of d-value))
         (remaining-dimensions (remove-if [member !1 projection-dimensions] (dimensions-of d-value)))
         (projection-coordinates-list (remove-duplicates
                                       (overlapping-coordinates-list-self-intersection
                                        projection-dimensions
                                        (mapcar-d-value d-value
                                                        (lambda (coordinates value)
                                                          (declare (ignorable value))
                                                          (collect-subcoordinates dimensions projection-dimensions coordinates))))
                                       :test #'coordinates=)))
    (iter (for projection-coordinates :in projection-coordinates-list)
          (for coordinates = (iter (generate projection-coordinate :in projection-coordinates)
                                   (for dimension :in dimensions)
                                   (collect (if (member dimension projection-dimensions)
                                                (next projection-coordinate)
                                                ;; FIXME:
                                                (etypecase dimension
                                                  (ordering-dimension (make-ie-coordinate-range
                                                                       (minimum-coordinate-of dimension)
                                                                       (maximum-coordinate-of dimension)))
                                                  (dimension +whole-domain-marker+))))))
          (for projected-d-value = (value-at-coordinates d-value coordinates))
          (collect-d-value (apply function remaining-dimensions
                                  (iter (for c-value :in (c-values-of projected-d-value))
                                        (for value = (value-of c-value))
                                        (for coordinates = (collect-subcoordinates dimensions remaining-dimensions (coordinates-of c-value)))
                                        (collect value :into values)
                                        (collect coordinates :into remaining-coordinates-list)
                                        (finally
                                         (return (list remaining-coordinates-list values)))))
                           :dimensions projection-dimensions
                           :coordinates projection-coordinates))))

(def (function e) d-fold (function folded-dimensions d-value &key initial-value)
  (assert (every (of-type 'dimension) folded-dimensions))
  (if folded-dimensions
      (iter (with dimensions = (dimensions-of d-value))
            (with unfolded-dimensions = (remove-if [member !1 folded-dimensions] (dimensions-of d-value)))
            (with result = (make-empty-d-value unfolded-dimensions))
            (for (coordinates value) :in-d-value d-value)
            (for folded-coordinates = (collect-subcoordinates dimensions folded-dimensions coordinates))
            (for unfolded-coordinates = (collect-subcoordinates dimensions unfolded-dimensions coordinates))
            (for accumulated-d-value = (aprog1 (make-single-d-value unfolded-dimensions unfolded-coordinates initial-value)
                                         (setf (into-d-value it) (value-at-coordinates result unfolded-coordinates))))
            (iter (for (unfolded-coordinates-2 accumulated-value) :in-d-value accumulated-d-value)
                  (setf (value-at-coordinates result unfolded-coordinates-2)
                        (funcall function accumulated-value folded-coordinates value)))
            (finally (return result)))
      d-value))

(def (function e) remove-dimensions (d-value dimensions)
  (d-fold (lambda (accumulated-value folded-coordinates value)
            (declare (ignore accumulated-value folded-coordinates))
            value)
          dimensions d-value))

(def (function e) d-volume (d-value &key (volume-function #'*))
  (iter (with dimensions = (dimensions-of d-value))
        (for c-value :in (c-values-of d-value))
        (summing (funcall volume-function
                          (value-of c-value)
                          (iter (for dimension :in dimensions)
                                (for coordinate :in (coordinates-of c-value))
                                (for volume :initially 1 :then (typecase dimension
                                                                 (inheriting-dimension volume)
                                                                 (ordering-dimension (- (coordinate-range-end coordinate)
                                                                                        (coordinate-range-begin coordinate)))
                                                                 (t
                                                                  (* volume (length coordinate)))))
                                (finally
                                 (return volume)))))))

(def (function e) d-equal (d-value-1 d-value-2)
  (d-apply #'equal (list d-value-1 d-value-2)))

(def (function e) d= (d-value &rest d-values)
  (d-apply #'= (list* d-value d-values)))

(def (function e) d+ (&rest d-values)
  (d-apply #'+ d-values))

(def (function e) d- (d-value &rest d-values)
  (d-apply #'- (list* d-value d-values)))

(def (function e) d* (&rest d-values)
  (d-apply #'* d-values))

(def (function e) d/ (d-value &rest d-values)
  (d-apply #'/ (list* d-value d-values)))

(def (macro e) d-incf (place delta)
  `(setf ,place (d+ ,place ,delta)))

(def (macro e) d-decf (place delta)
  `(setf ,place (d+ ,place ,delta)))
