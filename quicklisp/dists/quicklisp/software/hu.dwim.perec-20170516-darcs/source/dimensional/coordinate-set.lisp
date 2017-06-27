;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;
;;; KLUDGE
;;;
(def class* cooked-instance-set ()
  ((value () :type list)))

(def function make-cooked-instance-set (instance-set)
  (debug-only (assert (every [and (consp !1) (integerp (car !1)) (persistent-object-p (cdr !1))] instance-set)))
  (make-instance 'cooked-instance-set :value instance-set))

(def function cook-coordinate (instance-set)
  (if (and (consp instance-set)
           (persistent-object-p (first instance-set)))
      (make-cooked-instance-set (mapcar [cons (oid-of !1) !1] instance-set))
      instance-set))

(def function uncook-coordinate (cooked-instance-set)
  (check-type cooked-instance-set (or cooked-instance-set cons))
  (if (typep cooked-instance-set 'cooked-instance-set)
      (mapcar #'cdr (value-of cooked-instance-set))
      cooked-instance-set))

(def generic empty-set-p (set)
  (:method ((set null))
    #t)

  (:method ((set cons))
    #f)

  (:method ((set (eql +whole-domain-marker+)))
    #f) ;; FIXME always?

  (:method ((set cooked-instance-set))
    (null (value-of set))))

;;;
;;; coordinate-set equality
;;;
(def (generic e) coordinate= (coordinate-1 coordinate-2)
  (:method (coordinate-1 coordinate-2)
    (eq coordinate-1 coordinate-2))

  ;; sets
  (:method ((coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 (eql +whole-domain-marker+)))
    #t)

  (:method ((coordinate-1 (eql +whole-domain-marker+)) coordinate-2)
    (assert #f () "Unable to compare ~A and ~A. Consider using COORDINATE-EQUAL."
            coordinate-1 coordinate-2))

  (:method (coordinate-1 (coordinate-2 (eql +whole-domain-marker+)))
    (assert #f () "Unable to compare ~A and ~A. Consider using COORDINATE-EQUAL."
            coordinate-1 coordinate-2))

  ;; sets and ranges
  (:method ((coordinate-1 cons) (coordinate-2 cons))
    (if (or (coordinate-range-p coordinate-1)
            (coordinate-range-p coordinate-2))
        (coordinate-range= coordinate-1 coordinate-2)
        (and (length= coordinate-1 coordinate-2)
             (subsetp* coordinate-1 coordinate-2 :key #'key-for)
             (subsetp* coordinate-2 coordinate-1 :key #'key-for))))

  ;; sets and ranges
  (:method ((coordinate-1 cooked-instance-set) (coordinate-2 cooked-instance-set))
    (coordinate= (value-of coordinate-1) (value-of coordinate-2))))

(def (generic e) coordinate-equal (dimension coordinate-1 coordinate-2)

  (:method (dimension coordinate-1 coordinate-2)
    (when (or (typep coordinate-1 'persistent-object)
              (typep coordinate-2 'persistent-object))
      (error "COORDINATE-EQUAL must be called with list of coordinates, but it was called with the coordinates ~S ~S" coordinate-1 coordinate-2))
    (coordinate= coordinate-1 coordinate-2))

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 (eql +whole-domain-marker+)))
    #t)

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 cons))
    (coordinate= (domain dimension) coordinate-2))

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 (eql +whole-domain-marker+)))
    (coordinate= coordinate-1 (domain dimension)))

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 cooked-instance-set))
    (coordinate= (cook-coordinate (domain dimension)) coordinate-2))

  (:method ((dimension dimension) (coordinate-1 cooked-instance-set) (coordinate-2 (eql +whole-domain-marker+)))
    (coordinate= coordinate-1 (cook-coordinate (domain dimension)))))

;;;;;;
;;; Dimension coordinate

(def (generic e) covering-coordinate-p (dimension cover coordinate)
  (:method ((dimension dimension) (cover (eql +whole-domain-marker+)) (coordinate t))
    #t)

  (:method ((dimension dimension) (cover list) (coordinate (eql +whole-domain-marker+)))
    (coordinate= cover (domain dimension)))

  (:method ((dimension dimension) (cover cooked-instance-set) (coordinate (eql +whole-domain-marker+)))
    (coordinate= cover (cook-coordinate (domain dimension))))

  (:method ((dimension dimension) (cover cons) (coordinate cons))
    (subsetp* coordinate cover :key #'key-for))

  (:method ((dimension dimension) (cover cooked-instance-set) (coordinate cooked-instance-set))
    (covering-coordinate-p dimension (value-of cover) (value-of coordinate)))

  (:method ((dimension ordering-dimension) (cover cons) (coordinate cons))
    (covering-range-p cover coordinate)))

(def (generic e) coordinate-intersection (dimension coordinate-1 coordinate-2)
  (:method ((dimension dimension) (coordinate-1 null) (coordinate-2 null))
    nil)

  (:method ((dimension dimension) (coordinate-1 null) (coordinate-2 cons))
    nil)

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 null))
    nil)

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 t))
    coordinate-2)

  (:method ((dimension dimension) (coordinate-1 t) (coordinate-2 (eql +whole-domain-marker+)))
    coordinate-1)

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 cons))
    (intersection* coordinate-1 coordinate-2 :key #'key-for))

  (:method ((dimension dimension) (coordinate-1 cooked-instance-set) (coordinate-2 cooked-instance-set))
    (make-cooked-instance-set (coordinate-intersection dimension (value-of coordinate-1) (value-of coordinate-2))))

  (:method ((dimension ordering-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (range-intersection coordinate-1 coordinate-2)))

(def (generic e) coordinate-union (dimension coordinate-1 coordinate-2)
  (:method ((dimension dimension) (coordinate-1 null) (coordinate-2 cons))
    coordinate-2)

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 null))
    coordinate-1)

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 t))
    +whole-domain-marker+)

  (:method ((dimension dimension) (coordinate-1 t) (coordinate-2 (eql +whole-domain-marker+)))
    +whole-domain-marker+)

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 cons))
    (union* coordinate-1 coordinate-2 :key #'key-for))

  (:method ((dimension dimension) (coordinate-1 cooked-instance-set) (coordinate-2 cooked-instance-set))
    (make-cooked-instance-set (coordinate-union dimension (value-of coordinate-1) (value-of coordinate-2))))

  (:method ((dimension ordering-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (range-union coordinate-1 coordinate-2)))

(def (generic e) coordinate-difference (dimension coordinate-1 coordinate-2)
  (:method ((dimension dimension) (coordinate-1 null) (coordinate-2 null))
    nil)

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 null))
    (list coordinate-1))

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 null))
    (list coordinate-1))

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 cons))
    (coordinate-difference dimension (domain dimension) coordinate-2))

  (:method ((dimension dimension) (coordinate-1 (eql +whole-domain-marker+)) (coordinate-2 cooked-instance-set))
    (coordinate-difference dimension (cook-coordinate (domain dimension)) coordinate-2))

  (:method ((dimension dimension) (coordinate-1 t) (coordinate-2 (eql +whole-domain-marker+)))
    nil)

  (:method ((dimension dimension) (coordinate-1 cons) (coordinate-2 cons))
    (awhen (set-difference* coordinate-1 coordinate-2 :key #'key-for)
      (list it)))

  (:method ((dimension dimension) (coordinate-1 cooked-instance-set) (coordinate-2 cooked-instance-set))
    (mapcar #'make-cooked-instance-set (coordinate-difference dimension (value-of coordinate-1) (value-of coordinate-2))))

  (:method ((dimension ordering-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (range-difference coordinate-1 coordinate-2)))

;;;;;;
;;; Dimensions & coordinates

(def (function e) coordinates= (coordinates-1 coordinates-2)
  (every* #'coordinate= coordinates-1 coordinates-2))

(def (function e) coordinates-equal (dimensions coordinates-1 coordinates-2)
  (every* #'coordinate-equal dimensions coordinates-1 coordinates-2))

;; FIXME ???
(def (function e) make-empty-coordinates (dimensions)
  (mapcar (constantly nil) dimensions))

(def (function e) collect-subcoordinates (dimensions sub-dimensions coordinates)
  (iter (for dimension :in dimensions)
        (for coordinate :in coordinates)
        (when (member dimension sub-dimensions)
          (collect coordinate))))

(def (function e) covering-coordinates-p (dimensions cover-coordinates coordinates)
  (iter (for dimension :in dimensions)
        (for cover-coordinate :in cover-coordinates)
        (for coordinate :in coordinates)
        (always (covering-coordinate-p dimension cover-coordinate coordinate))))

(def (function e) coordinates-intersection (dimensions coordinates-1 coordinates-2)
  (iter (for dimension :in dimensions)
        (for coordinate-1 :in coordinates-1)
        (for coordinate-2 :in coordinates-2)
        (for intersection = (coordinate-intersection dimension coordinate-1 coordinate-2))
        (if (empty-set-p intersection)
            (return-from coordinates-intersection nil)
            (collect intersection))))

(def (function e) coordinates-union (dimensions coordinates-1 coordinates-2)
  (iter (with different-index = nil)
        (with different-dimension = nil)
        (with different-coordinate-1 = nil)
        (with different-coordinate-2 = nil)
        (for index :from 0)
        (for dimension :in dimensions)
        (for coordinate-1 :in coordinates-1)
        (for coordinate-2 :in coordinates-2)
        (when (null coordinate-1)
          (return-from coordinates-union coordinates-2))
        (when (null coordinate-2)
          (return-from coordinates-union coordinates-1))
        (unless (coordinate-equal dimension coordinate-1 coordinate-2)
          (if different-index
              (return-from coordinates-union nil)
              (progn
                (setf different-index index)
                (setf different-dimension dimension)
                (setf different-coordinate-1 coordinate-1)
                (setf different-coordinate-2 coordinate-2))))
        (finally
         (return
           (if different-index
               (when-bind unified-coordinate (coordinate-union different-dimension different-coordinate-1 different-coordinate-2)
                 (aprog1 (copy-seq coordinates-1)
                   (setf (elt it different-index) unified-coordinate)))
               coordinates-1)))))

(def (function e) coordinates-difference (dimensions coordinates-1 coordinates-2)
  "Returns the difference of COORDINATES-1 and COORDINATES-2 as a coordinates list."
  (labels ((recurse (dimensions coordinates-1 coordinates-2)
             (if (length= 1 dimensions)
                 (mapcar #'list (coordinate-difference (first dimensions)
                                                       (first coordinates-1)
                                                       (first coordinates-2)))
                 (bind ((dimension (first dimensions))
                        (coordinate-1 (first coordinates-1))
                        (coordinate-2 (first coordinates-2))
                        (difference-list (coordinate-difference dimension coordinate-1 coordinate-2))
                        (intersection (coordinate-intersection dimension coordinate-1 coordinate-2)))
                   (append
                    (iter (for difference :in difference-list)
                          (when difference
                            (collect (cons difference (rest coordinates-1)))))
                    (unless (empty-set-p intersection)
                      (iter (for rest-coords :in (recurse (rest dimensions)
                                                          (rest coordinates-1)
                                                          (rest coordinates-2)))
                            (collect (cons intersection rest-coords)))))))))

    (cond
      ((null dimensions)
       nil)
      ((coordinates-intersection dimensions coordinates-1 coordinates-2)
       (recurse dimensions coordinates-1 coordinates-2))
      (t (list coordinates-1)))))
