;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Coordinate

(def (type e) coordinate ()
  t)

(def (function e) coordinate-p (value)
  (typep value 'coordinate))

(def generic key-for (element)
  (:method (element)
    element)

  (:method ((instance persistent-object))
    (oid-of instance))

  ;; FIXME fragile
  (:method ((oid-and-instance cons))
    (car oid-and-instance)))

(def (generic e) ordering-coordinate= (coordinate-1 coordinate-2)
  (:method (coordinate-1 coordinate-2)
    (eql coordinate-1 coordinate-2))

  (:method ((coordinate-1 number) (coordinate-2 number))
    (= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp= coordinate-1 coordinate-2)))

(def (generic e) coordinate< (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (< coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp< coordinate-1 coordinate-2)))

(def (generic e) coordinate<= (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (<= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp<= coordinate-1 coordinate-2)))

(def (generic e) coordinate> (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (> coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp> coordinate-1 coordinate-2)))

(def (generic e) coordinate>= (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (>= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp>= coordinate-1 coordinate-2)))

(def (function e) coordinate-min (coordinate-1 coordinate-2)
  (if (coordinate< coordinate-1 coordinate-2)
      coordinate-1
      coordinate-2))

(def (function e) coordinate-max (coordinate-1 coordinate-2)
  (if (coordinate< coordinate-1 coordinate-2)
      coordinate-2
      coordinate-1))

