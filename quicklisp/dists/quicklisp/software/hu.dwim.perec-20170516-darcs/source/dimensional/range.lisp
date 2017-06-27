;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Coordinate range

(def (type e) bounds ()
  '(member ii ie ei ee))

(def (constant e) +ii-bounds+ 'ii)

(def (constant e) +ie-bounds+ 'ie)

(def (constant e) +ei-bounds+ 'ei)

(def (constant e) +ee-bounds+ 'ee)

(export '(ii ie ei ee))

(def (function ioe) make-bounds (begin-inclusive end-inclusive)
  (cond
    ((and begin-inclusive end-inclusive) 'ii)
    (begin-inclusive 'ie)
    (end-inclusive 'ei)
    (t 'ee)))

(def (function ioe) begin-inclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ii ie)))

(def (function ioe) begin-exclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ei ee)))

(def (function ioe) end-inclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ii ei)))

(def (function ioe) end-exclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ie ee)))

(def (type e) coordinate-range ()
  '(cons bounds (cons coordinate coordinate)))

(def (function e) coordinate-range-p (value)
  (and (consp value)
       (consp (cdr value))
       (bind ((bounds (coordinate-range-bounds value))
              (begin (coordinate-range-begin value))
              (end (coordinate-range-end value)))
         (and (typep bounds 'bounds)
              (not (null begin))
              (not (null end))
              (not (consp begin))
              (not (consp end))))))

(def function assert-coordinate-ranges (range-1 range-2)
  (assert (coordinate-range-p range-1))
  (assert (coordinate-range-p range-2)))

(def (function ioe) make-coordinate-range (bounds begin end)
  (debug-only
    (assert (typep bounds 'bounds))
    (assert (and (coordinate-p begin) (coordinate-p end)))
    (assert (coordinate<= begin end))
    (assert (or (coordinate< begin end) (eq bounds 'ii))))
  (cons bounds (cons begin end)))

(def (function io) make-coordinate-range* (bounds begin end)
  (when (or (coordinate< begin end)
            (and (ordering-coordinate= begin end)
                 (eq bounds 'ii)))
    (debug-only (assert (typep bounds 'bounds)))
    (cons bounds (cons begin end))))

(def (function ioe) make-empty-coordinate-range (coordinate)
  (debug-only (assert (coordinate-p coordinate)))
  (cons 'ii (cons coordinate coordinate)))

(def (function ioe) make-ie-coordinate-range (begin &optional (end begin))
  (make-coordinate-range (make-bounds #t #f) begin end))

(def (function ioe) make-ii-coordinate-range (begin &optional (end begin))
  (make-coordinate-range (make-bounds #t #t) begin end))

(def (function ioe) coordinate-range-bounds (range)
  (car range))

(def (function ioe) coordinate-range-begin (range)
  (cadr range))

(def (function ioe) coordinate-range-end (range)
  (cddr range))

(def (function ioe) coordinate-range-empty-p (range)
  (and (eq 'ii (coordinate-range-bounds range))
       (ordering-coordinate= (coordinate-range-begin range)
                             (coordinate-range-end range))))

(def (function e) single-coordinate-range-value (range)
  (assert (coordinate-range-empty-p range))
  (coordinate-range-begin range))

(def function in-coordinate-range-p (coordinate range)
  (ecase (coordinate-range-bounds range)
    (ii (and (coordinate<= (coordinate-range-begin range) coordinate)
             (coordinate<= coordinate (coordinate-range-end range))))
    (ie (and (coordinate<= (coordinate-range-begin range) coordinate)
             (coordinate< coordinate (coordinate-range-end range))))
    (ei (and (coordinate< (coordinate-range-begin range) coordinate)
             (coordinate<= coordinate (coordinate-range-end range))))
    (ee (and (coordinate< (coordinate-range-begin range) coordinate)
             (coordinate< coordinate (coordinate-range-end range))))))

(def function coordinate-range= (range-1 range-2)
  (debug-only (assert-coordinate-ranges range-1 range-2))
  (and (eq (coordinate-range-bounds range-1)
           (coordinate-range-bounds range-2))
       (ordering-coordinate= (coordinate-range-begin range-1)
                             (coordinate-range-begin range-2))
       (ordering-coordinate= (coordinate-range-end range-1)
                             (coordinate-range-end range-2))))

(def (function e) overlapping-range-p (range-1 range-2)
  (debug-only (assert-coordinate-ranges range-1 range-2))
  (bind ((bounds-1 (coordinate-range-bounds range-1))
         (bounds-2 (coordinate-range-bounds range-2)))
    (and (funcall (if (and (begin-inclusive-p bounds-1)
                           (end-inclusive-p bounds-2))
                     #'coordinate<=
                     #'coordinate<)
                 (coordinate-range-begin range-1)
                 (coordinate-range-end range-2))
         (funcall (if (and (end-inclusive-p bounds-1)
                           (begin-inclusive-p bounds-2))
                     #'coordinate<=
                     #'coordinate<)
                 (coordinate-range-begin range-2)
                 (coordinate-range-end range-1)))))

(def function covering-range-p (cover range)
  (debug-only (assert-coordinate-ranges cover range))
  (bind ((cover-bounds (coordinate-range-bounds cover))
         (range-bounds (coordinate-range-bounds range)))
    (and (funcall (if (and (begin-exclusive-p cover-bounds)
                           (begin-inclusive-p range-bounds))
                      #'coordinate<
                      #'coordinate<=)
                  (coordinate-range-begin cover)
                  (coordinate-range-begin range))
         (funcall (if (and (end-exclusive-p cover-bounds)
                           (end-inclusive-p range-bounds))
                      #'coordinate<
                      #'coordinate<=)
                  (coordinate-range-end range)
                  (coordinate-range-end cover)))))

(def function range-intersection (range-1 range-2)
  (debug-only (assert-coordinate-ranges range-1 range-2))
  (when (overlapping-range-p range-1 range-2)
    (bind ((begin-range (cond
                          ((coordinate< (coordinate-range-begin range-1)
                                        (coordinate-range-begin range-2))
                           range-2)
                          ((coordinate< (coordinate-range-begin range-2)
                                        (coordinate-range-begin range-1))
                           range-1)
                          ((begin-exclusive-p (coordinate-range-bounds range-1))
                           range-1)
                          (t
                           range-2)))
           (end-range (cond
                        ((coordinate< (coordinate-range-end range-1)
                                      (coordinate-range-end range-2))
                         range-1)
                        ((coordinate< (coordinate-range-end range-2)
                                      (coordinate-range-end range-1))
                         range-2)
                        ((end-exclusive-p (coordinate-range-bounds range-1))
                         range-1)
                        (t
                         range-2))))
      (make-coordinate-range
       (make-bounds (begin-inclusive-p (coordinate-range-bounds begin-range))
                    (end-inclusive-p (coordinate-range-bounds end-range)))
       (coordinate-range-begin begin-range)
       (coordinate-range-end end-range)))))

(def function range-union (range-1 range-2)
  (debug-only (assert-coordinate-ranges range-1 range-2))
  (when (or (overlapping-range-p range-1 range-2)
            (and (ordering-coordinate= (coordinate-range-end range-1)
                                       (coordinate-range-begin range-2))
                 (or (end-inclusive-p (coordinate-range-bounds range-1))
                     (begin-inclusive-p (coordinate-range-bounds range-2))))
            (and (ordering-coordinate= (coordinate-range-end range-2)
                                       (coordinate-range-begin range-1))
                 (or (end-inclusive-p (coordinate-range-bounds range-2))
                     (begin-inclusive-p (coordinate-range-bounds range-1)))))
    (bind ((begin-range (cond
                          ((coordinate< (coordinate-range-begin range-1)
                                        (coordinate-range-begin range-2))
                           range-1)
                          ((coordinate< (coordinate-range-begin range-2)
                                        (coordinate-range-begin range-1))
                           range-2)
                          ((begin-inclusive-p (coordinate-range-bounds range-1))
                           range-1)
                          (t
                           range-2)))
           (end-range (cond
                        ((coordinate< (coordinate-range-end range-1)
                                      (coordinate-range-end range-2))
                         range-2)
                        ((coordinate< (coordinate-range-end range-2)
                                      (coordinate-range-end range-1))
                         range-1)
                        ((end-inclusive-p (coordinate-range-bounds range-1))
                         range-1)
                        (t
                         range-2))))
      (make-coordinate-range
       (make-bounds (begin-inclusive-p (coordinate-range-bounds begin-range))
                    (end-inclusive-p (coordinate-range-bounds end-range)))
       (coordinate-range-begin begin-range)
       (coordinate-range-end end-range)))))

(def function range-difference (range-1 range-2)
  (debug-only (assert-coordinate-ranges range-1 range-2))
  (cond
    ((covering-range-p range-2 range-1)
     nil)
    ((overlapping-range-p range-1 range-2)
     (bind ((intersection (range-intersection range-1 range-2))
            (difference-1 (make-coordinate-range*
                           (make-bounds (begin-inclusive-p (coordinate-range-bounds range-1))
                                        (begin-exclusive-p (coordinate-range-bounds intersection)))
                           (coordinate-range-begin range-1)
                           (coordinate-range-begin intersection)))
            (difference-2 (make-coordinate-range*
                           (make-bounds (end-exclusive-p (coordinate-range-bounds intersection))
                                        (end-inclusive-p (coordinate-range-bounds range-1)))
                           (coordinate-range-end intersection)
                           (coordinate-range-end range-1))))
       (cond ((and difference-1 difference-2)
              (list difference-1 difference-2))
             (difference-1
              (list difference-1))
             (difference-2
              (list difference-2))
             (t
              nil))))
    (t (list range-1))))

