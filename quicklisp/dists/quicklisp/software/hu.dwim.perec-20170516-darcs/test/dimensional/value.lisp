;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; D value (multi dimensional value)

(def suite* (test/dimensional/value :in test/dimensional))

(def (dimension e) inherited
  :type fixnum
  :inherit :ascending
  :bind-default-coordinate #f
  :default-coordinate-begin 0
  :default-coordinate-end 0
  :minimum-coordinate most-negative-fixnum
  :maximum-coordinate most-positive-fixnum)

;;;;;;
;;; Validity dimension

(def (dimension e) ordered
  :type fixnum
  :ordered #t
  :default-coordinate-begin most-negative-fixnum
  :default-coordinate-end most-positive-fixnum
  :minimum-coordinate most-negative-fixnum
  :maximum-coordinate most-positive-fixnum)


(def special-variable *test-dimensions* (mapcar #'lookup-dimension '(inherited ordered enumerated)))

(def function is-not-null (value)
  (is (not (null value)) "The value should not be nil here")
  value)

;;;;;;
;;; Range

(def test test/dimensional/value/range-intersection ()
  (bind ((dimension (find-dimension 'ordered)))
    (is (null (coordinate-intersection dimension '(ie 0 . 10) '(ie 10 . 20))))
    (is (null (coordinate-intersection dimension '(ie 0 . 10) '(ii 10 . 10))))
    (is (equal '(ii 0 . 0) (coordinate-intersection dimension '(ie 0 . 10) '(ii 0 . 0))))
    (is (equal '(ie 5 . 10) (coordinate-intersection dimension '(ie 0 . 10) '(ie 5 . 15))))
    (is (equal '(ie 0 . 5) (coordinate-intersection dimension '(ie 0 . 10) '(ie -5 . 5))))
    (is (equal '(ie 0 . 10) (coordinate-intersection dimension '(ie 0 . 10) '(ie -5 . 15))))))

(def test test/dimensional/value/range-union ()
  (bind ((dimension (find-dimension 'ordered)))
    (is (equal '(ie 0 . 20) (coordinate-union dimension '(ie 0 . 10) '(ie 10 . 20))))
    (is (equal '(ii 0 . 10) (coordinate-union dimension '(ie 0 . 10) '(ii 10 . 10))))
    (is (equal '(ie 0 . 10) (coordinate-union dimension '(ie 0 . 10) '(ii 0 . 0))))
    (is (equal '(ie 0 . 15) (coordinate-union dimension '(ie 0 . 10) '(ie 5 . 15))))
    (is (equal '(ie -5 . 10) (coordinate-union dimension '(ie 0 . 10) '(ie -5 . 5))))
    (is (equal '(ie -5 . 15) (coordinate-union dimension '(ie 0 . 10) '(ie -5 . 15))))))

(def test test/dimensional/value/range-difference ()
  (bind ((dimension (find-dimension 'ordered)))
    (is (equal '((ie 0 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ie 10 . 20))))
    (is (equal '((ie 0 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ii 10 . 10))))
    (is (equal '((ee 0 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ii 0 . 0))))
    (is (equal '((ie 0 . 5)) (coordinate-difference dimension '(ie 0 . 10) '(ie 5 . 15))))
    (is (equal '((ie 5 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ie -5 . 5))))
    (is (equal '((ie 0 . 1) (ie 9 . 10)) (coordinate-difference dimension '(ie 0 . 10) '(ie 1 . 9))))
    (is (null (coordinate-difference dimension '(ie 0 . 10) '(ie -5 . 15))))))

;;;;;;
;;; Coordinates union

(def test test/dimensional/value/coordinates-a-union-empty-is-a (dimensions coordinates-a)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         (make-empty-coordinates dimensions)))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-union-a-is-a (dimensions coordinates-a)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         coordinates-a))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-union-b-is-b-union-a (dimensions coordinates-a coordinates-b)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         coordinates-b))
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-b
                                                         coordinates-a)))))

(def test test/dimensional/value/coordinates-a-union-b-union-c-is-a-union-b-union-c (dimensions coordinates-a coordinates-b coordinates-c)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-union dimensions
                                                         (is-not-null (coordinates-union dimensions
                                                                                         coordinates-a
                                                                                         coordinates-b))
                                                         coordinates-c))
                         (is-not-null (coordinates-union dimensions
                                                         coordinates-a
                                                         (is-not-null (coordinates-union dimensions
                                                                                         coordinates-b
                                                                                         coordinates-c)))))))

(def test test/dimensional/value/coordinates-union-invariants ()
  (test/dimensional/value/coordinates-a-union-empty-is-a *test-dimensions* '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-union-a-is-a *test-dimensions* '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-union-b-is-b-union-a *test-dimensions*
                                                             '((ie 10 . 20) (ie 0 . 100) (a))
                                                             '((ie 20 . 40) (ie 0 . 100) (a)))
  (test/dimensional/value/coordinates-a-union-b-union-c-is-a-union-b-union-c *test-dimensions*
                                                                             '((ie 10 . 20) (ie 0 . 100) (a))
                                                                             '((ie 20 . 40) (ie 0 . 100) (a))
                                                                             '((ie 40 . 80) (ie 0 . 100) (a))))

;;;;;;
;;; Coordinates intersecion

(def test test/dimensional/value/coordinates-a-intersection-empty-is-empty (dimensions coordinates-a)
  (is (null (coordinates-intersection dimensions
                                      coordinates-a
                                      (make-empty-coordinates dimensions)))))

(def test test/dimensional/value/coordinates-a-intersection-a-is-a (dimensions coordinates-a)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                coordinates-a))
                         coordinates-a)))

(def test test/dimensional/value/coordinates-a-intersection-b-is-b-intersection-a (dimensions coordinates-a coordinates-b)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                coordinates-b))
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-b
                                                                coordinates-a)))))

(def test test/dimensional/value/coordinates-a-intersection-b-intersection-c-is-a-intersection-b-intersection-c (dimensions coordinates-a coordinates-b coordinates-c)
  (is (coordinates-equal dimensions
                         (is-not-null (coordinates-intersection dimensions
                                                                (is-not-null (coordinates-intersection dimensions
                                                                                                       coordinates-a
                                                                                                       coordinates-b))
                                                                coordinates-c))
                         (is-not-null (coordinates-intersection dimensions
                                                                coordinates-a
                                                                (is-not-null (coordinates-intersection dimensions
                                                                                                       coordinates-b
                                                                                                       coordinates-c)))))))

(def test test/dimensional/value/coordinates-intersection-invariants ()
  (test/dimensional/value/coordinates-a-intersection-empty-is-empty
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-intersection-a-is-a
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-intersection-b-is-b-intersection-a
   *test-dimensions*
   '((ie 10 . 40) (ie 0 . 100) (a b))
   '((ie 20 . 60) (ie 50 . 200) (a c)))
  (test/dimensional/value/coordinates-a-intersection-b-intersection-c-is-a-intersection-b-intersection-c
   *test-dimensions*
   '((ie 10 . 40) (ie 0 . 100) (a b c))
   '((ie 20 . 60) (ie 50 . 200) (b c d))
   '((ie 0 . 100) (ie 20 . 60) (c))))

;;;;;;
;;; Coordinates difference

(def test test/dimensional/value/coordinates-a-difference-empty-is-a (dimensions coordinates-a)
  (is (hu.dwim.perec::every* [coordinates-equal dimensions !1 !2]
                   (is-not-null (coordinates-difference dimensions
                                                        coordinates-a
                                                        (make-empty-coordinates dimensions)))
                   (list coordinates-a))))

(def test test/dimensional/value/coordinates-a-difference-a-is-empty (dimensions coordinates-a)
  (is (null (coordinates-difference dimensions
                                    coordinates-a
                                    coordinates-a))))

(def test test/dimensional/value/coordinates-difference-invariants ()
  (test/dimensional/value/coordinates-a-difference-empty-is-a
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b)))
  (test/dimensional/value/coordinates-a-difference-a-is-empty
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a b))))

(def test test/dimensional/value/coordinates-difference (dimensions coordinates-a coordinates-b expected)
  (is (null (set-exclusive-or
             (coordinates-difference dimensions coordinates-a coordinates-b)
             expected
             :test [coordinates-equal dimensions !1 !2]))))

(def test test/dimensional/value/coordinates-difference-samples ()
  (test/dimensional/value/coordinates-difference
   *test-dimensions*
   '((ie 10 . 20) (ie 0 . 100) (a))
   '((ie 10 . 20) (ie 10 . 90) (a))
   '(((ie 10 . 20) (ie 0 . 10) (a))
     ((ie 10 . 20) (ie 90 . 100) (a))))

  (test/dimensional/value/coordinates-difference
   (mapcar 'find-dimension '(enumerated ordered))
   `((a) (ie 0 . 100))
   `((a) (ie 10 . 90))
   `(((a) (ie 0 . 10))
     ((a) (ie 90 . 100))))

  (test/dimensional/value/coordinates-difference
   (mapcar 'find-dimension '(enumerated ordered))
   `((a) (ie ,most-negative-fixnum . ,most-positive-fixnum))
   `((a) (ie 2008 . 2009))
   `(((a) (ie ,most-negative-fixnum . 2008))
     ((a) (ie 2009 . ,most-positive-fixnum)))))

(def test test/dimensional/value/d-value-equal/positive ()
  (is (d-value-equal
            (make-single-d-value
             '(inherited ordered)
             (list
              (make-empty-coordinate-range most-positive-fixnum)
              (make-coordinate-range 'ie most-negative-fixnum most-positive-fixnum))
             1)
            (make-single-d-value
             '(inherited ordered)
             (list
              (make-empty-coordinate-range most-positive-fixnum)
              (make-coordinate-range 'ie most-negative-fixnum most-positive-fixnum))
             1)))
  (is (d-value-equal
       (make-empty-d-value '(inherited ordered))
       (make-empty-d-value '(inherited ordered)))))

(def test test/dimensional/value/d-value-equal/negative ()
  (is (not (d-value-equal
            (make-single-d-value '(inherited ordered)
                                 (list
                                  (make-empty-coordinate-range most-positive-fixnum)
                                  (make-coordinate-range 'ie most-negative-fixnum most-positive-fixnum))
                                 1)
            (make-single-d-value '(inherited ordered)
                                 (list
                                  (make-empty-coordinate-range most-positive-fixnum)
                                  (make-coordinate-range 'ie most-negative-fixnum most-positive-fixnum))
                                 2))))
  (is (not (d-value-equal
            (make-empty-d-value '(inherited ordered))
            (make-single-d-value '(inherited ordered)
                                 (list
                                  (make-empty-coordinate-range most-positive-fixnum)
                                  (make-coordinate-range 'ie most-negative-fixnum most-positive-fixnum))
                                 2))))
  (is (not (d-value-equal
            (make-single-d-value '(inherited ordered)
                                 (list
                                  (make-empty-coordinate-range most-positive-fixnum)
                                  (make-coordinate-range 'ie most-negative-fixnum most-positive-fixnum))
                                 1)
            (make-empty-d-value '(inherited ordered))))))

(def test test/dimensional/value/iter-in-d-values (d-value-1 d-value-2 expected-d-values)
  (iter (with dimensions = (hu.dwim.perec::dimensions-of d-value-1))
        (with test = [d-value-equal !1 !2 :test #'equal])
        (for (coordinates values) :in-d-values (d-value-1 d-value-2) :unspecified-value nil)
        (for d-value = (make-single-d-value dimensions coordinates values))
        (is (member d-value expected-d-values :test test) "Unexpected: ~S" d-value)
        (removef expected-d-values d-value :test test)
        (finally (is (null expected-d-values) "Missing: ~S" expected-d-values))))

(def test test/dimensional/value/iter-in-d-values/1 ()
  (bind ((dimensions (list *time-dimension* *validity-dimension*)))
    (test/dimensional/value/iter-in-d-values
     (make-single-d-value
      dimensions
      (list (make-empty-coordinate-range +end-of-time+)
            (make-coordinate-range 'ie +beginning-of-time+ (parse-datestring "2001-01-01")))
      1)
     (make-single-d-value
      dimensions
      (list (make-coordinate-range 'ii (parse-datestring "2000-01-01") +end-of-time+)
            (make-coordinate-range 'ie (parse-datestring "2000-01-01") +end-of-time+))
      2)
     (list
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range 'ie +beginning-of-time+ (parse-datestring "2000-01-01")))
       (list 1 nil))
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range 'ie (parse-datestring "2000-01-01") (parse-datestring "2001-01-01")))
       (list 1 2))
      (make-single-d-value
       dimensions
       (list (make-empty-coordinate-range +end-of-time+)
             (make-coordinate-range 'ie (parse-datestring "2001-01-01") +end-of-time+))
       (list nil 2))
      (make-single-d-value
       dimensions
       (list (make-coordinate-range 'ie (parse-datestring "2000-01-01") +end-of-time+)
             (make-coordinate-range 'ie (parse-datestring "2000-01-01") +end-of-time+))
       (list nil 2))))))

(def test test/dimensional/value/d-project (d-value projection-dimensions expected-d-value)
  (is (d-value-equal
       (d-project
        (lambda (dimensions coordinates-list values)
          (d-volume (make-d-value dimensions coordinates-list values)))
        (mapcar #'lookup-dimension projection-dimensions)
        d-value)
       expected-d-value)))

(def test test/dimensional/value/d-project/1 ()
  (bind ((d-value (make-d-value *test-dimensions*
                                ;; inherited ordered enumerated
                                '(((ie 0 . 1) (ie 0 . 1) (a))
                                  ((ie 0 . 1) (ie 1 . 2) (a))
                                  ((ie 1 . 2) (ie 0 . 2) (a))
                                  ((ie 0 . 1) (ie 0 . 2) (b))
                                  ((ie 1 . 2) (ie 0 . 1) (b))
                                  ((ie 1 . 2) (ie 1 . 2) (b)))
                                '(1 2 3 4 5 6))))
    (test/dimensional/value/d-project d-value '() (make-d-value '()
                                                                '(())
                                                                `(,(+ (* 1 1) (* 1 2) (* 2 3) (* 2 4) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(ordered enumerated)
                                      (make-d-value '(ordered enumerated)
                                                    '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                    `(,(+ (* 1 1) (* 1 3))
                                                      ,(+ (* 1 2) (* 1 3))
                                                      ,(+ (* 1 4) (* 1 5))
                                                      ,(+ (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(inherited enumerated)
                                      (make-d-value '(inherited enumerated)
                                                    '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                    `(,(+ (* 1 1) (* 1 2))
                                                      ,(+ (* 2 3))
                                                      ,(+ (* 2 4))
                                                      ,(+ (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(inherited ordered)
                                      (make-d-value '(inherited ordered)
                                                    '(((ie 0 . 1) (ie 0 . 1)) ((ie 0 . 1) (ie 1 . 2))
                                                      ((ie 1 . 2) (ie 0 . 1)) ((ie 1 . 2) (ie 1 . 2)))
                                                    `(,(+ (* 1 1) (* 1 4))
                                                      ,(+ (* 1 2) (* 1 4))
                                                      ,(+ (* 1 3) (* 1 5))
                                                      ,(+ (* 1 3) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(inherited)
                                      (make-d-value '(inherited)
                                                    '(((ie 0 . 1)) ((ie 1 . 2)))
                                                    `(,(+ (* 1 1) (* 1 2) (* 2 4))
                                                      ,(+ (* 2 3) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(ordered)
                                      (make-d-value '(ordered)
                                                    '(((ie 0 . 1)) ((ie 1 . 2)))
                                                    `(,(+ (* 1 1) (* 1 3) (* 1 4) (* 1 5))
                                                      ,(+ (* 1 2) (* 1 3) (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-project d-value '(enumerated)
                                      (make-d-value '(enumerated)
                                                    '(((a)) ((b)))
                                                    `(,(+ (* 1 1) (* 1 2) (* 2 3))
                                                      ,(+ (* 2 4) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-project d-value '()
                                      (make-d-value '()
                                                    '(())
                                                    `(,(+ (* 1 1) (* 1 2) (* 2 3) (* 2 4) (* 1 5) (* 1 6)))))))

(def test test/dimensional/value/d-fold (d-value folded-dimensions expected-d-value)
  (setf folded-dimensions (mapcar #'lookup-dimension folded-dimensions))
  (is (d-value-equal
       (d-fold
        (lambda (accumulator folded-coordinates value)
          (+ accumulator
             (d-volume (make-single-d-value folded-dimensions folded-coordinates value))))
        folded-dimensions
        d-value
        :initial-value 0)
       expected-d-value)))

(def test test/dimensional/value/d-fold/1 ()
  (bind ((d-value (make-d-value *test-dimensions*
                                '(((ie 0 . 1) (ie 0 . 1) (a))
                                  ((ie 0 . 1) (ie 1 . 2) (a))
                                  ((ie 1 . 2) (ie 0 . 2) (a))
                                  ((ie 0 . 1) (ie 0 . 2) (b))
                                  ((ie 1 . 2) (ie 0 . 1) (b))
                                  ((ie 1 . 2) (ie 1 . 2) (b)))
                                '(1 2 3 4 5 6))))
    (test/dimensional/value/d-fold d-value '() d-value)
    (test/dimensional/value/d-fold d-value '(inherited)
                                   (make-d-value '(ordered enumerated)
                                                 '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                 `(,(+ (* 1 1) (* 1 3))
                                                   ,(+ (* 1 2) (* 1 3))
                                                   ,(+ (* 1 4) (* 1 5))
                                                   ,(+ (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(ordered)
                                   (make-d-value '(inherited enumerated)
                                                 '(((ie 0 . 1) (a)) ((ie 1 . 2) (a)) ((ie 0 . 1) (b)) ((ie 1 . 2) (b)))
                                                 `(,(+ (* 1 1) (* 1 2))
                                                   ,(+ (* 2 3))
                                                   ,(+ (* 2 4))
                                                   ,(+ (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(enumerated)
                                   (make-d-value '(inherited ordered)
                                                 '(((ie 0 . 1) (ie 0 . 1)) ((ie 0 . 1) (ie 1 . 2))
                                                   ((ie 1 . 2) (ie 0 . 1)) ((ie 1 . 2) (ie 1 . 2)))
                                                 `(,(+ (* 1 1) (* 1 4))
                                                   ,(+ (* 1 2) (* 1 4))
                                                   ,(+ (* 1 3) (* 1 5))
                                                   ,(+ (* 1 3) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(ordered enumerated)
                                   (make-d-value '(inherited)
                                                 '(((ie 0 . 1)) ((ie 1 . 2)))
                                                 `(,(+ (* 1 1) (* 1 2) (* 2 4))
                                                   ,(+ (* 2 3) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(inherited enumerated)
                                   (make-d-value '(ordered)
                                                 '(((ie 0 . 1)) ((ie 1 . 2)))
                                                 `(,(+ (* 1 1) (* 1 3) (* 1 4) (* 1 5))
                                                   ,(+ (* 1 2) (* 1 3) (* 1 4) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(inherited ordered)
                                   (make-d-value '(enumerated)
                                                 '(((a)) ((b)))
                                                 `(,(+ (* 1 1) (* 1 2) (* 2 3))
                                                   ,(+ (* 2 4) (* 1 5) (* 1 6)))))
    (test/dimensional/value/d-fold d-value '(inherited ordered enumerated)
                                   (make-d-value '()
                                                 '(())
                                                 `(,(+ (* 1 1) (* 1 2) (* 2 3) (* 2 4) (* 1 5) (* 1 6)))))))
