;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/query/type :in test/query))

(def definer query-type-test (name type &optional (test-value nil test-value-p))
  (with-unique-names (value slot init-arg accessor result)
    `(def test ,(concatenate-symbol "test/query/type/" name) ()
      (let ((,value ,(when test-value-p `(with-transaction ,test-value))))
        (declare (ignorable ,value))
        (with-transaction
          (with-confirmed-destructive-schema-changes
            (ensure-exported
             (def persistent-class* query-type-test ()
               ((,name :type ,type)))))
          (purge-instances 'query-type-test))
        (bind ((,slot (find-slot (find-class 'query-type-test) ',name))
               (,init-arg (first (slot-definition-initargs ,slot)))
               (,accessor (reader-name-of ,slot)))
          (declare (ignorable ,init-arg ,accessor))
          (flet ((make-object ()
                   (apply #'make-instance
                          'query-type-test
                          ,(when test-value-p `(list ,init-arg ,value))))
                 (test-object ()
                   (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                   ,(if test-value-p
                        `(bind ((test-fn (if (typep ,value 'timestamp)
                                             'timestamp=
                                             'equal))
                                (,result (execute-query
                                            (make-query
                                             `(select ((,,accessor o))
                                                (from (o query-type-test))
                                                (where (,test-fn (,,accessor o) ',,value)))))))
                           (is (and (length= 1 ,result)
                                    (object-equal-p (first ,result) ,value))))
                        `(is (length= 1
                              (execute-query
                               (make-query
                                `(select (o)
                                   (from (o query-type-test))
                                   (where (not (slot-boundp o ',',name)))))))))
                   #+nil
                   (is (length= 1
                        (execute-query
                         (make-query
                          ,(if test-value-p
                               `(if (typep ,value 'timestamp)
                                    `(select (o)
                                       (from (o query-type-test))
                                       (where (timestamp= (,,accessor o) ',,value)))
                                    `(select (o)
                                       (from (o query-type-test))
                                       (where (equal (,,accessor o) ',,value))))
                               ``(select (o)
                                   (from (o query-type-test))
                                   (where (not (slot-boundp o ',',name)))))))))))
            (with-transaction
              (when (hu.dwim.perec::persistent-object-p ,value)
                (revive-instance ,value))
              (make-object)
              (test-object))))))))

(def query-type-test t/1 t)
(def query-type-test t/2 t nil)
(def query-type-test t/3 t t)
(def query-type-test t/4 t #f)
(def query-type-test t/5 t #t)
(def query-type-test t/6 t 0)
(def query-type-test t/7 t 0.1)
(def query-type-test t/8 t "something")
(def query-type-test t/9 t 'something)
(def query-type-test t/10 t (make-structure-type-test))
(def query-type-test t/11 t (make-instance 'standard-class-type-test))
(def query-type-test t/12 t (make-instance 'persistent-class-type-test))
(def query-type-test t/13 t (list nil #f #t 0 0.1 "something" 'something
                          (make-structure-type-test)
                          (make-instance 'standard-class-type-test)
                          (make-instance 'persistent-class-type-test)))

(def query-type-test serialized/1 serialized (list nil #f #t 0 0.1 "something" 'something
                                                  (make-structure-type-test)
                                                  (make-instance 'standard-class-type-test)
                                                  (make-instance 'persistent-class-type-test)))
(def query-type-test serialized/2 (serialized 32) t)


(def query-type-test boolean/1 boolean #t)
(def query-type-test boolean/2 boolean #f)

(def query-type-test integer-16/1 integer-16 0)
(def query-type-test integer-16/2 integer-16 +1)
(def query-type-test integer-16/3 integer-16 -1)
(def query-type-test integer-16/4 integer-16 (1- (expt 2 15)))
(def query-type-test integer-16/5 integer-16 (- (expt 2 15)))

(def query-type-test integer-32/1 integer-32 0)
(def query-type-test integer-32/2 integer-32 +1)
(def query-type-test integer-32/3 integer-32 -1)
(def query-type-test integer-32/4 integer-32 (1- (expt 2 31)))
(def query-type-test integer-32/5 integer-32 (- (expt 2 31)))

(def query-type-test integer-64/1 integer-64 0)
(def query-type-test integer-64/2 integer-64 +1)
(def query-type-test integer-64/3 integer-64 -1)
(def query-type-test integer-64/4 integer-64 (1- (expt 2 63)))
(def query-type-test integer-64/5 integer-64 (- (expt 2 63)))

(def query-type-test integer/1 integer 0)
(def query-type-test integer/2 integer +1)
(def query-type-test integer/3 integer -1)
(def query-type-test integer/4 integer (expt 2 128))
(def query-type-test integer/5 integer (- (expt 2 128)))

(def query-type-test float-32/1 float-32 0)
(def query-type-test float-32/2 float-32 +1.5)
(def query-type-test float-32/3 float-32 -1.5)

(def query-type-test float-64/1 float-64 0)
(def query-type-test float-64/2 float-64 +1.5)
(def query-type-test float-64/3 float-64 -1.5)

(def query-type-test number/1 number 0)
(def query-type-test number/2 number +1.5)
(def query-type-test number/3 number -1.5)
(def query-type-test number/4 number 1/10)
(def query-type-test number/5 number -1/10)
(def query-type-test number/6 number (expt 2 128))
(def query-type-test number/7 number (- (expt 2 128)))

(def query-type-test string/1 string "")
(def query-type-test string/2 string (random-string 10000))
(def query-type-test string/3 (string 20) "                    ")
(def query-type-test string/4 (string 20) "something           ")

(def query-type-test symbol/1 symbol 'something)
(def query-type-test symbol/2 symbol 'hu.dwim.perec.test::something)
(def query-type-test symbol/3 (symbol* 30) 'hu.dwim.perec.test::something)

(def query-type-test date/1 date (parse-timestring "2006-06-06TZ"))

(def query-type-test time-of-day/1 time-of-day (parse-timestring "06:06:06Z"))

(def query-type-test timestamp/1 timestamp (parse-timestring "2006-06-06T06:06:06Z"))
(def query-type-test timestamp/2 timestamp (parse-timestring "2006-06-06T06:06:06.123456Z"))

(def query-type-test duration/1 duration (+ (* 5 60 60) (* 5 60) 5))
(def query-type-test duration/2 duration (+ (* 1 (* +seconds-per-day+ 30)) (* 1 +seconds-per-day+) (* 5 +seconds-per-hour+) (* 5 +seconds-per-minute+) 5))

(def query-type-test member/1 (member one two three) 'one)
(def query-type-test member/2 (member one two three) 'two)
(def query-type-test member/3 (member one two three) 'three)

(def query-type-test form/1 form '(progn 1 (print "Hello") 2))
(def query-type-test form/2 (form 100) '(progn 1 (print "Hello") 2))

(def query-type-test or-null-string/1 (or null string) nil)
(def query-type-test or-null-string/2 (or null string) "something")

(def query-type-test or-unbound-string/1 (or unbound string))
(def query-type-test or-unbound-string/2 (or unbound string) "something")

(def query-type-test or-unbound-null-string/1 (or unbound null string))
(def query-type-test or-unbound-null-string/2 (or unbound null string) nil)
(def query-type-test or-unbound-null-string/3 (or unbound null string) "something")
