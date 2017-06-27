;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; Canonical

(def suite* (test/persistence/canonical :in test/persistence))

(def test test/persistence/canonical/type (type canonical-type)
  (is (equalp canonical-type (canonical-type-for type))))

(def definer canonical-type-test (name type canonical-type)
  `(def test ,(concatenate-symbol "test/persistence/canonical/" name) ()
    (test/persistence/canonical/type ',type ',canonical-type)))

(def canonical-type-test null/1 null null)

(def canonical-type-test unbound/1 unbound unbound)

(def canonical-type-test boolean/1 boolean boolean)
(def canonical-type-test boolean/2 (and (not unbound) boolean) boolean)

(def canonical-type-test integer/1 integer integer)
(def canonical-type-test integer/2 (and (not unbound) (not null) integer) integer)

(def canonical-type-test float/1 float float)
(def canonical-type-test float/2 (and (not unbound) (not null) float) float)

(def canonical-type-test double/1 double double)
(def canonical-type-test double/2 (and (not unbound) (not null) double) double)

(def canonical-type-test number/1 number number)
(def canonical-type-test number/2 (and (not unbound) (not null) number) number)

(def canonical-type-test string/1 string string)
(def canonical-type-test string/2 (and (not unbound) (not null) string) string)

(def canonical-type-test symbol/1 symbol symbol)
(def canonical-type-test symbol/2 (and (not unbound) symbol) symbol)

(def canonical-type-test member/1 (member a b c) (member a b c))

(def persistent-type t1 ()
    ()
  '(member a b c))

(def canonical-type-test complex/1
  (or null unbound t1)
  (or null
      unbound
      (member a b c)))

(def canonical-type-test complex/2
  (and (not null)
       (not unbound)
       (or null unbound t1))
  (member a b c))

(def persistent-type t2 ()
    ()
  '(or null unbound t1))

(def canonical-type-test complex/3
  (and (not null)
       (not unbound)
       t2)
  (member a b c))

;;;;;;
;;; Normalized

(def suite* (test/persistence/normalized :in test/persistence))

(def test test/persistence/normalized/type (type normalized-type)
  (is (equalp normalized-type (normalized-type-for type))))

(def definer normalized-type-test (name type normalized-type)
  `(def test ,(concatenate-symbol "test/persistence/normalized/" name) ()
    (test/persistence/normalized/type ',type ',normalized-type)))

(def normalized-type-test null/1 null nil)

(def normalized-type-test unbound/1 unbound nil)

(def normalized-type-test t/1 t t)

(def normalized-type-test serialized/1 serialized serialized)
(def normalized-type-test serialized/2 (or unbound serialized) serialized)
(def normalized-type-test serialized/3 (or null serialized) serialized)
(def normalized-type-test serialized/4 (or unbound null serialized) serialized)

(def normalized-type-test boolean/1 boolean boolean)
(def normalized-type-test boolean/2 (or unbound boolean) boolean)

(def normalized-type-test integer/1 integer integer)
(def normalized-type-test integer/2 (or unbound integer) integer)
(def normalized-type-test integer/3 (or null integer) integer)
(def normalized-type-test integer/4 (or unbound null integer) integer)

(def normalized-type-test float/1 float float)
(def normalized-type-test float/2 (or unbound float) float)
(def normalized-type-test float/3 (or null float) float)
(def normalized-type-test float/4 (or unbound null float) float)

(def normalized-type-test double/1 double double)
(def normalized-type-test double/2 (or unbound double) double)
(def normalized-type-test double/3 (or null double) double)
(def normalized-type-test double/4 (or unbound null double) double)

(def normalized-type-test number/1 number number)
(def normalized-type-test number/2 (or unbound number) number)
(def normalized-type-test number/3 (or null number) number)
(def normalized-type-test number/4 (or unbound null number) number)

(def normalized-type-test string/1 string string)
(def normalized-type-test string/2 (or unbound string) string)
(def normalized-type-test string/3 (or null string) string)
(def normalized-type-test string/4 (or unbound null string) string)

(def normalized-type-test symbol/1 symbol symbol)
(def normalized-type-test symbol/2 (or unbound symbol) symbol)

(def normalized-type-test set/1 (set persistent-object) (set persistent-object))

;;;;;;
;;; Mapped

(def suite* (test/persistence/mapped :in test/persistence))

(def function check-mapped-type (type)
  (is (or (eq type 'unbound)
          (eq type 'member)
          (eq type t)
          (not (unbound-subtype-p type))))
  (is (or (eq type 'set)
          (eq type 'disjunct-set)
          (eq type 'ordered-set)
          (eq type 'member)
          (eq type t)
          (not (set-type-p* type)))))

(def test test/persistence/mapped/subtypep ()
  (mapc #'check-mapped-type *mapped-type-precedence-list*))

(def test test/persistence/mapped/type-precedence-list ()
  (iter (for types :on *mapped-type-precedence-list*)
        (for type-1 = (car types))
        (iter (for type-2 :in (cdr types))
              (unless (or (eq 'member type-1)
                          (eq 'member type-2)
                          (and (member type-1 '(float float-32 float-64 double))
                               (member type-2 '(float float-32 float-64 double)))
                          (and (member type-1 '(set ordered-set disjunct-set))
                               (member type-2 '(set ordered-set disjunct-set))))
                (is (not (subtypep type-2 type-1)))))))

(def test test/persistence/mapped/type (type mapped-type)
  (is (equalp mapped-type (mapped-type-for (normalized-type-for type)))))

(def definer mapped-type-test (type)
  `(progn
    (def test ,(concatenate-symbol (find-package :hu.dwim.perec.test) "test/persistence/mapped/" type "/1") ()
      (test/persistence/mapped/type ',type ',type))
    (def test ,(concatenate-symbol (find-package :hu.dwim.perec.test) "test/persistence/mapped/" type "/2") ()
      (test/persistence/mapped/type '(or unbound ,type) ',type))
    (def test ,(concatenate-symbol (find-package :hu.dwim.perec.test) "test/persistence/mapped/" type "/3") ()
      (test/persistence/mapped/type '(or null ,type) ',type))
    (def test ,(concatenate-symbol (find-package :hu.dwim.perec.test) "test/persistence/mapped/" type "/4") ()
      (test/persistence/mapped/type '(or null ,type) ',type))))

(def mapped-type-test boolean)

(def mapped-type-test serialized)

(def mapped-type-test form)

(def mapped-type-test t)

(def test test/persistence/mapped/null ()
  (test/persistence/mapped/type 'null nil))

(def test test/persistence/mapped/unbound ()
  (test/persistence/mapped/type 'unbound nil))

(def test test/persistence/mapped/or-null-unbound ()
  (test/persistence/mapped/type '(or null unbound) nil))

;;;;;;
;;; Type check

(def suite* (test/persistence/type-check :in test/persistence))

(def test test/persistence/type-check/primitive-type (type)
  (is (primitive-type-p type))
  (is (not (primitive-type-p `(or unbound ,type))))
  (is (not (primitive-type-p `(or null ,type))))
  (is (not (primitive-type-p `(or unbound null ,type))))
  (is (primitive-type-p* type))
  (is (primitive-type-p* `(or unbound ,type)))
  (is (primitive-type-p* `(or null ,type)))
  (is (primitive-type-p* `(or null unbound ,type))))

(def test test/persistence/type-check/boolean ()
  (test/persistence/type-check/primitive-type 'boolean)
  (is (not (null-subtype-p 'boolean))))

(def test test/persistence/type-check/string ()
  (test/persistence/type-check/primitive-type 'string))

(def test test/persistence/type-check/symbol ()
  (test/persistence/type-check/primitive-type 'symbol)
  (is (not (null-subtype-p 'symbol))))

(def persistent-class primitive-type-test ()
  ())

(def test test/persistence/type-check/persistent-object ()
  (is (persistent-class-type-p 'primitive-type-test))
  (is (not (persistent-class-type-p '(or unbound primitive-type-test))))
  (is (not (persistent-class-type-p '(or null primitive-type-test))))
  (is (not (persistent-class-type-p '(or unbound null primitive-type-test))))
  (is (persistent-class-type-p* 'primitive-type-test))
  (is (persistent-class-type-p* '(or unbound primitive-type-test)))
  (is (persistent-class-type-p* '(or null primitive-type-test)))
  (is (persistent-class-type-p* '(or unbound null primitive-type-test)))
  (is (not (primitive-type-p 'primitive-type-test)))
  (is (not (primitive-type-p '(or unbound primitive-type-test))))
  (is (not (primitive-type-p '(or null primitive-type-test))))
  (is (not (primitive-type-p '(or null unbound primitive-type-test)))))

;;;;;;
;;;; Reflection

(def suite* (test/persistence/type-reflection :in test/persistence))

(def test test/persistence/type-reflection/unbound ()
  (let ((type (find-type 'unbound)))
    (is (typep type 'unbound-type))
    (is (subtypep 'unbound-type 'eql-type))))

(def test test/persistence/type-reflection/null ()
  (let ((type (find-type 'null)))
    (is (typep type 'null-type))
    (is (subtypep 'null-type 'persistent-type))))

(def test test/persistence/type-reflection/boolean ()
  (let ((type (find-type 'boolean)))
    (is (typep type 'boolean-type))
    (is (subtypep 'boolean-type 'persistent-type))))

(def test test/persistence/type-reflection/integer-16 ()
  (let ((type (find-type 'integer-16)))
    (is (typep type 'integer-16-type))
    (is (subtypep 'integer-16-type 'integer-type))))

(def test test/persistence/type-reflection/integer-32 ()
  (let ((type (find-type 'integer-32)))
    (is (typep type 'integer-32-type))
    (is (subtypep 'integer-32-type 'integer-type))))

(def test test/persistence/type-reflection/integer-64 ()
  (let ((type (find-type 'integer-64)))
    (is (typep type 'integer-64-type))
    (is (subtypep 'integer-64-type 'integer-type))))

(def test test/persistence/type-reflection/string ()
  (let ((type (find-type 'string)))
    (is (typep type 'string-type))
    (is (subtypep 'string-type 'persistent-type))))

(def test test/persistence/type-reflection/text ()
  (let ((type (find-type 'text)))
    (is (typep type 'text-type))
    (is (subtypep 'text-type 'string-type))))

(def persistent-type member-test ()
  '(member a b c))

(def test test/persistence/type-reflection/member ()
  (let ((type (find-type 'member-test)))
    (is (typep type 'member-test-type))
    (is (subtypep 'member-test-type 'member-type))
    (is (equal (members-of type)
               '(a b c)))))

(def persistent-type integer-test ()
  'integer-32)

(def test test/persistence/type-reflection/integer ()
  (let ((type (find-type 'integer-test)))
    (is (typep type 'integer-test-type))
    (is (subtypep 'integer-test-type 'integer-32-type))))
