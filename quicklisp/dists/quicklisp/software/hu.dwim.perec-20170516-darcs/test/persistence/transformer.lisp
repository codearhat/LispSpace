;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(def suite* (test/persistence/transformer :in test/persistence))

(def special-variable +transformer-string-test-value+ "value")

(def special-variable +transformer-integer-test-value+ 42)

(def function is-equal-using-writer (type slot-value rdbms-values)
  (is (equalp rdbms-values (lisp-value->rdbms-values type slot-value))))

(def function is-equal-using-reader (type slot-value rdbms-values)
  (is (equalp slot-value (rdbms-values->lisp-value type rdbms-values))))

(def function is-equal-using-transformers (type slot-value rdbms-values)
  (is-equal-using-reader type slot-value rdbms-values)
  (is-equal-using-writer type slot-value rdbms-values))

;;;;;;
;;; Boolean

(def test test/persistence/transformer/boolean/t ()
  (is-equal-using-transformers 'boolean
                               #t
                               (vector "TRUE")))

(def test test/persistence/transformer/boolean/f ()
  (is-equal-using-transformers 'boolean
                               #f
                               (vector "FALSE")))

(def test test/persistence/transformer/or-unbound-boolean/unbound ()
  (is-equal-using-transformers '(or unbound boolean)
                               +unbound-slot-marker+
                               (vector :null)))

(def test test/persistence/transformer/or-unbound-boolean/t ()
  (is-equal-using-transformers '(or unbound boolean)
                               #t
                               (vector "TRUE")))

(def test test/persistence/transformer/or-unbound-boolean/f ()
  (is-equal-using-transformers '(or unbound boolean)
                               #f
                               (vector "FALSE")))

;;;;;;
;;; Integer

(def test test/persistence/transformer/integer ()
  (is-equal-using-transformers 'integer
                               +transformer-integer-test-value+
                               (vector +transformer-integer-test-value+)))

(def test test/persistence/transformer/or-null-integer/nil ()
  (is-equal-using-transformers '(or null integer)
                               nil
                               (vector :null)))

(def test test/persistence/transformer/or-null-integer/integer ()
  (is-equal-using-transformers '(or null integer)
                               +transformer-integer-test-value+
                               (vector +transformer-integer-test-value+)))

(def test test/persistence/transformer/or-unbound-integer/unbound ()
  (is-equal-using-transformers '(or unbound integer)
                               +unbound-slot-marker+
                               (vector :null)))

(def test test/persistence/transformer/or-unbound-integer/integer ()
  (is-equal-using-transformers '(or unbound integer)
                               +transformer-integer-test-value+
                               (vector +transformer-integer-test-value+)))

(def test test/persistence/transformer/or-unbound-null-integer/unbound ()
  (is-equal-using-transformers '(or unbound null integer)
                               +unbound-slot-marker+
                               (vector 1 :null)))

(def test test/persistence/transformer/or-unbound-null-integer/null ()
  (is-equal-using-transformers '(or unbound null integer)
                               nil
                               (vector 2 :null)))

(def test test/persistence/transformer/or-unbound-null-integer/integer ()
  (is-equal-using-transformers '(or unbound null integer)
                               +transformer-integer-test-value+
                               (vector 0 +transformer-integer-test-value+)))

;;;;;;
;;; String

(def test test/persistence/transformer/string ()
  (is-equal-using-transformers 'string
                               +transformer-string-test-value+
                               (vector +transformer-string-test-value+)))

(def test test/persistence/transformer/or-null-string/nil ()
  (is-equal-using-transformers '(or null string)
                               nil
                               (vector :null)))

(def test test/persistence/transformer/or-null-string/string ()
  (is-equal-using-transformers '(or null string)
                               +transformer-string-test-value+
                               (vector +transformer-string-test-value+)))

(def test test/persistence/transformer/or-unbound-string/unbound ()
  (is-equal-using-transformers '(or unbound string)
                               +unbound-slot-marker+
                               (vector :null)))

(def test test/persistence/transformer/or-unbound-string/string ()
  (is-equal-using-transformers '(or unbound string)
                               +transformer-string-test-value+
                               (vector +transformer-string-test-value+)))

(def test test/persistence/transformer/or-unbound-null-string/unbound ()
  (is-equal-using-transformers '(or unbound null string)
                               +unbound-slot-marker+
                               (vector 1 :null)))

(def test test/persistence/transformer/or-unbound-null-string/null ()
  (is-equal-using-transformers '(or unbound null string)
                               nil
                               (vector 2 :null)))

(def test test/persistence/transformer/or-unbound-null-string/string ()
  (is-equal-using-transformers '(or unbound null string)
                               +transformer-string-test-value+
                               (vector 0 +transformer-string-test-value+)))

;;;;;;
;;; Symbol

(def test test/persistence/transformer/symbol/nil ()
  (is-equal-using-transformers 'symbol
                               nil
                               (vector "COMMON-LISP::NIL")))

(def test test/persistence/transformer/symbol/something ()
  (is-equal-using-transformers 'symbol
                               'something
                               (vector "HU.DWIM.PEREC.TEST::SOMETHING")))

(def test test/persistence/transformer/or-unbound-symbol/unbound ()
  (is-equal-using-transformers '(or unbound symbol)
                               +unbound-slot-marker+
                               (vector :null)))

(def test test/persistence/transformer/or-unbound-symbol/nil ()
  (is-equal-using-transformers '(or unbound symbol)
                               nil
                               (vector "COMMON-LISP::NIL")))

(def test test/persistence/transformer/or-unbound-symbol/something ()
  (is-equal-using-transformers '(or unbound symbol)
                               'something
                               (vector "HU.DWIM.PEREC.TEST::SOMETHING")))

;;;;;
;;; t

(def test test/persistence/transformer/t/unbound ()
  (is-equal-using-transformers t
                               +unbound-slot-marker+
                               (vector :null)))

(def test test/persistence/transformer/t/nil ()
  (is-equal-using-transformers t
                               nil
                               (vector (hu.dwim.serializer:serialize nil))))

(def test test/persistence/transformer/t/something ()
  (is-equal-using-transformers t
                               'something
                               (vector (hu.dwim.serializer:serialize 'something))))
