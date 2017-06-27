;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

;;;;;;
;;; Partial timestamp

(def suite* (test/dimensional/partial-timestamp :in test/dimensional))

(def test test/dimensional/partial-timestamp/first-moment ()
  (iter (for (expected actual)
          :on (list "2006-01-01T00:00:00Z" (first-moment-for-partial-timestamp "2006")
                    "2006-06-01T00:00:00Z" (first-moment-for-partial-timestamp "2006-06")
                    "2006-06-06T00:00:00Z" (first-moment-for-partial-timestamp "2006-06-06")
                    "2006-06-06T06:00:00Z" (first-moment-for-partial-timestamp "2006-06-06T06")
                    "2006-06-06T06:06:00Z" (first-moment-for-partial-timestamp "2006-06-06T06:06")
                    "2006-06-06T06:06:06Z" (first-moment-for-partial-timestamp "2006-06-06T06:06:06"))
          :by #'cddr)
        (is (timestamp= (parse-timestring expected) actual))))

(def test test/dimensional/partial-timestamp/last-moment ()
  (iter (for (expected actual)
          :on (list "2007-01-01T00:00:00Z" (last-moment-for-partial-timestamp "2006")
                    "2006-07-01T00:00:00Z" (last-moment-for-partial-timestamp "2006-06")
                    "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06")
                    "2006-06-06T07:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T06")
                    "2006-06-06T06:07:00Z" (last-moment-for-partial-timestamp "2006-06-06T06:06")
                    "2006-06-06T06:06:07Z" (last-moment-for-partial-timestamp "2006-06-06T06:06:06")
                    )
          :by #'cddr)
        (is (timestamp= (parse-timestring expected) actual))))

(def test test/dimensional/partial-timestamp/overflow ()
  (iter (for (expected actual)
          :on (list "2007-01-01T00:00:00Z" (last-moment-for-partial-timestamp "2006-12")
                    "2004-03-01T00:00:00Z" (last-moment-for-partial-timestamp "2004-02-29")
                    "2005-03-01T00:00:00Z" (last-moment-for-partial-timestamp "2005-02-28")
                    "2007-01-01T00:00:00Z" (last-moment-for-partial-timestamp "2006-12-31")
                    "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T23")
                    "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T23:59")
                    "2006-06-07T00:00:00Z" (last-moment-for-partial-timestamp "2006-06-06T23:59:59"))
          :by #'cddr)
        (is (timestamp= (parse-timestring expected) actual))))
