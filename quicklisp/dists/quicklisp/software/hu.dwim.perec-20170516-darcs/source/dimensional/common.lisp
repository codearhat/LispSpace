;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(def function complete-partial-timestamp (timestamp)
  (bind (((partial-type completion)
          (ecase (count #\- timestamp)
            (0 '(:year "-01-01T00:00:00Z"))
            (1 '(:month "-01T00:00:00Z"))
            (2 (if (search "T" timestamp)
                   (ecase (count #\: timestamp)
                     (0 '(:hour ":00:00Z"))
                     (1 '(:minute ":00Z"))
                     (2 `(:sec ,(if (ends-with #\Z timestamp) "" "Z"))))
                   '(:day "T00:00:00Z"))))))
    (values (string+ timestamp completion) partial-type)))

(def function first-moment-for-partial-timestamp (timestamp)
  (parse-timestring (complete-partial-timestamp timestamp)))

(def function last-moment-for-partial-timestamp (timestamp)
  (bind (((:values timestamp-string timestamp-partial-type) (complete-partial-timestamp timestamp)))
    (adjust-timestamp (parse-timestring timestamp-string) (offset timestamp-partial-type 1))))
