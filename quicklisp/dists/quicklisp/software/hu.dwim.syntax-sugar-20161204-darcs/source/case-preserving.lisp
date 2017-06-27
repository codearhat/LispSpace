;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(define-syntax (case-preserving :export t)
    (&key (start-character #\,)
          (dispatch-character #\#)
          (readtable-case :preserve)
          packages)
  (let ((reader (named-lambda case-preserving-reader (input-stream char &optional dispatched-char)
                  (declare (ignore char dispatched-char))
                  (%case-preserving-reader input-stream readtable-case packages))))
    (set-dispatch-macro-character dispatch-character start-character
                                  reader *readtable*)))

(defun %case-preserving-reader (stream readtable-case packages)
  (let ((*readtable* (let ((readtable (copy-readtable)))
                       (setf (readtable-case readtable) readtable-case)
                       readtable))
        (packages-to-try (if packages
                             (mapcan (lambda (name)
                                       (list (or (find-package name)
                                                 (progn
                                                   (warn "Case preserving reader: Package ~S is not found." name)
                                                   nil))))
                                     packages)
                             (list *package*))))
    (if (= 1 (length packages-to-try))
        (let ((*package* (first packages-to-try)))
          (read stream))
        (let ((symbol-name (let* ((*package* (make-package "hu.dwim.syntax-sugar.tmp"))
                                  (symbol (read stream)))
                             (unless (symbolp symbol)
                               (error "~S only supports reading one symbol at a time when used with multiple packages"
                                      'case-preserving-reader))
                             (prog1
                                 (symbol-name symbol)
                               (delete-package *package*)))))
          (dolist (package packages-to-try (error "Couldn't find symbol ~S in the packages ~A"
                                                  symbol-name packages))
            (awhen (and (find-package package)
                        (find-symbol symbol-name package))
              (return it)))))))
