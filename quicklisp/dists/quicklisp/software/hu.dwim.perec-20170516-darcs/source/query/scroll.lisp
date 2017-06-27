;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;; ---------------------------------------------------------------------------

(defclass* scroll ()
  ())

(defgeneric elements (scroll)
  (:documentation "Return the elements on the current page"))

(defgeneric page (scroll))
(defgeneric (setf page) (page scroll)
  (:method :around (page (scroll scroll))
           (declare (type integer page))
           ;; TODO: why isn't it an error?
           (when (< page 0)
             (setf page 0))
           (unless (= page (page scroll))
             (call-next-method page scroll))))

(defgeneric page-size (scroll))
(defgeneric (setf page-size) (page-size scroll))

(defgeneric first-page! (scroll)
  (:method ((scroll scroll))
           (setf (page scroll) 0)))

(defgeneric next-page! (scroll)
  (:method ((scroll scroll))
           (incf (page scroll)))
  (:documentation "Should return the current page number or nil if there are no more pages."))

(defgeneric previous-page! (scroll)
  (:method ((scroll scroll))
           (decf (page scroll))))

(defgeneric revive-scroll! (scroll)
  (:method ((scroll scroll))
           (values))
  (:documentation "Revives the cache objects of the scroll in the current transaction."))

;;; ---------------------------------------------------------------------------

(defclass* fixed-size-scroll (scroll)
  ())

(defgeneric page-count (fixed-size-scroll))

(defgeneric element-count (fixed-size-scroll))

(defgeneric last-page! (scroll)
  (:method ((scroll fixed-size-scroll))
           (setf (page scroll) (1- (page-count scroll)))))

(defmethod next-page! :around ((scroll fixed-size-scroll))
  (if (< (page scroll) (1- (page-count scroll)))
      (call-next-method)
      nil))

(defmethod (setf page) :around (page (scroll fixed-size-scroll))
  (declare (type integer page))
  (bind ((page-count (page-count scroll)))
    (when (>= page page-count)
      (setf page (1- page-count)))
    (when (/= page (page scroll))
      (call-next-method page scroll))))

(defmethod revive-scroll! :after ((scroll fixed-size-scroll))
  (when (>= (page scroll) (page-count scroll))
    (last-page! scroll)))

;;; ---------------------------------------------------------------------------

(defclass* simple-scroll (fixed-size-scroll)
  ((elements #() :type vector)
   (page 0 :accessor page :type integer)
   (page-size 10 :accessor page-size :type integer))
  (:documentation "Provides the fixed-size-scroll interface for a vector of elements."))

(defmethod initialize-instance :around ((scroll simple-scroll)
                                        &rest args
                                        &key page page-size elements
                                        &allow-other-keys)
  (remove-from-plistf args :elements)
  (setf elements (coerce elements 'vector))
  (apply #'call-next-method scroll :elements elements args)
  ;; send them through the standard setters for sanity checks
  (when page-size
    (setf (page-size scroll) page-size))
  (when page
    (setf (page scroll) page)))

(defmethod element-count ((scroll simple-scroll))
  (length (elements-of scroll)))

(defmethod page-count ((scroll simple-scroll))
  (values (ceiling (/ (element-count scroll) (page-size scroll)))))

(defmethod elements ((scroll simple-scroll))
  (bind ((page-size (page-size scroll))
         (page (page scroll))
         (start-offset (* page page-size))
         (end-offset (min (* (1+ page) page-size)
                          (element-count scroll))))
    (make-array (- end-offset start-offset)
                :displaced-to (elements-of scroll)
                :displaced-index-offset start-offset)))
