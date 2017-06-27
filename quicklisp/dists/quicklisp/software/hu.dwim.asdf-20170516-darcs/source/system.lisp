;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

;;;;;;
;;; Package support

(defclass system-with-package (asdf:system)
  ((package-name
    :initarg :package-name
    :accessor system-package-name)))

(defmethod reinitialize-instance :after ((system system-with-package) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (slot-boundp system 'package-name)
    (setf (system-package-name system)
          (string-upcase (asdf:component-name system)))))

(defclass system-with-target ()
  ((target-system-name
    :initarg :target-system-name
    :accessor system-target-system-name)))

(defmethod reinitialize-instance :after ((system system-with-target) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let* ((system-name (string-downcase (asdf:component-name system)))
         (last-dot-position (position #\. system-name :from-end t)))
    (unless (slot-boundp system 'target-system-name)
      (setf (system-target-system-name system)
            (subseq system-name 0 last-dot-position)))
    (let ((target-system (find-system (system-target-system-name system) nil)))
      (when target-system
        (when (and (slot-boundp target-system 'asdf::author)
                   (not (slot-boundp system 'asdf::author)))
          (setf (asdf:system-author system)
                (asdf:system-author target-system)))
        (when (and (slot-boundp target-system 'asdf::licence)
                   (not (slot-boundp system 'asdf::licence)))
          (setf (asdf:system-licence system)
                (asdf:system-licence target-system)))
        (unless (slot-boundp system 'asdf::description)
          (setf (asdf:system-description system)
                (concatenate 'string
                             (string-capitalize (subseq system-name (1+ last-dot-position)))
                             " for "
                             (system-target-system-name system))))))))

;;;;;;
;;; DWIM system

(defvar *muffle-optimization-warnings* t)

(defclass hu.dwim.cl-source-file (asdf:cl-source-file)
  ())

(defclass system-with-output ()
  ((compile-output
    :initform nil
    :initarg :compile-output
    :accessor system-compile-output)
   (load-output
    :initform nil
    :initarg :load-output
    :accessor system-load-output)))

(defclass hu.dwim.base-system (system-with-output system-with-package)
  ())

(defmethod shared-initialize :around ((system hu.dwim.base-system) slot-names &rest initargs)
  (unless (getf initargs :license)
    (setf (getf initargs :license) "BSD or Bugroff"))
  (unless (getf initargs :author)
    (setf (getf initargs :author) '("Tamás Borbély <tomi.borbely@gmail.com>"
                                    "Attila Lendvai <attila.lendvai@gmail.com>"
                                    "Levente Mészáros <levente.meszaros@gmail.com>")))
  (apply #'call-next-method system slot-names initargs))

(defclass hu.dwim.system (hu.dwim.base-system)
  ((test-system-name
    :initarg :test-system-name
    :accessor system-test-system-name)
   (documentation-system-name
    :initarg :documentation-system-name
    :accessor system-documentation-system-name)))

(defclass hu.dwim.test-system (system-with-target hu.dwim.base-system)
  ((test-name
    :initform "TEST"
    :initarg :test-name
    :accessor system-test-name)
   (test-result
    :initform nil
    :initarg :test-result
    :accessor system-test-result)
   (test-output
    :initform nil
    :initarg :test-output
    :accessor system-test-output)))

(defmethod shared-initialize :around ((system hu.dwim.test-system) slot-names &rest initargs)
  (unless (getf initargs :description)
    (setf (getf initargs :description) "Test system for the similarly named system."))
  (apply #'call-next-method system slot-names initargs))

(defclass hu.dwim.documentation-system (system-with-target hu.dwim.base-system)
  ())

(defmethod shared-initialize :around ((system hu.dwim.documentation-system) slot-names &rest initargs)
  (unless (getf initargs :description)
    (setf (getf initargs :description) "Documentation for the similarly named system. It should contain formally processable data and its contents should be available at http://dwim.hu"))
  (apply #'call-next-method system slot-names initargs))

(defmacro with-capturing-output (place &body forms)
  (let ((stream (gensym "STREAM")))
    `(let* ((,stream (make-string-output-stream))
            (*standard-output* (make-broadcast-stream *standard-output* ,stream))
            (*error-output* (make-broadcast-stream *error-output* ,stream)))
       ,@forms
       (setf ,place (concatenate 'string ,place (get-output-stream-string ,stream))))))

(defmethod reinitialize-instance :after ((system hu.dwim.system) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (slot-boundp system 'test-system-name)
    (setf (system-test-system-name system)
          (concatenate 'string (string-downcase (asdf:component-name system)) ".test")))
  (unless (slot-boundp system 'documentation-system-name)
    (setf (system-documentation-system-name system)
          (concatenate 'string (string-downcase (asdf:component-name system)) ".documentation"))))

(defmethod asdf::module-default-component-class ((class hu.dwim.test-system))
  'hu.dwim.cl-source-file)

(defmethod asdf::module-default-component-class ((class hu.dwim.system))
  'hu.dwim.cl-source-file)

(defmethod perform :around ((op asdf:operation) (component hu.dwim.cl-source-file))
  (let ((*features* *features*)
        (*readtable* (copy-readtable *readtable*))
        (*package* *package*)
        (hu.dwim.common-package (find-package :hu.dwim.common)))
    (when hu.dwim.common-package
      ;; when the hu.dwim.common package is available, then we read lisp files into that, so that hu.dwim.common:in-package can shadow cl:in-package.
      ;; see hu.dwim.def/source/extended-package.lisp for more info.
      (setf *package* hu.dwim.common-package))
    (debug-only
      (pushnew :debug *features*))
    (call-in-system-environment op (asdf:component-system component) #'call-next-method)))

(defmethod perform :around ((op compile-op) (component hu.dwim.cl-source-file))
  (with-capturing-output (system-compile-output (asdf:component-system component))
    (call-next-method)))

(defmethod perform :around ((op load-op) (component hu.dwim.cl-source-file))
  (with-capturing-output (system-load-output (asdf:component-system component))
    (call-next-method)))

(defgeneric call-in-system-environment (operation system function)
  (:method ((op asdf:operation) (system asdf:system) function)
    (if *muffle-optimization-warnings*
        (call-with-muffled-boring-compiler-warnings function)
        (funcall function))))

(defmethod perform ((op test-op) (system hu.dwim.system))
  (let ((test-system (find-system (system-test-system-name system) nil)))
    (if (typep test-system 'hu.dwim.test-system)
        (progn
          (load-system test-system)
          (run-test-suite test-system))
        (warn "There is no test system for ~A, no tests were run." system))))

(defgeneric run-test-suite (system)
  (:method ((system asdf:system))
    (warn "Don't know how to run tests suite for ~A" system))
  (:method :around ((system hu.dwim.test-system))
    (with-capturing-output (system-test-output system)
      (setf (system-test-result system) (call-next-method))))
  (:method ((system hu.dwim.test-system))
    (if (find-package :hu.dwim.stefil)
        (let ((package-name (system-package-name system)))
          (if package-name
              (let ((test-name (find-symbol (system-test-name system) package-name)))
                (funcall (find-symbol "FUNCALL-TEST-WITH-FEEDBACK-MESSAGE" :hu.dwim.stefil) test-name))
              (warn "There is no test package for ~A, no tests were run." system)))
        (call-next-method))))

;;;;;;
;;; Develop

;; this is only needed to workaround asdf's rigidity (ASDF protects *package* by rebinding it, but we want to set it)
(defvar *development-package*)

(defclass develop-op (non-propagating-operation)
  ())

(defmethod asdf:operation-done-p ((operation develop-op) (component asdf:component))
  nil)

(defmethod perform ((operation develop-op) (component asdf:component))
  nil)

(defmethod perform ((operation develop-op) (system asdf:system))
  (load-system system)
  (let ((package (find-package (string-upcase (asdf:component-name system)))))
    (when package
      (setf *development-package* package))))

(defmethod perform :before ((operation develop-op) (system asdf:system))
  (with-simple-restart (continue "Give up loading Swank and continue...")
    (load-system :swank)
    (set (read-from-string "swank:*globally-redirect-io*") t)))

(defmethod perform :after ((operation develop-op) (system asdf:system))
  (load-system :hu.dwim.debug)
  (use-package :hu.dwim.debug :hu.dwim.common)
  (do-external-symbols (symbol :hu.dwim.debug)
    (export symbol :hu.dwim.common))
  (find-and-load-swank-integration-systems)
  (declaim (optimize (debug 3)))
  (pushnew :debug *features*)
  (warn "Pushed :debug in *features* and issued (declaim (optimize (debug 3))) to help later C-c C-c'ing"))

(defmethod perform ((operation develop-op) (system hu.dwim.system))
  (let ((system-to-load (or (find-system (system-test-system-name system) nil)
                            system))
        (quickload-fn (when (find-package '#:quicklisp)
                        (find-symbol (string '#:quickload) (find-package '#:quicklisp)))))
    (if quickload-fn
        (funcall quickload-fn (asdf:component-name system-to-load)
                 :verbose t)
        (load-system system-to-load))
    (let ((package (find-package (system-package-name system-to-load))))
      (when package
        (setf *development-package* package)))))

(defun develop-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:develop-op system)`. See [operate][] for details."
  (declare (ignore force version))
  (let ((*development-package* nil))
    (multiple-value-prog1
        (apply 'asdf:operate 'develop-op system :verbose verbose args)
      (when *development-package*
        (setf *package* *development-package*)
        (warn "Changed *package* to ~A" *package*)))))

;;;;;;
;;; Util

(defun system-pathname (name)
  (asdf:component-pathname (find-system name)))

(defun system-directory (name)
  (make-pathname :directory (pathname-directory (system-pathname name))))

(defun system-loaded-p (system-name)
  (let ((system (find-system system-name)))
    (when system
      (asdf:component-loaded-p system))))

(defun map-asdf-source-registry-directories (visitor)
  (loop
    :for asd-file :being :the :hash-value :of asdf::*source-registry*
    :do (funcall visitor (make-pathname :directory (pathname-directory asd-file)))))

(defun find-all-swank-integration-systems ()
  (map-asdf-source-registry-directories
   (lambda (directory)
     (dolist (file (directory (merge-pathnames directory (make-pathname :name :wild :type "asd"))))
       (let ((name (pathname-name file)))
         (when (and (search "hu.dwim" name)
                    (search "+swank" name))
           (find-system name)))))))

(defun load-swank-integration-systems ()
  "Loads the +swank systems for the already loaded systems."
  (map nil (lambda (name)
             (let ((system (asdf:find-system name)))
               (when (and (search "+swank" name)
                          (not (system-loaded-p name))
                          (every 'system-loaded-p (collect-system-dependencies system)))
                 (with-simple-restart (skip-system "Skip loading swank integration ~A" system)
                   (load-system system)))))
           (asdf:registered-systems)))

(defun find-and-load-swank-integration-systems ()
  (find-all-swank-integration-systems)
  (load-swank-integration-systems))

(defun %iterate-system-dependencies-1 (function system)
  (check-type system asdf:system)
  ;; NOTE: it's not clear how to iterate dependencies, see this old discussion:
  ;; http://article.gmane.org/gmane.lisp.asdf.devel/3105
  ;; although ASDF:COMPONENT-SIDEWAY-DEPENDENCIES might be newer than that discussion.
  (dolist (dependency (asdf:component-sideway-dependencies system))
    ;; NOTE: there may be dependencies here like this: (:VERSION :METATILITIES-BASE "0.6.6")
    (when (consp dependency)
      (case (first dependency)
        (:version
         (setf dependency (second dependency)))
        (t (error "Don't know how to interpret the following ASDF dependency specification: ~S" dependency))))
    (funcall function (asdf:find-system dependency))))

(defun iterate-system-dependencies (function system &key (transitive nil))
  (setf system (find-system system))
  (if transitive
      (let ((dependencies '()))
        (labels ((recurse (system)
                   (%iterate-system-dependencies-1 (lambda (dependency)
                                                     (unless (member dependency dependencies)
                                                       (push dependency dependencies)
                                                       (recurse dependency)))
                                                   system)))
          (recurse system)
          (map nil function dependencies)))
      (%iterate-system-dependencies-1 function system))
  (values))

(defun map-system-dependencies (function system &key (transitive nil))
  (let ((result '()))
    (iterate-system-dependencies (lambda (dependency)
                                   (push (funcall function dependency) result))
                                 system
                                 :transitive transitive)
    result))

(defun collect-system-dependencies (system &key (transitive nil))
  (map-system-dependencies 'identity system :transitive transitive))

(defmacro do-system-dependencies ((variable-name system-name &key (transitive nil)) &body body)
  (let ((body-fn (gensym "DSD-BODY")))
    `(block nil
       (flet ((,body-fn (,variable-name)
                ,@body))
         (iterate-system-dependencies #',body-fn ,system-name :transitive ,transitive)))))

(reinitialize-instance (change-class (find-system :hu.dwim.asdf) 'hu.dwim.system))

#+sbcl
;; KLUDGE: TODO: this is an ugly hack to work around the bug https://bugs.launchpad.net/sbcl/+bug/501075
(sb-ext::without-package-locks
  (defun sb-impl::line-length (&optional (stream *standard-output*))
    (declare (ignore stream))
    160))
