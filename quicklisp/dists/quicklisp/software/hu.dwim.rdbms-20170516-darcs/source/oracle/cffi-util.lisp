;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.oracle)

(def symbol-macro null (cffi:null-pointer))

(def function make-void-pointer ()
  (cffi:foreign-alloc '(:pointer :void) :initial-element null))

(def special-variable *default-oci-flags* (logior oci:+threaded+ oci:+new-length-semantics+))

;;; helpers to access foreign stuff

(def macro dereference-foreign-pointer (data-pointer type &optional size-pointer)
  (if (eq type :string)
      `(cffi:foreign-string-to-lisp
        (cffi:mem-ref ,data-pointer :pointer)
        :count (cffi:mem-ref ,size-pointer 'oci:ub-4)
        :encoding (connection-encoding-of (database-of *transaction*)))
      `(cffi:mem-ref ,data-pointer ,type)))

(def function oci-attr-get (param-descriptor
                     attribute-id
                     attribute-value         ; output
                     attribute-value-length) ; output
  (oci-call (oci:attr-get param-descriptor
                          oci:+dtype-param+
                          attribute-value
                          attribute-value-length
                          attribute-id
                          (error-handle-of *transaction*))))

(def macro get-param-descriptor-attribute (param-descriptor attribute type)
  `(cffi:with-foreign-objects ((data-pointer ,type)
                               (size-pointer 'oci:ub-4))
    (oci-call (oci:attr-get ,param-descriptor
               oci:+dtype-param+
               data-pointer
               size-pointer
               ,attribute
               (error-handle-of *transaction*)))
    (dereference-foreign-pointer data-pointer ,type size-pointer)))

(def macro get-statement-attribute (statement attribute type)
  `(cffi:with-foreign-objects ((data-pointer ,type)
                               (size-pointer 'oci:ub-4))
    (oci-call (oci:attr-get (statement-handle-of ,statement)
               oci:+htype-stmt+
               data-pointer
               size-pointer
               ,attribute
               (error-handle-of *transaction*)))
    (dereference-foreign-pointer data-pointer ,type size-pointer)))

(def function select-p (prepared-statement)
  (= oci:+stmt-select+
     (get-statement-attribute prepared-statement oci:+attr-stmt-type+ 'oci:ub-2)))

(def function insert-p (prepared-statement)
  (= oci:+stmt-insert+
     (get-statement-attribute prepared-statement oci:+attr-stmt-type+ 'oci:ub-2)))

(def function update-p (prepared-statement)
  (= oci:+stmt-update+
     (get-statement-attribute prepared-statement oci:+attr-stmt-type+ 'oci:ub-2)))

(def macro get-row-count-attribute (statement)
  `(get-statement-attribute ,statement oci:+attr-row-count+ 'oci:ub-4))

(def macro with-foreign-oci-string ((string c-string c-size &key (null-terminated-p #f)) &body body)
  `(cffi:with-foreign-string ((,c-string ,c-size) ,string
                              :encoding (connection-encoding-of (database-of *transaction*))
                              :null-terminated-p ,null-terminated-p)
     ,@body))

(def macro foreign-oci-string-alloc (string &rest args)
  `(cffi:foreign-string-alloc
    ,string
    :encoding (connection-encoding-of (database-of *transaction*))
    ,@args))

(def function oci-string-to-lisp (pointer &optional size)
  (cffi:foreign-string-to-lisp pointer :count size
                               :encoding (connection-encoding-of (database-of *transaction*))))

(def function oci-char-width ()
  (cffi::null-terminator-len (connection-encoding-of (database-of *transaction*)))) ;; FIXME using internal fn

(def function set-session-string-attribute (attribute value)
  (with-foreign-oci-string (value c-string c-size)
    (oci-call (oci:attr-set (session-handle-of *transaction*)
                            oci:+htype-session+
                            c-string
                            c-size
                            attribute
                            (error-handle-of *transaction*)))))

(def function server-attach (datasource)
  (with-foreign-oci-string (datasource c-datasource c-size)
    (oci-call (oci:server-attach (server-handle-of *transaction*)
                                 (error-handle-of *transaction*)
                                 c-datasource
                                 c-size
                                 oci:+default+))))

(def function stmt-prepare (statement command)
  (with-foreign-oci-string (command c-command c-size)
    (oci-call (oci:stmt-prepare (statement-handle-of statement)
                                (error-handle-of *transaction*)
                                c-command
                                c-size
                                oci:+ntv-syntax+
                                *default-oci-flags*))))

(def function stmt-execute (statement mode)
  (oci-call (oci:stmt-execute (service-context-handle-of *transaction*)
                              (statement-handle-of statement)
                              (error-handle-of *transaction*)
                              (if (select-p statement) 0 1)
                              0
                              null
                              null
                              mode)))

(def function stmt-fetch-2 (statement number-of-rows orientation offset)
  (let ((status
         #+allegro ;; TODO THL report error when offset used?
          (oci:stmt-fetch (statement-handle-of statement)
                          (error-handle-of *transaction*)
                          number-of-rows
                          orientation
                          *default-oci-flags*)
          #-allegro
          (oci:stmt-fetch-2 (statement-handle-of statement)
                            (error-handle-of *transaction*)
                            number-of-rows
                            orientation
                            offset
                            *default-oci-flags*)))
    (case status
      (#.oci:+success+ #t)
      (#.oci:+success-with-info+ #t) ;; warning, e.g. ORA-24347 sum over null
      (#.oci:+no-data+ #f)
      (t (handle-oci-error)))))

(def function stmt-fetch-last (statement)
  (stmt-fetch-2 statement 1 oci:+fetch-last+ 0))

(def function stmt-fetch-next (statement number-of-rows)
  (stmt-fetch-2 statement number-of-rows oci:+fetch-next+ 0))

(def function handle-alloc (handle-ptr handle-type)
  (oci-call (oci:handle-alloc (environment-handle-of *transaction*)
                              handle-ptr
                              handle-type
                              0
                              null)))

(def function dump-c-byte-array (ptr size)
  (with-output-to-string (s)
                         (loop for i from 0 below size
                               do (format s "~2,'0X "
                                          (cffi:mem-ref ptr :uint8 i)))))

(def function descriptor-alloc (descriptor-ptr-ptr descriptor-type)
  (oci-call (oci:descriptor-alloc (environment-handle-of *transaction*)
                                  descriptor-ptr-ptr
                                  descriptor-type
                                  0
                                  null)))

(def function allocate-oci-lob-locator (descriptor-ptr-ptr)
  (descriptor-alloc descriptor-ptr-ptr oci:+dtype-lob+))

(def function allocate-oci-date-time (descriptor-ptr-ptr)
  (descriptor-alloc descriptor-ptr-ptr oci:+dtype-timestamp+))

(def function allocate-oci-date-time-tz (descriptor-ptr-ptr)
  (descriptor-alloc descriptor-ptr-ptr oci:+dtype-timestamp-tz+))

(def function descriptor-free (descriptor-ptr descriptor-type)
  (oci-call (oci:descriptor-free descriptor-ptr
                                 descriptor-type)))

(def function free-oci-lob-locator (descriptor-ptr)
  (descriptor-free descriptor-ptr oci:+dtype-lob+))

(def function free-oci-date-time (descriptor-ptr)
  (descriptor-free descriptor-ptr oci:+dtype-timestamp+))

(def function free-oci-date-time-tz (descriptor-ptr)
  (descriptor-free descriptor-ptr oci:+dtype-timestamp-tz+))

(def function set-empty-lob (locator)
  (cffi:with-foreign-object (attribute 'oci:ub-4)
    (setf (cffi:mem-aref attribute 'oci:ub-4) 0)
    (oci-call (oci:attr-set locator
                            oci:+dtype-lob+
                            attribute
                            0
                            oci:+attr-lobempty+
                            (error-handle-of *transaction*)))))

(def function make-lob-locator (&optional empty)
  (cffi:with-foreign-object (descriptor-ptr-ptr :pointer)
    (allocate-oci-lob-locator descriptor-ptr-ptr)
    (let ((locator (cffi:mem-aref descriptor-ptr-ptr :pointer)))
      (when empty
        (set-empty-lob locator))
      (values locator (cffi:foreign-type-size :pointer)))))

(def function clob-type-p (sql-type)
  (typep sql-type 'sql-character-large-object-type))

(def function blob-type-p (sql-type)
  (typep sql-type 'sql-binary-large-object-type))

(def function lob-type-p (sql-type)
  (or (clob-type-p sql-type)
      (blob-type-p sql-type)))

(def function lob-get-length (svchp errhp locator)
  (cffi:with-foreign-object (len 'oci:ub-4)
    (oci:lob-get-length svchp errhp locator len)
    (cffi:mem-ref len 'oci:ub-4)))

(def function lob-read (svchp errhp locator bufp siz &optional amt csid)
  (cffi:with-foreign-object (amtp 'oci:sb-4)
    (setf (cffi:mem-ref amtp 'oci:sb-4) (or amt siz))
    (oci-call (oci:lob-read svchp errhp locator amtp 1 bufp siz null null (or csid 0)
                            oci:+sqlcs-implicit+))
    (assert (= (or amt siz) (cffi:mem-ref amtp 'oci:sb-4)))))

(def function lob-write (svchp errhp locator bufp siz &optional amt csid)
  (cffi:with-foreign-object (amtp 'oci:sb-4)
    (setf (cffi:mem-ref amtp 'oci:sb-4) (or amt siz))
    (oci-call (oci:lob-write svchp errhp locator amtp 1 bufp siz oci:+one-piece+
                             null null (or csid 0) oci:+sqlcs-implicit+))
    (assert (= (or amt siz) (cffi:mem-ref amtp 'oci:sb-4)))))

(def function lob-enable-buffering (svchp errhp locator)
  (oci-call (oci:lob-enable-buffering svchp errhp locator)))

(def function lob-flush-buffer (svchp errhp locator)
  (oci-call (oci:lob-flush-buffer svchp errhp locator oci:+lob-buffer-nofree+)))

(def method upload-lob (locator (value string))
  (assert (plusp (length value)))
  (let ((svchp (service-context-handle-of *transaction*))
        (errhp (error-handle-of *transaction*)))
    (multiple-value-bind (bufp siz) (foreign-oci-string-alloc value)
      (unwind-protect (lob-write svchp errhp locator bufp siz
                                 (length value) oci:+utf-16-id+)
        (cffi:foreign-string-free bufp)))))

(def method upload-lob (locator (value vector))
  (assert (plusp (length value)))
  (let ((svchp (service-context-handle-of *transaction*))
        (errhp (error-handle-of *transaction*))
        (siz (array-dimension value 0)))
    (let ((bufp (cffi::foreign-alloc 'oci:ub-1 :count siz)))
      (unwind-protect
           (progn
             (loop
                for byte across value
                for i from 0
                do (setf (cffi:mem-aref bufp 'oci:ub-1 i) byte))
             (lob-write svchp errhp locator bufp siz))
        (cffi:foreign-string-free bufp)))))

(def function download-clob (locator)
  (let* ((svchp (service-context-handle-of *transaction*))
         (errhp (error-handle-of *transaction*))
         (siz (lob-get-length svchp errhp locator)))
    (if (plusp siz)
        (let* ((amt siz)
               (encoding (connection-encoding-of (database-of *transaction*)))
               (character-width (cffi::null-terminator-len encoding))
               (siz (+ character-width (* character-width siz)))
               (bufp (cffi::foreign-alloc :char :count siz)))
          (unwind-protect
               (progn
                 (lob-read svchp errhp locator bufp siz amt oci:+utf-16-id+)
                 (dotimes (i character-width) ;; \0 terminator
                   (setf (cffi:mem-aref bufp 'oci:ub-1 (- siz i 1)) 0))
                 (oci-string-to-lisp bufp siz))
            (cffi-sys:foreign-free bufp)))
        (if (zerop siz)
            ""
            (error "unexpected clob length ~s" siz)))))

(def function download-blob (locator)
  (let* ((svchp (service-context-handle-of *transaction*))
         (errhp (error-handle-of *transaction*))
         (siz (lob-get-length svchp errhp locator)))
    (if (plusp siz)
        (let ((bufp (cffi::foreign-alloc 'oci:ub-1 :count siz))
              (result (make-array siz)))
          (unwind-protect
               (progn
                 (lob-read svchp errhp locator bufp siz)
                 (loop
                    for i from 0 below siz
                    do (setf (aref result i) (cffi:mem-aref bufp 'oci:ub-1 i)))
                 result)
            (cffi:foreign-string-free bufp)))
        (if (zerop siz)
            #()
            (error "unexpected blob length ~s" siz)))))
