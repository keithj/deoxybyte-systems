;;;
;;; Copyright (c) 2008-2012 Keith James. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials
;;;       provided with the distribution.
;;;
;;;     * Neither the names of the copyright holders nor the names of
;;;       its contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written
;;;       permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;

(in-package :uk.co.deoxybyte-systems)

;; This works around an issue in ASDF 1 where it raises an error when
;; finding timestamps. This occurs where the component pathname is a
;; directory that does not yet exist.
(defmethod operation-done-p :before ((op operation) (c component))
  (let ((path (component-pathname c)))
    (when (fad:directory-pathname-p path)
      (ensure-directories-exist path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LIFT unit testing ASDF extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass lift-test-config (static-file)
  ((target-system :initform nil
                  :initarg :target-system
                  :accessor target-system
                  :documentation "The system to be tested
   e.g. :cl-system-utilities"))
  (:documentation "An ASDF component that represents a LIFT unit
  testing configuration file."))

(defmethod source-file-type ((c lift-test-config) (s module))
  "config")

(defmethod input-files ((op test-op) (c lift-test-config))
  (list (component-pathname c)))

(defmethod operation-done-p ((op test-op) (c lift-test-config))
  nil)

(defmethod perform ((op load-op) (c lift-test-config))
  nil)

(defmethod perform ((op compile-op) (c lift-test-config))
  nil)

(defmethod perform ((op test-op) (c lift-test-config))
  ;; Make sure LIFT drops its test results in the system's home directory
  (let ((*default-pathname-defaults* (system-relative-pathname
                                      (find-system (target-system c))
                                      (pathname ""))))
    ;; Refer to LIFT package indirectly so that we are not forced to
    ;; load it immediately
    ;; (lift:run-tests :config (component-pathname c))
    (funcall (intern (string 'run-tests) :lift)
             :config (component-pathname c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLDOC ASDF extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass cldoc-config (static-file)
  ((target-system :initform nil
                  :initarg :target-system
                  :accessor target-system
                  :documentation "The system to be documented
                  e.g. :cl-system-utilities"))
  (:documentation "An ASDF component that represents a CLDOC
  documentation extraction configuration file."))

(defclass doc-op (operation)
  ()
  (:documentation "An ASDF operation that extracts docstring
  documentation from Lisp code using CLDOC."))

(defmethod source-file-type ((c cldoc-config) (s module))
  ;; The cldoc pathname is a directory, so has pathname-type NIL
  nil)

(defmethod operation-done-p ((op doc-op) (c cldoc-config))
  nil)

(defmethod perform ((op doc-op) (c component))
  nil)

(defmethod perform ((op doc-op) (c cl-source-file))
  nil)

(defmethod perform ((op doc-op) (c cldoc-config))
  ;; Need to work relative to the root of the target system
  (let* ((target (find-system (target-system c)))
         (path (system-relative-pathname target (pathname ""))))

    ;; I'm sure I should be able to do the following in the ASDF file
    ;; using something like
    ;;
    ;; :in-order-to ((doc-op (load-op :cldoc)))
    ;;
    ;; but :in-order-to doesn't seem to be extensible for new
    ;; operations.
    (unless (find-package :cldoc)
      (operate 'load-op :cldoc))

    ;; Refer to CLDOC package indirectly so that we are not forced to
    ;; load it immediately
    ;; (cldoc:extract-documentation 'cldoc:html (namestring path) target)
    (funcall (intern (string 'extract-documentation) :cldoc)
             (intern (string 'html) :cldoc) (namestring path) target)))
