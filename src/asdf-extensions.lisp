;;;
;;; Copyright (C) 2008-2010 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-systems.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :uk.co.deoxybyte-systems)

;; This works around an issue in ASDF where it raises an error when
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
  (component-pathname c))

(defmethod operation-done-p ((op test-op) (c lift-test-config))
  nil)

(defmethod perform ((op load-op) (c lift-test-config))
  nil)

(defmethod perform ((op compile-op) (c lift-test-config))
  nil)

(defmethod perform ((op test-op) (c lift-test-config))
  ;; Need to work relative to the root of the target system
  (let ((*default-pathname-defaults* (component-pathname
                                      (find-system (target-system c)))))
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
  (let ((target (find-system (target-system c)))
        (path (component-pathname c)))

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
