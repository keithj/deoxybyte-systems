;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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

(in-package :cl-system-utilities)

(defclass lift-test-config (static-file)
  ((target-system :initform nil
                  :initarg :target-system
                  :accessor target-system)))

(defclass cldoc-config (static-file)
  ((target-system :initform nil
                  :initarg :target-system
                  :accessor target-system)))

(defclass doc-op (operation)
  ())

(defmethod source-file-type ((c lift-test-config) (s module))
  "config")

(defmethod perform ((op load-op) (c lift-test-config))
  nil)

(defmethod perform ((op compile-op) (c lift-test-config))
  nil)

(defmethod input-files ((op test-op) (c lift-test-config))
  (component-pathname c))

(defmethod operation-done-p ((op test-op) (c lift-test-config))
  nil)

;;; load on demand so that we don't add dependencies for those who do
;;; not need to run the tests
(defmethod perform :before ((op test-op) (c lift-test-config))
  (unless (find-package :lift)
    (operate 'load-op :lift)))

(defmethod perform ((op test-op) (c lift-test-config))
  ;; Need to work relative to the root of the target system
  (let ((*default-pathname-defaults* (component-pathname
                                      (find-system (target-system c)))))    
    (funcall (intern (string :run-tests) (string :lift))
             :config (component-pathname c))))


(defmethod source-file-type ((c cldoc-config) (s module))
  ;; The cldoc pathname is a directory, so has pathname-type NIL
  nil)

(defmethod operation-done-p ((op doc-op) (c cldoc-config))
  nil)

(defmethod perform ((op doc-op) (c component))
  nil)

(defmethod perform ((op doc-op) (c cl-source-file))
  nil)

;;; load on demand so that we don't add dependencies for those who do
;;; not need to generate the documentation
(defmethod perform :before ((op doc-op) (c cldoc-config))
  (unless (find-package :cldoc)
    (operate 'load-op :cldoc)))

(defmethod perform ((op doc-op) (c cldoc-config))
  ;; Need to work relative to the root of the target system
  (let* ((target (find-system (target-system c)))
         (*default-pathname-defaults* (component-pathname target))
         (fn-sym (intern (string :extract-documentation) (string :cldoc)))
         (op-sym (intern (string :html) (string :cldoc))))
    (funcall fn-sym op-sym (namestring (component-pathname c)) target)))
