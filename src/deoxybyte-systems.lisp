;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
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

;;; Wrapper functions to make ASDF slightly less painful
(defun compile-system (system &key force)
  "Compiles SYSTEM using ASDF. When FORCE is T, forces the operation."
  (operate 'compile-op system :force force))

(defun load-system (system &key force)
  "Loads SYSTEM using ASDF. When FORCE is T, forces the operation."
  (operate 'load-op system :force force))

(defun test-system (system &key force)
  "Runs unit tests on SYSTEM using ASDF and LIFT. When FORCE is T,
forces the operation."
  (operate 'test-op system :force force))

(defun document-system (system &key force)
  "Extracts documentation from SYSTEM using ASDF and CL-DOC. When
FORCE is T, forces the operation."
  (operate 'doc-op system :force force))

(defun package-version-p (version)
  "Returns T if VERSION is a valid package version (a list of 3
integers), or NIL otherwise."
  (and (listp version)
       (= 3 (length version))
       (every #'integerp version)))

(defun package-version (package)
  "Returns the version of PACKAGE which is, by convention, given by
the value of the exported *PACKAGE-VERSION* within the package in
question. Raises an error where the *PACKAGE-VERSION* symbol does not
exist within PACKAGE, is not exported from PACKAGE or is bound to a
value that is either NIL or is not a valid package version."
  (multiple-value-bind (symbol status)
      (find-symbol "*PACKAGE-VERSION*" (find-package package))
    (ecase status
      (:internal (error "The package version is not external to ~a." package))
      (:inherited (error "No package version is defined for ~a." package))
      ((nil) (error "No package version is defined for ~a." package))
      (:external (let ((version (symbol-value symbol)))
                   (if (package-version-p version)
                       version
                     (error "The package version ~a for ~a is invalid."
                            version package)))))))

(defun package-version> (package1 package2)
  "Returns T if the version of PACKAGE1 is greater than that of
PACKAGE2, or NIL otherwise."
  (loop
     for v1 in (package-version package1)
     for v2 in (package-version package2)
     do (cond ((> v1 v2)
               (return t))
              ((< v1 v2)
               (return nil)))))

(defun package-version= (package1 package2)
  "Returns T if the version of PACKAGE1 is equal to that of PACKAGE2,
or NIL otherwise. This means that all three components of the version
number are equal."
  (every #'= (package-version package1) (package-version package2)))

(defun package-version>= (package1 package2)
  "Returns T if the version of PACKAGE1 is equal to, or greater than
that of PACKAGE2, or NIL otherwise."
  (or (package-version= package1 package2)
      (package-version> package1 package2)))

(defun package-version< (package1 package2)
  "Returns T if the version of PACKAGE1 is less than that of PACKAGE2,
or NIL otherwise."
  (not (package-version>= package1 package2)))

(defun package-version<= (package1 package2)
  "Returns T if the version of PACKAGE1 is less than, or equal to that
of PACKAGE2, or NIL otherwise."
  (or (package-version= package1 package2)
      (package-version< package1 package2)))
