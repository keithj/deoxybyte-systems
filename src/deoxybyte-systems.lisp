;;;
;;; Copyright (c) 2008-2010 Keith James. All rights reserved.
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

;;; compile-system is now provided by ASDF

(defun load-system (system &rest args &key force verbose version)
  "Loads SYSTEM using ASDF. When FORCE is T, forces the
operation."
  (flet ((load-op ()
           (apply #'operate 'load-op system args)))
    (if verbose
        (load-op)
        (and (load-op) t))))

(defun test-system (system &rest args &key force verbose version)
  "Loads SYSTEM using ASDF. When FORCE is T, forces the
operation. When VERBOSE is NIL, suppresses the return value from
ASDF:TEST-SYSTEM which pushes all the test results off my REPL. Grr."
  (let ((result (apply #'operate 'test-op system args)))
    (if verbose
        result
        (and result t))))

(defun document-system (system &key force)
  "Extracts documentation from SYSTEM using ASDF and CLDOC. When
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
