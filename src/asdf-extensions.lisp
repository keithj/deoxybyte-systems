;;;
;;; Copyright (c) 2008-2013 Keith James. All rights reserved.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LIFT unit testing ASDF utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe-run-lift-tests (system-name lift-config-filename)
  "When LIFT is available, run tests on SYSTEM-NAME using
configuration in LIFT-CONFIG-FILENAME ."
  (if (find-package :lift)
      (funcall (intern (symbol-name :run-tests) :lift)
               :config (namestring
                        (system-relative-pathname
                         system-name lift-config-filename)))
      (format *error-output*
              "The LIFT package is not available: skipping tests of system ~a"
              system-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLDOC ASDF extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass doc-op (operation)
  ()
  (:documentation "An ASDF operation that extracts docstring
  documentation from Lisp code using CLDOC."))

(defun maybe-build-cldoc-docs (system-name root-path)
  "When CLDOC is available, build HTML documentation for SYSTEM-NAME
with TITLE ."
  (if (find-package :cldoc)
      (funcall (intern (symbol-name '#:extract-documentation) :cldoc)
               (intern (string 'html) :cldoc)
               (namestring (system-relative-pathname system-name root-path))
               (find-system system-name))
      (format *error-output*
              "The CLDOC package is not available: skipping documentation generaion of system ~a"
              system-name)))
