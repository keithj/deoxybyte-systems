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

(defpackage :uk.co.deoxybyte-systems
  (:use #:common-lisp #:asdf #:cldoc)
  (:nicknames
   #:deoxybyte-systems
   #:dxs)
  (:export
   #:lift-test-config
   #:cldoc-config

   #:compile-system
   #:load-system
   #:test-system
   #:document-system

   #:package-version-p
   #:package-version
   #:package-version=
   #:package-version>
   #:package-version>=
   #:package-version<
   #:package-version<=)
  (:documentation "The deoxybyte-systems system provides tools for
  managing Common Lisp systems in batch mode and from the REPL. It
  provides convenient wrapper functions for ASDF operations and
  collects my ASDF extensions in one place.

  This system is required by most of my other Common Lisp systems
  because it supplies the unit test and documentation
  configurations (using LIFT {http://common-lisp.net/project/lift/}
  and CL-DOC {http://common-lisp.net/project/cldoc/} respectively."))
