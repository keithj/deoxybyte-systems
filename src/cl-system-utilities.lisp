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

(in-package :cl-system-utilities)

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
