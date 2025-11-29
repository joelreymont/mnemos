(defpackage :hemis.index
  (:use :cl)
  (:export :find-usages :find-definition))

(in-package :hemis.index)

;; Stubs for now.
(defun find-usages (symbol-name)
  (declare (ignore symbol-name))
  ;; Return a list of plist objects describing locations.
  nil)

(defun find-definition (symbol-name)
  (declare (ignore symbol-name))
  ;; Return a single location or NIL.
  nil)
