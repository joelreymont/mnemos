(defpackage :mnemos.tools
  (:use :cl)
  (:import-from :mnemos.index :find-usages :find-definition)
  (:export :dispatch-tool))

(in-package :mnemos.tools)

(defun tool-find-usages (args)
  (let* ((symbol (gethash "symbol" args))
         (results (find-usages symbol)))
    `(("status" . "ok")
      ("results" . ,results))))

(defun tool-find-definition (args)
  (let* ((symbol (gethash "symbol" args))
         (loc (find-definition symbol)))
    (if loc
        `(("status" . "ok") ("location" . ,loc))
        `(("status" . "not-found")))))

(defun dispatch-tool (name args)
  (ecase name
    ("find-usages" (tool-find-usages args))
    ("find-definition" (tool-find-definition args))))
