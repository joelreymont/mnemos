(defpackage :hemis.core
  (:use :cl)
  (:export :log-info :log-error))

(in-package :hemis.core)

(defun log-info (fmt &rest args)
  (apply #'format *standard-output* (concatenate 'string "[INFO] " fmt "~%") args))

(defun log-error (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "[ERROR] " fmt "~%") args))
