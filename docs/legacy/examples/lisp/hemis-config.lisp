(defpackage :hemis.config
  (:use :cl)
  (:export :*config* :load-config))

(in-package :hemis.config)

(defparameter *config* (make-hash-table :test 'equal))

(defun load-config (path)
  ;; Placeholder: parse TOML/JSON and populate *config*
  (setf (gethash "engine.type" *config*) "llama-cpp")
  (setf (gethash "engine.host" *config*) "127.0.0.1")
  (setf (gethash "engine.port" *config*) 4010)
  *config*)
