(defpackage :hemis.server
  (:use :cl)
  (:import-from :hemis.tools :dispatch-tool)
  (:export :start-server))

(in-package :hemis.server)

(defun start-server (&key (port 7777))
  (declare (ignore port))
  ;; Placeholder: implement JSON-RPC server here.
  (format t "Starting Hemis JSON-RPC server (stub).~%"))

;; You'd parse incoming JSON, dispatch to appropriate functions,
;; and send JSON-RPC responses back to the UI.
