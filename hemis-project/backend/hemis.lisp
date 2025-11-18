(defpackage :hemis
  (:use :cl)
  (:export :start-server))

(in-package :hemis)

;; Very small stub for the Hemis Lisp backend.
;; Here you'll:
;; - wire up Tree-sitter (via cl-tree-sitter)
;; - implement RPC handlers for the UI (parse-buffer, list-notes, etc.)

(defun handle-request (json-request)
  "Given a decoded JSON request from the UI, dispatch and return a response object."
  (declare (ignore json-request))
  ;; TODO: implement dispatch based on :method and :params
  (list :ok t :message "Hemis backend stub is alive."))

(defun start-server (&key (port 4000))
  "Start a simple HTTP/WebSocket server that the Lauri/React UI can talk to."
  (declare (ignore port))
  ;; TODO: integrate with your chosen Lisp HTTP / WS library.
  (format t "Starting Hemis backend stub...~%"))
