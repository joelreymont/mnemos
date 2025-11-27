;;;; hemis.lisp --- Hemis JSON-RPC backend over stdio (protocol v2)
;;;
;;; Run with: sbcl --script hemis.lisp

(require :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (ql:quickload :jonathan)
    (error (c)
      (format *error-output* "Error loading jonathan: ~A~%" c)
      (uiop:quit 1))))

(defpackage :hemis
  (:use :cl)
  (:import-from :jonathan
                #:parse
                #:to-json)
  (:export :start-server))
(in-package :hemis)

(defvar *notes-by-id* (make-hash-table :test 'equal))
(defvar *notes-by-file* (make-hash-table :test 'equal))
(defvar *next-id* 0)

(defun now-unix ()
  (truncate (get-universal-time)))

(defun json-obj (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun ensure-notes-list-for-file (file)
  (or (gethash file *notes-by-file*)
      (setf (gethash file *notes-by-file*) '())))

(defun make-note-id ()
  (princ-to-string (incf *next-id*)))

(defun summarize-text (text)
  (cond
    ((null text) "")
    ((<= (length text) 60) text)
    (t (concatenate 'string (subseq text 0 57) "..."))))

(defun make-note (&key file project-root line column node-path tags text)
  (let* ((id (make-note-id))
         (ts (now-unix))
         (summary (summarize-text text))
         (note (json-obj
                "id" id
                "file" file
                "projectRoot" project-root
                "line" line
                "column" column
                "nodePath" node-path
                "tags" (or tags '())
                "text" text
                "summary" summary
                "createdAt" ts
                "updatedAt" ts)))
    note))

(defun index-note (note)
  (let* ((id   (gethash "id" note))
         (file (gethash "file" note)))
    (setf (gethash id *notes-by-id*) note)
    (when file
      (let ((lst (ensure-notes-list-for-file file)))
        (push note lst)
        (setf (gethash file *notes-by-file*) lst)))))

(defun remove-note-from-indexes (note)
  (let* ((id   (gethash "id" note))
         (file (gethash "file" note)))
    (remhash id *notes-by-id*)
    (when file
      (let ((lst (ensure-notes-list-for-file file)))
        (setf (gethash file *notes-by-file*)
              (remove note lst :test #'eq))))))

(defun find-note-by-id (id)
  (gethash id *notes-by-id*))

(defun handle-notes-list-for-file (params)
  (let* ((file (gethash "file" params)))
    (copy-list (ensure-notes-list-for-file file))))

(defun handle-notes-list-project (params)
  (let* ((project-root (gethash "projectRoot" params)))
    (loop for note being the hash-values of *notes-by-id*
          when (equal (gethash "projectRoot" note) project-root)
            collect note)))

(defun handle-notes-create (params)
  (let* ((file   (gethash "file" params))
         (proj   (gethash "projectRoot" params))
         (line   (gethash "line" params))
         (column (gethash "column" params))
         (node   (gethash "nodePath" params))
         (tags   (gethash "tags" params))
         (text   (gethash "text" params))
         (note   (make-note :file file
                            :project-root proj
                            :line line
                            :column column
                            :node-path node
                            :tags tags
                            :text text)))
    (index-note note)
    note))

(defun handle-notes-update (params)
  (let* ((id   (gethash "id" params))
         (text (gethash "text" params))
         (tags (gethash "tags" params))
         (note (find-note-by-id id)))
    (unless note
      (error "Note not found: ~A" id))
    (when text
      (setf (gethash "text" note) text
            (gethash "summary" note) (summarize-text text)))
    (when tags
      (setf (gethash "tags" note) tags))
    (setf (gethash "updatedAt" note) (now-unix))
    note))

(defun handle-notes-delete (params)
  (let* ((id   (gethash "id" params))
         (note (find-note-by-id id)))
    (if note
        (progn
          (remove-note-from-indexes note)
          (json-obj "ok" t))
        (json-obj "ok" nil))))

(defun note-matches-query-p (note query)
  (let* ((q (string-downcase (or query "")))
         (text (string-downcase (or (gethash "text" note) "")))
         (file (string-downcase (or (gethash "file" note) "")))
         (summary (string-downcase (or (gethash "summary" note) ""))))
    (or (search q text)
        (search q file)
        (search q summary))))

(defun handle-notes-search (params)
  (let* ((query (gethash "query" params))
         (proj  (gethash "projectRoot" params)))
    (loop for note being the hash-values of *notes-by-id*
          when (and (or (null proj)
                        (equal (gethash "projectRoot" note) proj))
                    (note-matches-query-p note query))
            collect note)))

(defun dispatch-request (req)
  (let* ((id     (gethash "id" req))
         (method (gethash "method" req))
         (params (or (gethash "params" req)
                     (make-hash-table :test 'equal))))
    (handler-case
        (cond
          ((string= method "notes/list-for-file")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-notes-list-for-file params)))
          ((string= method "notes/list-project")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-notes-list-project params)))
          ((string= method "notes/create")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-notes-create params)))
          ((string= method "notes/update")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-notes-update params)))
          ((string= method "notes/delete")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-notes-delete params)))
          ((string= method "notes/search")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-notes-search params)))
          ((string= method "shutdown")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" "shutting down"))
          (t
           (json-obj "jsonrpc" "2.0" "id" id
                     "error" (json-obj
                              "code" -32601
                              "message" (format nil "Method not found: ~A" method)))))
      (error (e)
        (json-obj "jsonrpc" "2.0" "id" id
                  "error" (json-obj
                           "code" -32000
                           "message" (princ-to-string e)))))))

(defun start-server ()
  "Run the Hemis JSON-RPC loop over stdio until EOF or shutdown method."
  (loop
     for line = (read-line *standard-input* nil nil)
     while line do
       (handler-case
           (let* ((req  (parse line))
                  (resp (dispatch-request req))
                  (json (to-json resp)))
             (format t "~a~%" json)
             (finish-output)
             (when (and (hash-table-p req)
                        (string= (gethash "method" req) "shutdown"))
               (return)))
         (error (e)
           (format *error-output* "Error handling request: ~A~%" e)
           (finish-output *error-output*)))))

;; Entry point for --script invocation
(start-server)
