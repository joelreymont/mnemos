;;;; hemis.lisp --- Hemis JSON-RPC backend over stdio (protocol v2)
;;;
;;; Run with: sbcl --script hemis.lisp

(require :asdf)

(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql-setup)
    (load ql-setup)))

(unless (find-package :ql)
  (format *error-output* "Quicklisp is required (expected at ~/quicklisp/setup.lisp).~%")
  (uiop:quit 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (let ((*standard-output* *error-output*))
        (ql:quickload '(:jonathan :dbi :dbd-sqlite3 :split-sequence)))
    (error (c)
      (format *error-output* "Error loading deps: ~A~%" c)
      (uiop:quit 1))))

(defpackage :hemis
  (:use :cl)
  (:import-from :jonathan
                #:parse
                #:to-json)
  (:export :start-server))
(in-package :hemis)

(defparameter *db* nil)
(defparameter *db-path*
  (uiop:native-namestring
   (or (uiop:getenv "HEMIS_DB_PATH")
       (merge-pathnames "hemis-notes.db"
                        (uiop:ensure-directory-pathname (uiop:getcwd))))))
(defparameter *hemis-debug-io* (uiop:getenv "HEMIS_DEBUG_IO"))

(defun now-unix ()
  (truncate (get-universal-time)))

(defun json-obj (&rest pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun maybe-json-key (key)
  (cond
    ((stringp key) key)
    ((symbolp key) (string-downcase (symbol-name key)))
    (t (princ-to-string key))))

(defun normalize-json (value)
  (cond
    ((hash-table-p value)
     (let ((ht (make-hash-table :test 'equal)))
       (maphash (lambda (k v)
                  (setf (gethash (maybe-json-key k) ht)
                        (normalize-json v)))
                value)
       ht))
    ((listp value)
     (mapcar #'normalize-json value))
    (t value)))

(defun read-json-payload ()
  "Read a JSON payload from stdin.
Supports newline-delimited JSON or Content-Length framed messages.
Returns two values: the raw JSON string and non-nil when Content-Length
framing was used."
  (let ((line (read-line *standard-input* nil nil)))
    (cond
      ((null line) (values nil nil))
      ((uiop:string-prefix-p "Content-Length" line)
       (when *hemis-debug-io*
         (format *error-output* "RX header: ~A~%" line)
         (finish-output *error-output*))
       (let* ((len-str (string-trim '(#\Space #\Tab #\Return)
                                    (subseq line (1+ (position #\: line)))))
              (len (parse-integer len-str :junk-allowed t)))
         ;; Consume the blank separator line.
          (read-line *standard-input* nil nil)
         (let* ((buffer (make-string len))
                (read (read-sequence buffer *standard-input*)))
           (declare (ignore read))
           (when *hemis-debug-io*
             (format *error-output* "RX payload (~A chars)~%" len)
             (finish-output *error-output*))
           (values buffer t))))
      (t (values line nil)))))

(defun send-json-response (json use-headers)
  "Write JSON string JSON to stdout using framing determined by USE-HEADERS."
  (when *hemis-debug-io*
    (format *error-output* "TX payload (~A chars): ~A~%"
            (length json)
            (if (> (length json) 120)
                (concatenate 'string (subseq json 0 120) "...")
                json))
    (when use-headers
      (format *error-output* "TX header Content-Length: ~A~%" (length json)))
    (finish-output *error-output*))
  (if use-headers
      (format t "Content-Length: ~A~C~C~C~C~A~C~C"
              (length json)
              #\Return #\Linefeed #\Return #\Linefeed
              json
              #\Return #\Linefeed)
      (format t "~a~%" json))
  (finish-output))

;;; DB setup and helpers

(defun ensure-db ()
  (unless *db*
    (let* ((path (uiop:parse-native-namestring *db-path*))
           (dir  (uiop:pathname-directory-pathname path)))
      (when dir (uiop:ensure-all-directories-exist (list dir))))
    (setf *db* (dbi:connect :sqlite3 :database-name *db-path*))
    (dbi:do-sql *db*
                "CREATE TABLE IF NOT EXISTS notes (
                   id TEXT PRIMARY KEY,
                   file TEXT,
                   project_root TEXT,
                   line INTEGER,
                   column INTEGER,
                   node_path TEXT,
                   tags TEXT,
                   text TEXT,
                   summary TEXT,
                   created_at INTEGER,
                   updated_at INTEGER
                 );")
    (dbi:do-sql *db*
                "CREATE TABLE IF NOT EXISTS files (
                   file TEXT PRIMARY KEY,
                   project_root TEXT,
                   content TEXT,
                   updated_at INTEGER
                 );")))

(defun db-exec (sql &rest params)
  (ensure-db)
  (dbi:execute (dbi:prepare *db* sql) params))

(defun db-fetch-all (sql &rest params)
  (let ((stmt (apply #'db-exec sql params)))
    (loop for row = (dbi:fetch stmt)
          while row
          collect row)))

(defun maybe-to-json (value)
  (when value (to-json value)))

(defun maybe-parse-json (string)
  (when (and string (> (length string) 0))
    (parse string)))

;;; Notes

(defun make-note-id ()
  (princ-to-string (random most-positive-fixnum)))

(defun summarize-text (text)
  (cond
    ((null text) "")
    ((<= (length text) 60) text)
    (t (concatenate 'string (subseq text 0 57) "..."))))

(defun row->note (row)
  (json-obj
   "id" (getf row :|id|)
   "file" (getf row :|file|)
   "projectRoot" (getf row :|project_root|)
   "line" (getf row :|line|)
   "column" (getf row :|column|)
   "nodePath" (maybe-parse-json (getf row :|node_path|))
   "tags" (or (maybe-parse-json (getf row :|tags|)) '())
   "text" (getf row :|text|)
   "summary" (getf row :|summary|)
   "createdAt" (getf row :|created_at|)
   "updatedAt" (getf row :|updated_at|)))

(defun handle-notes-list-for-file (params)
  (let* ((file (gethash "file" params)))
    (mapcar #'row->note
            (db-fetch-all "SELECT * FROM notes WHERE file = ? ORDER BY updated_at DESC;" file))))

(defun handle-notes-list-project (params)
  (let* ((project-root (gethash "projectRoot" params)))
    (mapcar #'row->note
            (db-fetch-all "SELECT * FROM notes WHERE project_root = ? ORDER BY updated_at DESC;"
                          project-root))))

(defun handle-notes-create (params)
  (let* ((file   (gethash "file" params))
         (proj   (gethash "projectRoot" params))
         (line   (gethash "line" params))
         (column (gethash "column" params))
         (node   (gethash "nodePath" params))
         (tags   (gethash "tags" params))
         (text   (gethash "text" params))
         (id     (make-note-id))
         (ts     (now-unix))
         (summary (summarize-text text)))
    (db-exec "INSERT INTO notes
              (id, file, project_root, line, column, node_path, tags, text, summary, created_at, updated_at)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
             id file proj line column (maybe-to-json node) (maybe-to-json tags)
             text summary ts ts)
    (row->note
     (first (db-fetch-all "SELECT * FROM notes WHERE id = ?;" id)))))

(defun handle-notes-update (params)
  (let* ((id   (gethash "id" params))
         (row  (first (db-fetch-all "SELECT * FROM notes WHERE id = ?;" id))))
    (unless row
      (error "Note not found: ~A" id))
    (let* ((existing (row->note row))
           (text (or (gethash "text" params) (gethash "text" existing)))
           (tags (or (gethash "tags" params) (gethash "tags" existing)))
           (summary (summarize-text text))
           (updated (now-unix)))
      (db-exec "UPDATE notes
                SET text = ?, summary = ?, tags = ?, updated_at = ?
                WHERE id = ?;"
               text summary (maybe-to-json tags) updated id)
      (row->note (first (db-fetch-all "SELECT * FROM notes WHERE id = ?;" id))))))

(defun handle-notes-delete (params)
  (let* ((id   (gethash "id" params))
         (row  (first (db-fetch-all "SELECT * FROM notes WHERE id = ?;" id))))
    (when row
      (db-exec "DELETE FROM notes WHERE id = ?;" id))
    (json-obj "ok" (and row t))))

(defun handle-notes-search (params)
  (let* ((query (gethash "query" params))
         (proj  (gethash "projectRoot" params))
         (pattern (format nil "%~A%" query))
         (sql "SELECT * FROM notes
               WHERE (? IS NULL OR project_root = ?)
                 AND (text LIKE ? OR summary LIKE ? OR file LIKE ?);"))
    (mapcar #'row->note
            (db-fetch-all sql proj proj pattern pattern pattern))))

;;; File indexing and search

(defun handle-index-add-file (params)
  (let* ((file (gethash "file" params))
         (project-root (gethash "projectRoot" params))
         (content (or (gethash "content" params)
                      (when (and file (probe-file file))
                        (uiop:read-file-string file))))
         (updated (now-unix)))
    (when (and file content)
      (db-exec "INSERT OR REPLACE INTO files (file, project_root, content, updated_at)
                VALUES (?, ?, ?, ?);"
               file project-root content updated))
    (json-obj
     "file" file
     "projectRoot" project-root
     "bytes" (length (or content ""))
     "lines" (length (split-sequence:split-sequence #\Newline (or content "")))
     "updatedAt" updated)))

(defun search-file-for-query (file content query)
  (let ((lines (split-sequence:split-sequence #\Newline content)))
    (loop for line-text in lines
          for idx from 1
          for pos = (search query line-text :test #'char-equal)
          when pos
            collect (json-obj
                     "file" file
                     "line" idx
                     "column" pos
                     "text" line-text))))

(defun handle-index-search (params)
  (let* ((query (gethash "query" params))
         (proj  (gethash "projectRoot" params))
         (rows  (if proj
                    (db-fetch-all "SELECT file, content FROM files WHERE project_root = ?;" proj)
                    (db-fetch-all "SELECT file, content FROM files;"))))
    (loop for row in rows
          append (search-file-for-query (getf row :|file|) (getf row :|content|) query))))

;;; Dispatch

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
          ((string= method "index/add-file")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-index-add-file params)))
          ((string= method "index/search")
           (json-obj "jsonrpc" "2.0" "id" id
                     "result" (handle-index-search params)))
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
  (loop for (payload use-headers) = (multiple-value-list (read-json-payload))
        while payload do
        (let (req)
          (handler-case
              (progn
                (setf req (normalize-json (parse payload :as :hash-table)))
                (let* ((resp (dispatch-request req))
                       (json (to-json resp)))
                  (send-json-response json use-headers)
                  (when (and (hash-table-p req)
                             (string= (gethash "method" req) "shutdown"))
                    (return))))
            (error (e)
              (progn
                (let ((id (and (hash-table-p req)
                               (gethash "id" req))))
                  (send-json-response
                   (to-json
                    (json-obj
                     "jsonrpc" "2.0"
                     "id" id
                     "error" (json-obj
                              "code" -32700
                              "message" (princ-to-string e)))))
                   use-headers))
                (format *error-output* "Error handling request: ~A~%" e)
                (finish-output *error-output*))))))

;; Entry point for --script invocation
(unless (uiop:getenv "HEMIS_SKIP_SERVER")
  (start-server))
