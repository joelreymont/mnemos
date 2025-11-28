;;;; test-notes.lisp --- minimal backend test for note node_path persistence

(require :asdf)

(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql-setup)
    (load ql-setup)))

(setf (uiop:getenv "HEMIS_SKIP_SERVER") "1")

(load "hemis-project/backend/hemis.lisp")
(in-package :hemis)

(defun fail (msg)
  (format *error-output* "FAIL: ~A~%" msg)
  (finish-output *error-output*)
  (uiop:quit 1))

(defun ok (msg)
  (format t "OK: ~A~%" msg)
  (finish-output))

(let* ((tmp (uiop:with-temporary-file (:pathname tmp-path :keep t :suffix ".db")
              tmp-path))
       (*db-path* (uiop:native-namestring tmp))
       (*db* nil))
  (ensure-db)
  (let* ((params (make-hash-table :test 'equal))
         (node '("function_item" "parameters")))
    (setf (gethash "file" params) "/tmp/test.rs")
    (setf (gethash "projectRoot" params) "/tmp")
    (setf (gethash "line" params) 1)
    (setf (gethash "column" params) 0)
    (setf (gethash "text" params) "hello")
    (setf (gethash "nodePath" params) node)
    (handle-notes-create params)
    (let* ((notes (handle-notes-list-for-file params))
           (note (first notes)))
      (unless (= 1 (length notes))
        (fail "expected one note inserted"))
      (let ((saved (gethash "nodePath" note)))
        (unless (and (listp saved)
                     (every #'stringp saved)
                     (equal saved node))
          (fail (format nil "nodePath mismatch: ~A" saved)))))
    (ok "nodePath persisted")))

(uiop:quit 0)
