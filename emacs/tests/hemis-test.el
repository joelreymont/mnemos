;;; hemis-test.el --- Tests for Hemis  -*- lexical-binding: t; -*-

(require 'ert)
(require 'hemis)

(defmacro hemis-test-with-mocked-backend (&rest body)
  (declare (indent 0))
  `(let (hemis-test-last-method hemis-test-last-params)
     (cl-letf (((symbol-function 'hemis--request)
                (lambda (method &optional params)
                  (setq hemis-test-last-method method
                        hemis-test-last-params params)
                  (pcase method
                    ("notes/list-for-file"
                     (list (list 'id "1"
                                 'file (cdr (assoc 'file params))
                                 'line 1
                                 'column 0
                                 'summary "Mock note")))
                    ("notes/create"
                     (list 'id "2"
                           'file (cdr (assoc 'file params))
                           'line (cdr (assoc 'line params))
                           'column (cdr (assoc 'column params))
                           'text (cdr (assoc 'text params))
                           'summary "Created"
                           'nodePath (cdr (assoc 'nodePath params))))
                    (_ (error "Unexpected method in mock: %s" method))))))
       ,@body)))

(ert-deftest hemis-refresh-notes-creates-overlay ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/main.rs" t t)
      (when (fboundp 'rust-ts-mode)
        (rust-ts-mode))
      (hemis-notes-mode 1)
      (hemis-refresh-notes)
      (should (consp hemis--overlays))
      (let ((ov (car hemis--overlays)))
        (goto-char (overlay-start ov))
        (should (= (line-number-at-pos) 1))
        (should (= (current-column) 0))))))

(ert-deftest hemis-node-path-at-point-rust ()
  (skip-unless (and (fboundp 'rust-ts-mode)
                    (fboundp 'treesit-node-at)))
  (with-temp-buffer
    (insert "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}\n")
    (set-visited-file-name "/tmp/add.rs" t t)
    (unless (hemis--ensure-rust-grammar t)
      (ert-skip "Rust Tree-sitter grammar unavailable"))
    (rust-ts-mode)
    (unless (hemis--treesit-available-p)
      (ert-skip "Rust Tree-sitter not ready"))
    (goto-char (point-min))
    (search-forward "add")
    (let* ((node (treesit-node-at (point)))
           (node-type (treesit-node-type node))
           (path (hemis--node-path-at-point)))
      (should (listp path))
      (should (stringp (car path)))
      (should (string= (car path) node-type)))))

(ert-deftest hemis-add-note-sends-node-path ()
  (skip-unless (and (fboundp 'rust-ts-mode)
                    (fboundp 'treesit-node-at)))
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}\n")
      (set-visited-file-name "/tmp/add.rs" t t)
      (unless (hemis--ensure-rust-grammar t)
        (ert-skip "Rust Tree-sitter grammar unavailable"))
      (rust-ts-mode)
      (unless (hemis--treesit-available-p)
        (ert-skip "Rust Tree-sitter not ready"))
      (goto-char (point-min))
      (search-forward "add")
      (hemis-add-note "test note")
      (should (equal hemis-test-last-method "notes/create"))
      (let ((node-path (cdr (assoc 'nodePath hemis-test-last-params))))
        (should (listp node-path))
        (should (stringp (car node-path)))))))

(provide 'hemis-test)
