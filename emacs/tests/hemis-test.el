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
                                 'summary "Mock note"
                                 'text "Mock note body")))
                    ("notes/get"
                     (list 'id (cdr (assoc 'id params))
                           'file "/tmp/mock.rs"
                           'line 1
                           'column 0
                           'text "Title\n\n- bullet"))
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

(defmacro hemis-test-with-backend (&rest body)
  "Run BODY with a real backend using a temp SQLite DB."
  (declare (indent 0))
  `(let* ((hemis-backend-env (list (concat "HEMIS_DB_PATH=" (make-temp-file "hemis-test-db"))))
          (hemis-backend (or (getenv "HEMIS_BACKEND")
                             hemis-backend
                             hemis--default-backend
                             (error "Set HEMIS_BACKEND to the Rust backend binary"))))
     (unwind-protect
         (progn
           (hemis-shutdown)
           ,@body)
       (hemis-shutdown))))

(defun hemis-test--note-id (note)
  "Extract note id from NOTE (alist/plist/hash-table)."
  (cond
   ((hash-table-p note) (gethash "id" note))
   ((and (listp note) (symbolp (car note))) (plist-get note :id))
   ((listp note) (or (alist-get 'id note)
                     (alist-get "id" note nil nil #'equal)))
   (t nil)))

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
          (should (sequencep node-path))
          (should (stringp (elt node-path 0)))))))

(ert-deftest hemis-add-note-allows-multiline ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/main.rs" t t)
      (hemis-add-note "line1\nline2")
      (should (equal (cdr (assoc 'text hemis-test-last-params))
                     "line1\nline2")))))

(ert-deftest hemis-add-note-anchors-to-node-start ()
  (skip-unless (and (fboundp 'rust-ts-mode)
                    (fboundp 'treesit-node-at)))
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {\n    let SCHEMA = 1;\n}\n")
      (set-visited-file-name "/tmp/schema.rs" t t)
      (unless (hemis--ensure-rust-grammar t)
        (ert-skip "Rust Tree-sitter grammar unavailable"))
      (rust-ts-mode)
      (unless (hemis--treesit-available-p)
        (ert-skip "Rust Tree-sitter not ready"))
      ;; Place point inside the symbol to ensure we anchor to its start.
      (goto-char (point-min))
      (search-forward "SCH")
      (backward-char 1)
      (let* ((node (treesit-node-at (point)))
             (start (treesit-node-start node)))
        (hemis-add-note "anchor test")
        (let ((line (cdr (assoc 'line hemis-test-last-params)))
              (col  (cdr (assoc 'column hemis-test-last-params))))
          (save-excursion
            (goto-char start)
            (should (= line (line-number-at-pos)))
            (should (= col (current-column)))))))))

(ert-deftest hemis-apply-notes-reanchors-marker ()
  (skip-unless (and (fboundp 'rust-ts-mode)
                    (fboundp 'treesit-node-at)))
  (with-temp-buffer
    (insert "fn main() {\n    let SCHEMA = 1;\n}\n")
    (set-visited-file-name "/tmp/schema.rs" t t)
    (unless (hemis--ensure-rust-grammar t)
      (ert-skip "Rust Tree-sitter grammar unavailable"))
    (rust-ts-mode)
    (unless (hemis--treesit-available-p)
      (ert-skip "Rust Tree-sitter not ready"))
    ;; Simulate backend returning a note placed mid-identifier.
    (hemis--apply-notes
     (list '((id . "1") (file . "/tmp/schema.rs") (line . 2) (column . 11) (summary . "mid"))))
    (should hemis--overlays)
    (let* ((marker (seq-find (lambda (ov) (overlay-get ov 'hemis-note-marker))
                             hemis--overlays)))
      (should marker)
      (goto-char (overlay-start marker))
      ;; Identifier starts at column 8 for \"    let SCHEMA = 1;\".
      (should (= (line-number-at-pos) 2))
      (should (= (current-column) 8)))))

(ert-deftest hemis-apply-notes-reanchors-without-treesit ()
  "Even without Tree-sitter, stickies move to the token start."
  (let ((hemis-auto-install-treesit-grammars nil))
    (cl-letf (((symbol-function 'hemis--treesit-available-p) (lambda () nil)))
      (with-temp-buffer
        (insert "fn main() {\n    let SCHEMA = 1;\n}\n")
        (set-visited-file-name "/tmp/schema.rs" t t)
        ;; Simulate backend returning a note placed mid-identifier.
        (hemis--apply-notes
         (list '((id . "1") (file . "/tmp/schema.rs") (line . 2) (column . 11) (summary . "mid"))))
        (should hemis--overlays)
        (let* ((marker (seq-find (lambda (ov) (overlay-get ov 'hemis-note-marker))
                                 hemis--overlays)))
          (should marker)
          (goto-char (overlay-start marker))
          (should (= (line-number-at-pos) 2))
          (should (= (current-column) 8)))))))

(ert-deftest hemis-view-note-uses-markdown-mode-when-available ()
  (hemis-test-with-mocked-backend
    ;; Build a notes buffer with a note property, then view it.
    (let ((note '((id . "1") (file . "/tmp/mock.rs") (line . 1) (column . 0)
                  (text . "Heading\n\n- bullet"))))
      (with-current-buffer (get-buffer-create "*Hemis Notes*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert "mock\n")
        (add-text-properties (point-min) (point-max)
                             (list 'hemis-note note)))
      (with-current-buffer "*Hemis Notes*"
        (goto-char (point-min)))
      (with-current-buffer "*Hemis Notes*"
        (hemis-view-note))
      (with-current-buffer "*Hemis Note*"
        (should (string-match-p "- bullet" (buffer-string)))
        (when (fboundp 'markdown-mode)
          (should (eq major-mode 'markdown-mode)))))))

(ert-deftest hemis-index-rust-integration ()
  (skip-unless (and (fboundp 'rust-ts-mode)
                    (file-readable-p "../bebop/util/src/lib.rs")))
  (hemis-test-with-backend
    (let* ((file (expand-file-name "../bebop/util/src/lib.rs" default-directory)))
      (with-temp-buffer
        (insert-file-contents file)
        (set-visited-file-name file t t)
        (rust-ts-mode)
        (unless (hemis--ensure-rust-grammar t)
          (ert-skip "Rust Tree-sitter grammar unavailable"))
        (unless (hemis--treesit-available-p)
          (ert-skip "Rust Tree-sitter not ready"))
        (let ((proj (hemis--project-root)))
          (hemis-index-file)
          (goto-char (point-min))
          (search-forward "fn")
          (let ((local-path (hemis--node-path-at-point)))
            (should (sequencep local-path))
            (should (stringp (elt local-path 0))))
          (let* ((created (hemis-add-note "integration note"))
                 (created2 (hemis-add-note "integration note 2"))
                 (cid (hemis-test--note-id created))
                 (fetched (hemis-get-note cid)))
            (should (stringp cid))
            (should (string= cid (hemis-test--note-id fetched)))
            (should (equal (or (alist-get 'nodePath created)
                               (alist-get "nodePath" created nil nil #'equal)
                               (and (hash-table-p created) (gethash "nodePath" created)))
                           (or (alist-get 'nodePath fetched)
                               (alist-get "nodePath" fetched nil nil #'equal)
                               (and (hash-table-p fetched) (gethash "nodePath" fetched))))))
          (hemis-refresh-notes)
          (should (>= (length hemis--overlays) 2))
          (let* ((results (hemis--request "index/search"
                                          `((query . "fn")
                                            (projectRoot . ,proj)))))
            (should (sequencep results))
            (should (> (length results) 0))
            (let* ((by-node (hemis-notes-for-node (hemis--node-path-at-point))))
              (should (sequencep by-node))
              (should (>= (length by-node) 2)))
            ;; Ensure backend returns stored nodePath for created note.
            (let* ((notes (hemis--request "notes/list-for-file"
                                          `((file . ,file)
                                            (projectRoot . ,proj))))
                   (first-note (seq-first notes))
                   (node-path (or (alist-get 'nodePath first-note)
                                  (plist-get first-note :nodePath))))
              (should (sequencep notes))
              (should (sequencep node-path))
              (should (stringp (elt node-path 0))))))))))

(ert-deftest hemis-explain-region-calls-backend ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (insert "line1\nline2\n")
      (set-visited-file-name "/tmp/foo.rs" t t)
      (cl-letf (((symbol-function 'hemis--request)
                 (lambda (method &optional _params)
                   (setq hemis-test-last-method method)
                   '((explanation . "stub")))))
        (hemis-explain-region (point-min) (point-max))
        (should (equal hemis-test-last-method "hemis/explain-region"))
        (with-current-buffer "*Hemis Explain*"
          (goto-char (point-min))
          (should (search-forward "stub" nil t)))))))

(ert-deftest hemis-list-files-and-view-file ()
  (hemis-test-with-mocked-backend
    (cl-letf* (((symbol-function 'hemis--request)
                (lambda (method &rest _)
                  (pcase method
                    ("hemis/list-files" (list "/tmp/a" "/tmp/b"))
                    ("hemis/get-file" '((content . "hello")))
                    (_ (error "unexpected %s" method))))))
      (hemis-list-files "/tmp")
      (with-current-buffer "*Hemis Files*"
        (goto-char (point-min))
        (should (search-forward "/tmp/a" nil t)))
      (hemis-view-file "/tmp/a")
      (with-current-buffer "*Hemis File*"
        (goto-char (point-min))
        (should (search-forward "hello" nil t))))))

(ert-deftest hemis-insert-note-link-inserts-format ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (let* ((results (list '((id . "abc") (summary . "First") (file . "/tmp/f"))
                            '((id . "def") (summary . "Second") (file . "/tmp/g")))))
        (cl-letf (((symbol-function 'hemis--request)
                   (lambda (method &optional _params)
                     (should (equal method "notes/search"))
                     results))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "First (abc)"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "First")))
          (hemis-insert-note-link "First")
          (goto-char (point-min))
          (should (search-forward "[[" nil t))
          (should (search-forward "][abc]]" nil t)))
        (should (get-buffer hemis--link-search-buffer))))))

(ert-deftest hemis-double-bracket-triggers-link ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (hemis-notes-mode 1)
      (let* ((results (list '((id . "abc") (summary . "Note") (file . "/tmp/f")))))
        (cl-letf (((symbol-function 'hemis--request)
                   (lambda (&rest _) results))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Note (abc)"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "Note")))
          (let ((last-command-event ?\[))
            (self-insert-command 1)
            (let ((last-command-event ?\[))
              (self-insert-command 1)))
          (goto-char (point-min))
          (should (search-forward "][abc]]" nil t)))))))

(ert-deftest hemis-notes-mode-hook-triggers-link ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (hemis-notes-mode 1)
      (let ((req-calls 0))
        (cl-letf (((symbol-function 'hemis--request)
                   (lambda (&rest _)
                     (setq req-calls (1+ req-calls))
                     (list '((id . "xyz") (summary . "Desc") (file . "/tmp/file")))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Desc (xyz)"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "Desc")))
          (insert "[[")
          (let ((last-command-event ?\[))
            (hemis--maybe-trigger-link))
          (goto-char (point-min))
          (should (= req-calls 1))
          (should (search-forward "][xyz]]" nil t)))))))

(ert-deftest hemis-insert-note-link-no-results ()
  (hemis-test-with-mocked-backend
    (with-temp-buffer
      (cl-letf (((symbol-function 'hemis--request)
                 (lambda (&rest _) nil)))
        (should-error (hemis-insert-note-link "missing")
                      :type 'user-error)))))

(provide 'hemis-test)
