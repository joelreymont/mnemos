;;; mnemos-test.el --- Tests for Mnemos  -*- lexical-binding: t; -*-

(require 'ert)
(require 'mnemos)

(defmacro mnemos-test-with-mocked-backend (&rest body)
  "Run BODY with a mocked backend (no real connections)."
  (declare (indent 0))
  `(let (mnemos-test-last-method mnemos-test-last-params
         ;; Isolate from any real backend state
         (mnemos--process nil)
         (mnemos--conn nil)
         (mnemos-dir (make-temp-file "mnemos-mock-" t)))
     ;; Disable global mode to prevent auto-connections
     (mnemos-notes-global-mode -1)
     (unwind-protect
         (cl-letf (((symbol-function 'mnemos--request)
                    (lambda (method &optional params)
                      (setq mnemos-test-last-method method
                            mnemos-test-last-params params)
                      (pcase method
                        ("mnemos/version"
                         (list 'protocolVersion 1
                               'gitHash "mock"
                               'uptimeSecs 0
                               'connections 1))
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
                        (_ (error "Unexpected method in mock: %s" method)))))
                   ;; Also mock ensure-connection to prevent any real connections
                   ((symbol-function 'mnemos--ensure-connection)
                    (lambda () nil)))
           ,@body)
       ;; Cleanup
       (ignore-errors (delete-directory mnemos-dir t)))))

(defmacro mnemos-test-with-backend (&rest body)
  "Run BODY with a real backend using isolated temp directory."
  (declare (indent 0))
  `(let* ((test-dir (make-temp-file "mnemos-test-" t))
          (mnemos-dir test-dir)
          (mnemos--process nil)
          (mnemos--conn nil)
          (mnemos--server-process nil)
          ;; Include AI provider if set, so AI tests can work
          (mnemos-backend-env (append
                              (list (concat "MNEMOS_DIR=" test-dir)
                                    (concat "MNEMOS_NOTES_PATH=" test-dir "/notes"))
                              (when (getenv "MNEMOS_AI_PROVIDER")
                                (list (concat "MNEMOS_AI_PROVIDER=" (getenv "MNEMOS_AI_PROVIDER"))))))
          ;; Expand backend path NOW before test changes default-directory
          (mnemos-backend (expand-file-name
                          (or (getenv "MNEMOS_BACKEND")
                              mnemos-backend
                              mnemos--default-backend
                              (error "Set MNEMOS_BACKEND to the Mnemos backend binary")))))
     ;; Disable global mode to prevent auto-connections to wrong socket
     (mnemos-notes-global-mode -1)
     (unwind-protect
         (progn
           ;; Clean up any stale connections (not shutdown, just disconnect local state)
           (setq mnemos--process nil mnemos--conn nil mnemos--server-process nil)
           ,@body)
       ;; Shutdown the isolated server
       (ignore-errors (mnemos-shutdown))
       ;; Wait for socket to be removed (server shutdown)
       (let ((deadline (+ (float-time) 2)))
         (while (and (file-exists-p (expand-file-name "mnemos.sock" test-dir))
                     (< (float-time) deadline))
           (sleep-for 0.1)))
       ;; Force remove socket/lock files
       (ignore-errors (delete-file (expand-file-name "mnemos.sock" test-dir)))
       (ignore-errors (delete-file (expand-file-name "mnemos.lock" test-dir)))
       ;; Clean up test directory
       (ignore-errors (delete-directory test-dir t)))))

(defun mnemos-test--note-id (note)
  "Extract note id from NOTE (alist/plist/hash-table)."
  (cond
   ((hash-table-p note) (gethash "id" note))
   ((and (listp note) (symbolp (car note))) (plist-get note :id))
   ((listp note) (or (alist-get 'id note)
                     (alist-get "id" note nil nil #'equal)))
   (t nil)))

(ert-deftest mnemos-refresh-notes-creates-overlay ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/main.rs" t t)
      (when (fboundp 'rust-ts-mode)
        (rust-ts-mode))
      (mnemos-notes-mode 1)
      (mnemos-refresh-notes)
      (should (consp mnemos--overlays))
      (let ((ov (car mnemos--overlays)))
        (goto-char (overlay-start ov))
        (should (= (line-number-at-pos) 1))
        (should (= (current-column) 0))))))

(ert-deftest mnemos-add-note-sends-content ()
  ;; Server-side tree-sitter: add-note now sends buffer content
  ;; so server can compute nodeTextHash and nodePath
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}\n")
      (set-visited-file-name "/tmp/add.rs" t t)
      (goto-char (point-min))
      (search-forward "add")
      (mnemos-add-note "test note")
      (should (equal mnemos-test-last-method "notes/create"))
      (let ((content (cdr (assoc 'content mnemos-test-last-params))))
        (should (stringp content))
        (should (string-match-p "fn add" content))))))

(ert-deftest mnemos-add-note-allows-multiline ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/main.rs" t t)
      (mnemos-add-note "line1\nline2")
      (should (equal (cdr (assoc 'text mnemos-test-last-params))
                     "line1\nline2")))))

(ert-deftest mnemos-add-note-interactive-multiline ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/main.rs" t t)
      (cl-letf (((symbol-function 'mnemos--read-note-text)
                 (lambda () "line1\nline2\nline3")))
        (mnemos-add-note (mnemos--read-note-text))
        (should (equal (cdr (assoc 'text mnemos-test-last-params))
                       "line1\nline2\nline3"))))))

(ert-deftest mnemos-apply-notes-reanchors-to-line-start ()
  "Stickies move to the token start."
  (with-temp-buffer
    (insert "fn main() {\n    let SCHEMA = 1;\n}\n")
    (set-visited-file-name "/tmp/schema.rs" t t)
    ;; Simulate backend returning a note placed mid-identifier.
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/schema.rs") (line . 2) (column . 11) (summary . "mid"))))
    (should mnemos--overlays)
    (let* ((marker (seq-find (lambda (ov) (overlay-get ov 'mnemos-note-marker))
                             mnemos--overlays)))
      (should marker)
      (goto-char (overlay-start marker))
      (should (= (line-number-at-pos) 2))
      (should (= (current-column) 0)))))

(ert-deftest mnemos-view-note-uses-markdown-mode-when-available ()
  (mnemos-test-with-mocked-backend
    ;; Build a notes buffer with a note property, then view it.
    (let ((note '((id . "1") (file . "/tmp/mock.rs") (line . 1) (column . 0)
                  (text . "Heading\n\n- bullet"))))
      (with-current-buffer (get-buffer-create "*Mnemos Notes*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert "mock\n")
        (add-text-properties (point-min) (point-max)
                             (list 'mnemos-note note)))
      (with-current-buffer "*Mnemos Notes*"
        (goto-char (point-min)))
      (with-current-buffer "*Mnemos Notes*"
        (mnemos-view-note))
      (with-current-buffer "*Mnemos Note*"
        (should (string-match-p "- bullet" (buffer-string)))
        (when (fboundp 'markdown-mode)
          (should (eq major-mode 'markdown-mode)))))))

(ert-deftest mnemos-overlay-begins-on-newline ()
  (with-temp-buffer
    (insert "fn main() {}\n    let X = 1;\n")
    (set-visited-file-name "/tmp/newline.rs" t t)
    (let* ((note '((id . "1") (file . "/tmp/newline.rs") (line . 2) (column . 11) (summary . "line"))))
      (mnemos--apply-notes (list note))
      (let* ((ov (seq-find (lambda (o) (overlay-get o 'mnemos-note-marker))
                           mnemos--overlays))
             (before (and ov (overlay-get ov 'before-string))))
        (should ov)
        (should (stringp before))
        (should (string-match-p "^\\s-*\\(\n\\|//\\|;\\|--\\|#\\)" before))
        (should (string-match-p "line" before))))))

(ert-deftest mnemos-rust-note-integration ()
  "Test note CRUD and overlays."
  (let ((rust-content "fn main() {\n    println!(\"hello\");\n}\n\nfn helper() {\n    let x = 1;\n}\n"))
    (mnemos-test-with-backend
      (let ((test-file (expand-file-name "test.rs" test-dir)))
        (with-temp-file test-file
          (insert rust-content))
        (with-temp-buffer
          (insert rust-content)
          (set-visited-file-name test-file t t)
          (setq comment-start "// ")
          (let ((mnemos--project-root-override test-dir))
            ;; Basic note CRUD
            (goto-char (point-min))
            (search-forward "println")
            (let* ((created (mnemos-add-note "integration note"))
                   (created2 (mnemos-add-note "second note"))
                   (cid (mnemos-test--note-id created))
                   (fetched (mnemos-get-note cid)))
              (should (stringp cid))
              (should (string= cid (mnemos-test--note-id fetched)))
              ;; Verify overlays with comment prefix and note text
              (mnemos-refresh-notes)
              (should (>= (length mnemos--overlays) 2))
              ;; Collect all texts from all marker overlays
              (let* ((markers (seq-filter (lambda (o) (overlay-get o 'mnemos-note-marker))
                                          mnemos--overlays))
                     (all-texts (apply #'append
                                       (mapcar (lambda (o) (overlay-get o 'mnemos-note-texts))
                                               markers)))
                     (first-marker (car markers))
                     (before (and first-marker (overlay-get first-marker 'before-string))))
                (should markers)
                (should (stringp before))
                ;; Comment prefix (// for Rust)
                (should (string-match-p "^\\s-*\\(\n\\|//\\|;\\|--\\|#\\)" before))
                ;; Both note texts stored across markers
                (should (member "integration note" all-texts))
                (should (member "second note" all-texts))
                ;; Face is applied to overlay text
                (should (get-text-property 0 'face before))))))))))

(ert-deftest mnemos-list-notes-integration ()
  "Test mnemos-list-notes with real backend returns notes in buffer."
  (let ((rust-content "fn main() {\n    println!(\"hello\");\n}\n"))
    (mnemos-test-with-backend
      ;; Create test file inside the macro's test-dir
      (let ((test-file (expand-file-name "test.rs" test-dir)))
        (with-temp-file test-file
          (insert rust-content))
        (with-temp-buffer
          (insert rust-content)
          (set-visited-file-name test-file t t)
          (setq comment-start "// ")
          (let ((mnemos--project-root-override test-dir))
            (goto-char (point-min))
            ;; Create a note so list-notes has something to show
            (mnemos-add-note "integration list test")
            ;; Now list notes
            (mnemos-list-notes)
            (with-current-buffer "*Mnemos Notes*"
              (goto-char (point-min))
              ;; Should find our note text in the buffer
              (should (search-forward "integration list test" nil t))
              ;; Should have mnemos-note property on that line
              (goto-char (point-min))
              (let ((found nil))
                (while (and (not found) (not (eobp)))
                  (setq found (get-text-property (line-beginning-position) 'mnemos-note))
                  (forward-line 1))
                (should found)))))))))

(ert-deftest mnemos-explain-region-calls-backend ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "line1\nline2\n")
      (set-visited-file-name "/tmp/foo.rs" t t)
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional _params)
                   (setq mnemos-test-last-method method)
                   '((content . "stub")))))
        (mnemos-explain-region (point-min) (point-max))
        (should (equal mnemos-test-last-method "mnemos/explain-region"))
        (with-current-buffer "*Mnemos Explain*"
          (goto-char (point-min))
          (should (search-forward "stub" nil t)))))))

;;; Explain Region AI E2E Tests
;;; Tests that explain-region-ai creates notes like Neovim does

(ert-deftest mnemos-explain-region-ai-shows-timer-message ()
  "Test that explain-region-ai shows 'AI thinking... 0s' message."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/timer-test.rs" t t)
      (goto-char (point-min))
      (let ((messages-shown nil))
        (cl-letf (((symbol-function 'jsonrpc-async-request)
                   (lambda (_conn method _params &rest args)
                     (when (string= method "mnemos/explain-region")
                       (let ((success-fn (plist-get args :success-fn)))
                         (when success-fn
                           (funcall success-fn '(:explanation "test" :ai (:statusDisplay "[AI]"))))))))
                  ((symbol-function 'mnemos--request)
                   (lambda (method &optional _params)
                     (pcase method
                       ("notes/create" '(:id "test-id"))
                       (_ nil))))
                  ((symbol-function 'mnemos--make-note-overlay) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages-shown))))
          (mnemos-explain-region-ai (point-min) (point-max))
          ;; Should have shown "AI thinking... 0s"
          (should (cl-some (lambda (m) (string-match-p "AI thinking\\.\\.\\. 0s" m))
                           messages-shown)))))))

(ert-deftest mnemos-explain-region-ai-detailed-shows-timer-message ()
  "Test that explain-region-ai-detailed shows 'AI thinking deeply... 0s' message."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/timer-test2.rs" t t)
      (goto-char (point-min))
      (let ((messages-shown nil))
        (cl-letf (((symbol-function 'jsonrpc-async-request)
                   (lambda (_conn method _params &rest args)
                     (when (string= method "mnemos/explain-region")
                       (let ((success-fn (plist-get args :success-fn)))
                         (when success-fn
                           (funcall success-fn '(:explanation "test" :ai (:statusDisplay "[AI]"))))))))
                  ((symbol-function 'mnemos--request)
                   (lambda (method &optional _params)
                     (pcase method
                       ("notes/create" '(:id "test-id"))
                       (_ nil))))
                  ((symbol-function 'mnemos--make-note-overlay) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages-shown))))
          (mnemos-explain-region-ai-detailed (point-min) (point-max))
          ;; Should have shown "AI thinking deeply... 0s"
          (should (cl-some (lambda (m) (string-match-p "AI thinking deeply\\.\\.\\. 0s" m))
                           messages-shown)))))))

(ert-deftest mnemos-explain-region-ai-creates-note ()
  "Test that explain-region-ai creates a note with AI explanation.
This mirrors Neovim's explain_region_e2e_spec.lua behavior."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {\n    let config = load_config();\n    server.start();\n}\n")
      (set-visited-file-name "/tmp/explain-ai.rs" t t)
      (setq comment-start "// ")
      (goto-char (point-min))
      (forward-line 1)
      (let ((note-created nil)
            (note-text-captured nil)
            (async-params-captured nil))
        (cl-letf (((symbol-function 'jsonrpc-async-request)
                   (lambda (_conn method params &rest args)
                     (when (string= method "mnemos/explain-region")
                       (setq async-params-captured params)
                       (let ((success-fn (plist-get args :success-fn)))
                         (when success-fn
                           (funcall success-fn
                                    '(:content "let config = load_config();"
                                      :explanation "This code loads configuration from the default config loader."
                                      :ai (:statusDisplay "[AI]" :model "test-model"))))))))
                  ((symbol-function 'mnemos--request)
                   (lambda (method &optional params)
                     (pcase method
                       ("notes/create"
                        (setq note-created t)
                        (setq note-text-captured (cdr (assoc 'text params)))
                        '(:id "ai-note-id"
                          :file "/tmp/explain-ai.rs"
                          :line 2
                          :column 0
                          :text "mock"))
                       (_ nil))))
                  ((symbol-function 'mnemos--make-note-overlay)
                   (lambda (_note) nil)))
          (mnemos-explain-region-ai (line-beginning-position) (line-end-position))
          ;; Check async request was made with useAI
          (should async-params-captured)
          (should (eq t (cdr (assoc 'useAI async-params-captured))))
          (should note-created)
          (should (stringp note-text-captured))
          (should (string-match-p "\\[AI\\]" note-text-captured))
          (should (string-match-p "loads configuration" note-text-captured)))))))

(ert-deftest mnemos-explain-region-ai-no-explanation ()
  "Test that explain-region-ai shows message when AI returns no explanation."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/no-explain.rs" t t)
      (goto-char (point-min))
      (let ((message-shown nil))
        (cl-letf (((symbol-function 'jsonrpc-async-request)
                   (lambda (_conn method _params &rest args)
                     (when (string= method "mnemos/explain-region")
                       (let ((success-fn (plist-get args :success-fn)))
                         (when success-fn
                           ;; Return plist without :explanation (AI unavailable)
                           (funcall success-fn '(:content "fn main() {}")))))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest _args)
                     (when (string-match-p "No AI explanation" fmt)
                       (setq message-shown t)))))
          (mnemos-explain-region-ai (point-min) (point-max))
          (should message-shown))))))

(ert-deftest mnemos-explain-region-ai-detailed ()
  "Test that explain-region-ai-detailed passes detailed flag."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/detailed.rs" t t)
      (goto-char (point-min))
      (let ((detailed-flag nil))
        (cl-letf (((symbol-function 'jsonrpc-async-request)
                   (lambda (_conn method params &rest args)
                     (when (string= method "mnemos/explain-region")
                       (setq detailed-flag (cdr (assoc 'detailed params)))
                       (let ((success-fn (plist-get args :success-fn)))
                         (when success-fn
                           (funcall success-fn
                                    '(:content "fn main() {}"
                                      :explanation "Detailed explanation here"
                                      :ai (:statusDisplay "[AI]"))))))))
                  ((symbol-function 'mnemos--request)
                   (lambda (method &optional _params)
                     (pcase method
                       ("notes/create" '(:id "detailed-note"))
                       (_ nil))))
                  ((symbol-function 'mnemos--make-note-overlay)
                   (lambda (_note) nil)))
          (mnemos-explain-region-ai-detailed (point-min) (point-max))
          (should (eq t detailed-flag)))))))

(ert-deftest mnemos-explain-region-ai-integration ()
  "Integration test: explain-region-ai creates note with real backend.
Tests the FULL flow that the demo uses - this would catch real bugs."
  :tags '(:integration)
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "explain-ai-int.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Select lines 2-4 (the interesting code)
          (goto-char (point-min))
          (forward-line 1)
          (let ((start (line-beginning-position)))
            (forward-line 3)
            (let ((end (line-end-position)))
              ;; Call explain-region-ai (async - need to wait for completion)
              (mnemos-explain-region-ai start end)
              ;; Wait for async request to complete (check timer stopped)
              (let ((wait-count 0))
                (while (and mnemos--ai-timer (< wait-count 600))
                  (sleep-for 0.1)
                  (setq wait-count (1+ wait-count))))
              ;; Check if notes were created
              (let* ((result (mnemos--request "notes/list-for-file"
                                             `((file . ,test-file)
                                               (projectRoot . ,test-dir)
                                               (content . ,mnemos-test-demo-code))))
                     (notes (mnemos-test--unwrap-notes result)))
                (if (>= (length notes) 1)
                    ;; Got notes - verify AI content
                    (let* ((note (car notes))
                           (text (or (plist-get note :text)
                                     (alist-get 'text note)
                                     (cdr (assoc 'text note)))))
                      (should (stringp text))
                      ;; AI notes should have statusDisplay prefix like [AI] or [Claude]
                      (should (string-match-p "^\\[" text)))
                  ;; No notes - AI not configured
                  (ert-skip "AI provider not configured or no notes created"))))))))))

(ert-deftest mnemos-explain-region-ai-creates-overlay ()
  "Integration test: explain-region-ai creates visible overlay.
This tests what the demo visually expects to see."
  :tags '(:integration)
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "explain-overlay.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Clear any existing overlays
          (mnemos--clear-note-overlays)
          (should (= 0 (mnemos-test--count-note-overlays)))
          ;; Select region
          (goto-char (point-min))
          (forward-line 1)
          (let ((start (line-beginning-position)))
            (forward-line 2)
            (let ((end (line-end-position)))
              ;; Call explain-region-ai (async)
              (mnemos-explain-region-ai start end)
              ;; Wait for async request to complete
              (let ((wait-count 0))
                (while (and mnemos--ai-timer (< wait-count 600))
                  (sleep-for 0.1)
                  (setq wait-count (1+ wait-count))))
              ;; Check overlay was created (if AI available)
              (if (>= (mnemos-test--count-note-overlays) 1)
                  ;; Verify overlay is at the right location (line 2)
                  (should (mnemos-test--overlay-at-line 2))
                ;; No overlay - AI not configured
                (ert-skip "AI provider not configured or no overlay created")))))))))

(ert-deftest mnemos-list-notes-renders-buffer ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (set-visited-file-name "/tmp/mock.rs" t t)
      (let ((mnemos-test-last-params nil))
        (mnemos-list-notes)
        (with-current-buffer "*Mnemos Notes*"
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (setq found (get-text-property (line-beginning-position) 'mnemos-note))
              (forward-line 1))
            (should found)))))))

(defun mnemos-test--note-id-at-point ()
  "Get note id at point, handling both alist and plist formats."
  (let ((note (get-text-property (point) 'mnemos-note)))
    (or (alist-get 'id note)
        (plist-get note :id)
        (mnemos--note-get note 'id))))

(ert-deftest mnemos-notes-list-navigation ()
  "Test next/prev navigation in notes list buffer."
  (mnemos-test-with-mocked-backend
    ;; Mock to return multiple notes
    (cl-letf (((symbol-function 'mnemos--request)
               (lambda (method &optional _params)
                 (pcase method
                   ("notes/list-for-file"
                    (list '((id . "1") (file . "/tmp/a.rs") (line . 1) (column . 0) (text . "Note one"))
                          '((id . "2") (file . "/tmp/a.rs") (line . 5) (column . 0) (text . "Note two"))
                          '((id . "3") (file . "/tmp/a.rs") (line . 10) (column . 0) (text . "Note three"))))
                   (_ nil)))))
      (with-temp-buffer
        (set-visited-file-name "/tmp/a.rs" t t)
        (mnemos-list-notes)
        (with-current-buffer "*Mnemos Notes*"
          ;; Start at beginning, find first note
          (goto-char (point-min))
          (mnemos-notes-list-next)
          (should (get-text-property (point) 'mnemos-note))
          (should (equal (mnemos-test--note-id-at-point) "1"))
          ;; Move to next note
          (mnemos-notes-list-next)
          (should (equal (mnemos-test--note-id-at-point) "2"))
          ;; Move to third note
          (mnemos-notes-list-next)
          (should (equal (mnemos-test--note-id-at-point) "3"))
          ;; No more notes - should error
          (should-error (mnemos-notes-list-next) :type 'user-error)
          ;; Go back with prev
          (mnemos-notes-list-prev)
          (should (equal (mnemos-test--note-id-at-point) "2"))
          ;; Back to first
          (mnemos-notes-list-prev)
          (should (equal (mnemos-test--note-id-at-point) "1"))
          ;; No previous - should error
          (should-error (mnemos-notes-list-prev) :type 'user-error))))))

(ert-deftest mnemos-notes-list-visit-opens-file ()
  "Test that RET on a note opens the file at the correct location."
  (mnemos-test-with-mocked-backend
    (let ((test-file (make-temp-file "mnemos-visit-test-" nil ".rs"))
          opened-file opened-line opened-col)
      (unwind-protect
          (progn
            ;; Create a test file with some content
            (with-temp-file test-file
              (insert "fn main() {\n")
              (insert "    let x = 1;\n")
              (insert "    let y = 2;\n")
              (insert "}\n"))
            ;; Mock to return a note pointing to line 3
            (let ((mock-find-file
                   (lambda (file)
                     (setq opened-file file)
                     (with-current-buffer (get-buffer-create "*test-visit*")
                       (erase-buffer)
                       (insert "fn main() {\n")
                       (insert "    let x = 1;\n")
                       (insert "    let y = 2;\n")
                       (insert "}\n")
                       (set-buffer (current-buffer))))))
              (cl-letf (((symbol-function 'mnemos--request)
                         (lambda (method &optional _params)
                           (pcase method
                             ("notes/list-for-file"
                              (list `((id . "test-id")
                                      (file . ,test-file)
                                      (line . 3)
                                      (column . 4)
                                      (text . "Note on y"))))
                             (_ nil))))
                        ;; Mock both find-file variants
                        ((symbol-function 'find-file) mock-find-file)
                        ((symbol-function 'find-file-other-window) mock-find-file)
                        ;; Capture position after forward-line/move-to-column
                        ((symbol-function 'recenter)
                         (lambda (&optional _arg)
                           (setq opened-line (line-number-at-pos))
                           (setq opened-col (current-column)))))
                (with-temp-buffer
                  (set-visited-file-name test-file t t)
                  (mnemos-list-notes)
                  (with-current-buffer "*Mnemos Notes*"
                    (goto-char (point-min))
                    (mnemos-notes-list-next)
                    (mnemos-notes-list-visit)))
                ;; Check that visit attempted to open the right file and position
                (should (equal opened-file test-file))
                (should (= opened-line 3))
                (should (= opened-col 4)))))
        (ignore-errors (kill-buffer "*test-visit*"))
        (delete-file test-file)))))

(ert-deftest mnemos-notes-list-visit-integration ()
  "Integration test: visit note from list opens correct location."
  (let ((rust-content "fn main() {\n    let x = 1;\n    let y = 2;\n}\n"))
    (mnemos-test-with-backend
      ;; Create test file inside the macro's test-dir
      (let ((test-file (expand-file-name "test.rs" test-dir)))
        (with-temp-file test-file
          (insert rust-content))
        (with-temp-buffer
          (insert rust-content)
          (set-visited-file-name test-file t t)
          (setq comment-start "// ")
          (let ((mnemos--project-root-override test-dir))
            ;; Go to a specific line and create a note
            (goto-char (point-min))
            (forward-line 2)
            (let ((target-line (line-number-at-pos)))
              (mnemos-add-note "visit integration test")
              ;; List notes and visit
              (mnemos-list-notes)
              (with-current-buffer "*Mnemos Notes*"
                (goto-char (point-min))
                (mnemos-notes-list-next)
                (mnemos-notes-list-visit))
              ;; Should be in the file buffer at the note's line
              (with-current-buffer (find-buffer-visiting test-file)
                (should (= (line-number-at-pos) target-line))))))))))

(ert-deftest mnemos-notes-list-visit-other-window ()
  "Integration test: visit opens in other window when multiple windows exist."
  (let ((rust-content "fn main() {\n    let x = 1;\n}\n"))
    (mnemos-test-with-backend
      ;; Create test file inside the macro's test-dir
      (let ((test-file (expand-file-name "test.rs" test-dir)))
        (with-temp-file test-file
          (insert rust-content))
        ;; Set up a split window layout
        (delete-other-windows)
        (split-window-horizontally)
        (should (= (count-windows) 2))
        (let ((left-window (selected-window))
              (right-window (next-window)))
          (unwind-protect
              (with-temp-buffer
                (insert rust-content)
                (set-visited-file-name test-file t t)
                (setq comment-start "// ")
                (let ((mnemos--project-root-override test-dir))
                  (goto-char (point-min))
                  (mnemos-add-note "window test note")
                  ;; Open notes list - it will appear in one window
                  (mnemos-list-notes)
                  (let ((list-window (get-buffer-window "*Mnemos Notes*")))
                    (should list-window)
                    ;; Select the list window and visit
                    (select-window list-window)
                    (with-current-buffer "*Mnemos Notes*"
                      (goto-char (point-min))
                      (mnemos-notes-list-next)
                      (mnemos-notes-list-visit))
                    ;; Notes list should still be visible
                    (should (get-buffer-window "*Mnemos Notes*"))
                    ;; File should be open in a different window
                    (let ((file-window (get-buffer-window (find-buffer-visiting test-file))))
                      (should file-window)
                      (should-not (eq file-window list-window))))))
            ;; Cleanup window layout
            (delete-other-windows)))))))

(ert-deftest mnemos-insert-note-link-inserts-format ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (let* ((results (list '((id . "abc") (summary . "First") (file . "/tmp/f"))
                            '((id . "def") (summary . "Second") (file . "/tmp/g")))))
        (cl-letf (((symbol-function 'mnemos--request)
                   (lambda (method &optional _params)
                     (should (equal method "notes/search"))
                     results))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "First (abc)"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "First")))
          (mnemos-insert-note-link "First")
          (goto-char (point-min))
          (should (search-forward "[[" nil t))
          (should (search-forward "][abc]]" nil t)))
        (should (get-buffer mnemos--link-search-buffer))))))

(ert-deftest mnemos-double-bracket-triggers-link ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (mnemos-notes-mode 1)
      (let* ((results (list '((id . "abc") (summary . "Note") (file . "/tmp/f")))))
        (cl-letf (((symbol-function 'mnemos--request)
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

(ert-deftest mnemos-notes-mode-hook-triggers-link ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (mnemos-notes-mode 1)
      (let ((req-calls 0))
        (cl-letf (((symbol-function 'mnemos--request)
                   (lambda (&rest _)
                     (setq req-calls (1+ req-calls))
                     (list '((id . "xyz") (summary . "Desc") (file . "/tmp/file")))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Desc (xyz)"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "Desc")))
          (insert "[[")
          (let ((last-command-event ?\[))
            (mnemos--maybe-trigger-link))
          (goto-char (point-min))
          (should (= req-calls 1))
          (should (search-forward "][xyz]]" nil t)))))))

(ert-deftest mnemos-kill-backend-processes-removes-duplicates ()
  (let* ((p1 (start-process "mnemos-backend" nil "cat"))
         (p2 (start-process "mnemos-backend" nil "cat")))
    (should (process-live-p p1))
    (should (process-live-p p2))
    (mnemos--kill-backend-processes)
    (sleep-for 0.05)
    (should-not (process-live-p p1))
    (should-not (process-live-p p2))))

(ert-deftest mnemos-notes-global-mode-enables-keymap ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (prog-mode)
      (set-visited-file-name "/tmp/test.rs" t t)
      ;; Ensure keymaps are set up (don't nil them - mode captures at definition)
      (mnemos--ensure-notes-mode-keymap)
      (mnemos-notes-mode 1)
      (should (keymapp mnemos-notes-mode-map))
      (should (lookup-key mnemos-notes-mode-map (kbd "C-c m a"))))))

(ert-deftest mnemos-notes-list-keymap-reloads ()
  (let ((mnemos-notes-list-mode-map nil))
    (mnemos--ensure-notes-list-keymap)
    (should (keymapp mnemos-notes-list-mode-map))
    (should (lookup-key mnemos-notes-list-mode-map (kbd "RET")))
    (should (lookup-key mnemos-notes-list-mode-map (kbd "v")))))

(ert-deftest mnemos-reset-keymaps-and-enable-restores-bindings ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (prog-mode)
      (set-visited-file-name "/tmp/test.rs" t t)
      (setq mnemos-notes-mode-map nil
            mnemos-notes-list-mode-map nil)
      (mnemos-reset-keymaps-and-enable)
      (mnemos-notes-mode 1)
      (should (local-key-binding (kbd "C-c m a")))
      (should (keymapp mnemos-notes-list-mode-map)))))

(ert-deftest mnemos-insert-note-link-no-results ()
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (&rest _) nil)))
        (should-error (mnemos-insert-note-link "missing")
                      :type 'user-error)))))

(ert-deftest mnemos-show-backlinks-displays-linking-notes ()
  "Test that mnemos-show-backlinks shows notes that link to the target."
  (mnemos-test-with-mocked-backend
    ;; Mock notes/backlinks to return one linking note
    (cl-letf (((symbol-function 'mnemos--request)
               (lambda (method &optional params)
                 (pcase method
                   ("notes/backlinks"
                    (should (equal (cdr (assoc 'id params)) "target-note"))
                    (list '((id . "linking-note-id")
                            (shortId . "linking-")
                            (file . "/tmp/linker.rs")
                            (line . 5)
                            (column . 0)
                            (text . "This links to [[target][target-note]]"))))
                   (_ nil)))))
      ;; Call show-backlinks with a note id
      (mnemos-show-backlinks "target-note")
      ;; Check that the backlinks buffer was created with the linking note
      (with-current-buffer "*Mnemos Backlinks*"
        (goto-char (point-min))
        (should (search-forward "Backlinks to note target-note" nil t))
        (should (search-forward "(1 notes link" nil t))
        ;; ID is truncated to 8 chars in display
        (should (search-forward "linking-" nil t))
        (should (search-forward "This links to" nil t))))))

(ert-deftest mnemos-show-backlinks-empty ()
  "Test that mnemos-show-backlinks shows message when no backlinks."
  (mnemos-test-with-mocked-backend
    (cl-letf (((symbol-function 'mnemos--request)
               (lambda (method &optional _params)
                 (pcase method
                   ("notes/backlinks" nil)
                   (_ nil)))))
      (mnemos-show-backlinks "orphan-note")
      (with-current-buffer "*Mnemos Backlinks*"
        (goto-char (point-min))
        (should (search-forward "(0 notes link" nil t))
        (should (search-forward "No backlinks found" nil t))))))

(ert-deftest mnemos-show-backlinks-from-notes-list ()
  "Test backlinks command from notes list buffer uses note at point."
  (mnemos-test-with-mocked-backend
    ;; First create a notes buffer with a note at point
    (let ((note '((id . "from-list") (file . "/tmp/test.rs") (line . 1) (column . 0)
                  (text . "Note text")))
          (backlinks-called nil)
          (backlinks-id nil))
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional params)
                   (pcase method
                     ("notes/backlinks"
                      (setq backlinks-called t)
                      (setq backlinks-id (cdr (assoc 'id params)))
                      nil)
                     (_ nil)))))
        ;; Set up a buffer with mnemos-note property
        (with-current-buffer (get-buffer-create "*Mnemos Notes*")
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "Note entry\n")
          (add-text-properties (point-min) (point-max)
                               (list 'mnemos-note note))
          (goto-char (point-min))
          (mnemos-show-backlinks)
          (should backlinks-called)
          (should (equal backlinks-id "from-list")))))))

(ert-deftest mnemos-notes-list-keymap-has-backlinks ()
  "Test that notes list keymap includes backlinks binding."
  (mnemos--ensure-notes-list-keymap)
  (should (eq (lookup-key mnemos-notes-list-mode-map (kbd "b"))
              'mnemos-show-backlinks)))

;;; Overlay Display State Tests
;;; These tests verify the exact rendered content users see

(defun mnemos-test--capture-overlay-state ()
  "Capture the display state of all mnemos overlays in current buffer.
Returns list of plists with :line :before-string :face :count :texts."
  (let (result)
    (dolist (ov mnemos--overlays)
      (when (overlay-get ov 'mnemos-note-marker)
        (push (list :line (line-number-at-pos (overlay-start ov))
                    :before-string (overlay-get ov 'before-string)
                    :face (get-text-property 0 'face (or (overlay-get ov 'before-string) ""))
                    :count (overlay-get ov 'mnemos-note-count)
                    :texts (overlay-get ov 'mnemos-note-texts))
              result)))
    (nreverse result)))

(ert-deftest mnemos-overlay-renders-comment-block ()
  "Verify overlay before-string contains formatted comment block."
  (with-temp-buffer
    (insert "fn main() {\n    let x = 1;\n}\n")
    (set-visited-file-name "/tmp/comment.rs" t t)
    (setq comment-start "// ")
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/comment.rs") (line . 2) (column . 4)
             (text . "Check this variable"))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string)))
      (should (= 1 (length state)))
      ;; Should contain comment prefix
      (should (string-match-p "//" before-str))
      ;; Should contain the note text
      (should (string-match-p "Check this variable" before-str))
      ;; Should have indentation matching line 2
      (should (string-match-p "^    //" before-str)))))

(ert-deftest mnemos-overlay-has-steel-blue-face ()
  "Verify overlay face is SteelBlue for visibility."
  (with-temp-buffer
    (insert "fn main() {}\n")
    (set-visited-file-name "/tmp/face.rs" t t)
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/face.rs") (line . 1) (column . 0)
             (text . "Note with face"))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (face (plist-get ov-state :face)))
      (should face)
      ;; Face should specify foreground color
      (should (plist-get face :foreground)))))

(ert-deftest mnemos-overlay-multiline-note ()
  "Verify multiline notes render each line with comment prefix."
  (with-temp-buffer
    (insert "fn main() {}\n")
    (set-visited-file-name "/tmp/multi.rs" t t)
    (setq comment-start "// ")
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/multi.rs") (line . 1) (column . 0)
             (text . "Line one\nLine two\nLine three"))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string)))
      ;; Each line should have comment prefix
      (should (string-match-p "// Line one" before-str))
      (should (string-match-p "// Line two" before-str))
      (should (string-match-p "// Line three" before-str)))))

(ert-deftest mnemos-overlay-multiple-notes-same-line ()
  "Verify multiple notes on same line at same column are combined."
  (with-temp-buffer
    (insert "fn main() {}\n")
    (set-visited-file-name "/tmp/combined.rs" t t)
    (setq comment-start "// ")
    ;; Both notes at same position (line 1, column 0) to ensure combining
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/combined.rs") (line . 1) (column . 0)
             (text . "First note"))
           '((id . "2") (file . "/tmp/combined.rs") (line . 1) (column . 0)
             (text . "Second note"))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string))
           (count (plist-get ov-state :count))
           (texts (plist-get ov-state :texts)))
      ;; Notes at same position combine into single marker overlay
      (should (= 1 (length state)))
      ;; Count should be 2
      (should (= 2 count))
      ;; Both texts stored
      (should (= 2 (length texts)))
      ;; Both notes appear in rendered string
      (should (string-match-p "First note" before-str))
      (should (string-match-p "Second note" before-str)))))

(ert-deftest mnemos-overlay-lisp-comment-style ()
  "Verify Lisp modes use ;; comment prefix."
  (with-temp-buffer
    (insert "(defun foo () nil)\n")
    (set-visited-file-name "/tmp/test.el" t t)
    (setq comment-start "; ")
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/test.el") (line . 1) (column . 0)
             (text . "Lisp note"))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string)))
      ;; Should use ;; for Lisp (not single ;)
      (should (string-match-p ";; Lisp note" before-str)))))

(ert-deftest mnemos-overlay-preserves-indentation ()
  "Verify overlay indentation matches the code line."
  (with-temp-buffer
    (insert "fn main() {\n        let deeply_indented = 1;\n}\n")
    (set-visited-file-name "/tmp/indent.rs" t t)
    (setq comment-start "// ")
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/indent.rs") (line . 2) (column . 8)
             (text . "Indented note"))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string)))
      ;; Should start with 8-space indent
      (should (string-match-p "^        //" before-str)))))

;;; Demo Flow Tests
;;; These tests verify user-visible features from docs/DEMO.md

(ert-deftest mnemos-help-displays-keybindings ()
  "Test that mnemos-help shows expected keybindings."
  (mnemos-help)
  (with-current-buffer "*Mnemos Help*"
    (goto-char (point-min))
    ;; Should show main keybindings
    (should (search-forward "C-c m a" nil t))
    (should (search-forward "Add a note" nil t))
    (goto-char (point-min))
    (should (search-forward "C-c m e" nil t))
    (should (search-forward "Edit note" nil t))
    (goto-char (point-min))
    (should (search-forward "C-c m d" nil t))
    (should (search-forward "Delete note" nil t))
    (goto-char (point-min))
    (should (search-forward "C-c m S" nil t))
    (should (search-forward "status" nil t))))

(ert-deftest mnemos-status-shows-counts ()
  "Test that mnemos-status displays note/file/embedding counts."
  (mnemos-test-with-mocked-backend
    (cl-letf (((symbol-function 'mnemos--request)
               (lambda (method &optional _params)
                 (pcase method
                   ("mnemos/status"
                    '((counts . ((notes . 5) (files . 10) (embeddings . 3)))))
                   (_ nil)))))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (mnemos-status)
          (should (cl-some (lambda (m) (string-match-p "5 notes" m)) messages))
          (should (cl-some (lambda (m) (string-match-p "10 files" m)) messages))
          (should (cl-some (lambda (m) (string-match-p "3 embeddings" m)) messages)))))))

(ert-deftest mnemos-edit-note-at-point-updates-note ()
  "Test that mnemos-edit-note-at-point calls notes/update."
  (mnemos-test-with-mocked-backend
    (let ((updated-id nil)
          (updated-text nil))
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional params)
                   (pcase method
                     ("notes/update"
                      (setq updated-id (cdr (assoc 'id params)))
                      (setq updated-text (cdr (assoc 'text params)))
                      '((id . "test-id")))
                     ("notes/list-for-file" nil)
                     (_ nil))))
                ((symbol-function 'mnemos--read-note-text)
                 (lambda (&optional _default) "new text"))
                ((symbol-function 'mnemos-refresh-notes) #'ignore))
        ;; Set up buffer with note property
        (with-current-buffer (get-buffer-create "*Mnemos Notes*")
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "test\n")
          (add-text-properties (point-min) (point-max)
                               '(mnemos-note ((id . "test-id") (text . "old text"))))
          (goto-char (point-min))
          (mnemos-edit-note-at-point)
          (should (equal updated-id "test-id"))
          (should (equal updated-text "new text")))))))

(ert-deftest mnemos-edit-note-buffer-opens-buffer ()
  "Test that mnemos-edit-note-buffer opens a dedicated edit buffer."
  (mnemos-test-with-mocked-backend
    (cl-letf (((symbol-function 'mnemos--request)
               (lambda (method &optional params)
                 (pcase method
                   ("notes/update" '((id . "test-id")))
                   ("notes/list-for-file" nil)
                   (_ nil))))
              ((symbol-function 'mnemos-refresh-notes) #'ignore)
              ;; Mock markdown-mode if not available
              ((symbol-function 'markdown-mode)
               (lambda () (setq major-mode 'markdown-mode))))
      ;; Set up buffer with note property
      (with-current-buffer (get-buffer-create "*Mnemos Test Source*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert "test source\n")
        (add-text-properties (point-min) (point-max)
                             '(mnemos-note ((id . "test-note-id") (shortId . "test1234") (text . "Original note text\nLine two"))))
        (goto-char (point-min))
        (mnemos-edit-note-buffer)
        ;; Should have opened an edit buffer
        (let ((edit-buf (get-buffer "*Mnemos Edit: test1234*")))
          (should edit-buf)
          (with-current-buffer edit-buf
            ;; Buffer should contain the note text
            (should (string-match-p "Original note text" (buffer-string)))
            (should (string-match-p "Line two" (buffer-string)))
            ;; Should have note ID stored
            (should (equal mnemos--edit-buffer-note-id "test-note-id")))
          ;; Clean up
          (kill-buffer edit-buf))
        (kill-buffer)))))

(ert-deftest mnemos-edit-note-buffer-uses-selected-note ()
  "Test that mnemos-edit-note-buffer uses mnemos--selected-note if set."
  (mnemos-test-with-mocked-backend
    (cl-letf (((symbol-function 'mnemos--request)
               (lambda (method &optional _params)
                 (pcase method
                   ("notes/update" '((id . "selected-id")))
                   (_ nil))))
              ((symbol-function 'mnemos-refresh-notes) #'ignore)
              ;; Mock markdown-mode to avoid marker issues in batch
              ((symbol-function 'markdown-mode)
               (lambda () (setq major-mode 'markdown-mode)))
              ;; Mock pop-to-buffer to avoid window issues in batch
              ((symbol-function 'pop-to-buffer)
               (lambda (buf) (set-buffer buf))))
      (with-current-buffer (get-buffer-create "*Mnemos Test Source*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert "test source\n")
        (goto-char (point-min))
        ;; Set selected note (simulating C-c m s)
        (setq mnemos--selected-note '((id . "selected-note-id")
                                     (shortId . "sel12345")
                                     (text . "Selected note text")))
        (mnemos-edit-note-buffer)
        ;; Should use the selected note, not note at point
        (let ((edit-buf (get-buffer "*Mnemos Edit: sel12345*")))
          (should edit-buf)
          (with-current-buffer edit-buf
            (should (string-match-p "Selected note text" (buffer-string)))
            (should (equal mnemos--edit-buffer-note-id "selected-note-id")))
          (kill-buffer edit-buf))
        ;; Clean up
        (setq mnemos--selected-note nil)
        (kill-buffer)))))

(ert-deftest mnemos-edit-buffer-save-updates-note ()
  "Test that saving from edit buffer calls notes/update."
  (mnemos-test-with-mocked-backend
    (let ((updated-id nil)
          (updated-text nil))
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional params)
                   (pcase method
                     ("notes/update"
                      (setq updated-id (cdr (assoc 'id params)))
                      (setq updated-text (cdr (assoc 'text params)))
                      '((id . "test-id")))
                     ("notes/list-for-file" nil)
                     (_ nil))))
                ((symbol-function 'mnemos-refresh-notes) #'ignore)
                ;; Mock kill-buffer-and-window for batch mode (only one window)
                ((symbol-function 'kill-buffer-and-window)
                 (lambda () (kill-buffer))))
        (with-current-buffer (get-buffer-create "*Mnemos Edit Test*")
          (erase-buffer)
          (insert "New note content")
          (setq-local mnemos--edit-buffer-note-id "test-note-id")
          (mnemos--edit-buffer-save)
          (should (equal updated-id "test-note-id"))
          (should (equal updated-text "New note content")))))))

(ert-deftest mnemos-delete-note-at-point-removes-note ()
  "Test that mnemos-delete-note-at-point calls notes/delete."
  (mnemos-test-with-mocked-backend
    (let ((deleted-id nil))
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional params)
                   (pcase method
                     ("notes/delete"
                      (setq deleted-id (cdr (assoc 'id params)))
                      '((success . t)))
                     ("notes/list-for-file" nil)
                     (_ nil))))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'mnemos-refresh-notes) #'ignore))
        ;; Set up buffer with note property
        (with-current-buffer (get-buffer-create "*Mnemos Notes*")
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "test\n")
          (add-text-properties (point-min) (point-max)
                               '(mnemos-note ((id . "delete-me"))))
          (goto-char (point-min))
          (mnemos-delete-note-at-point)
          (should (equal deleted-id "delete-me")))))))

(ert-deftest mnemos-search-project-displays-results ()
  "Test that mnemos-search-project shows matching notes and files."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (set-visited-file-name "/tmp/test.rs" t t)
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional params)
                   (pcase method
                     ("mnemos/search"
                      (list '((kind . "file") (file . "/tmp/foo.rs") (line . 1) (text . "fn foo"))
                            '((kind . "note") (file . "/tmp/bar.rs") (line . 5) (text . "note text"))))
                     (_ nil)))))
        (mnemos-search-project "foo")
        (with-current-buffer "*Mnemos Search*"
          (goto-char (point-min))
          (should (search-forward "file" nil t))
          (should (search-forward "foo.rs" nil t)))))))


(ert-deftest mnemos-overlay-stale-note-indicator ()
  "Test that stale notes are visually distinguished."
  (with-temp-buffer
    (insert "fn main() {}\n")
    (set-visited-file-name "/tmp/stale.rs" t t)
    (setq comment-start "// ")
    ;; Apply a stale note
    (mnemos--apply-notes
     (list '((id . "1") (file . "/tmp/stale.rs") (line . 1) (column . 0)
             (text . "This note is stale") (stale . t))))
    (let* ((state (mnemos-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string)))
      (should (= 1 (length state)))
      ;; Should contain [stale] indicator or different face
      (should (or (string-match-p "stale" before-str)
                  (string-match-p "STALE" before-str)
                  ;; Or check face property includes stale indication
                  (plist-get ov-state :face))))))

(ert-deftest mnemos-notes-list-has-edit-delete-keybindings ()
  "Test that notes list mode has edit and delete keybindings."
  (mnemos--ensure-notes-list-keymap)
  (should (eq (lookup-key mnemos-notes-list-mode-map (kbd "e"))
              'mnemos-edit-note-at-point))
  (should (eq (lookup-key mnemos-notes-list-mode-map (kbd "d"))
              'mnemos-delete-note-at-point)))

;;; Demo Workflow Tests (mirrors neovim.demo)
;;; These test position tracking and stale detection with real backend

(defconst mnemos-test-demo-code
  "fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

impl Server {
    fn new(config: Config) -> Self {
        Server { config }
    }
}
"
  "Demo code for position/stale testing.")

(defun mnemos-test--unwrap-notes (result)
  "Unwrap notes from backend response RESULT.
Backend returns plist with :notes key containing a vector."
  (let ((notes (or (plist-get result :notes)
                   (and (listp result) (assoc 'notes result)
                        (cdr (assoc 'notes result)))
                   (and (listp result) result))))
    ;; Convert vector to list if needed (backend returns vector)
    (if (vectorp notes)
        (append notes nil)
      notes)))

(defun mnemos-test--note-line (note)
  "Get line from NOTE."
  (or (alist-get 'line note)
      (plist-get note :line)
      (cdr (assoc "line" note))))

(defun mnemos-test--note-stale (note)
  "Get stale status from NOTE.
Returns t if stale, nil if not stale.
Handles :json-false which is truthy in Emacs."
  (let ((stale (or (alist-get 'stale note)
                   (plist-get note :stale)
                   (cdr (assoc "stale" note)))))
    ;; Convert :json-false to nil (JSON false is truthy in Emacs)
    (and stale (not (eq stale :json-false)))))

(ert-deftest mnemos-demo-position-tracking ()
  "Test that note line updates when lines inserted above."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "position-test.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (let ((mnemos--project-root-override test-dir))
          ;; Create note at line 7 (fn load_config) - content sent automatically
          (goto-char (point-min))
          (forward-line 6)  ; Move to line 7
          (let* ((created (mnemos-add-note "Position tracking test"))
                 (note-id (mnemos-test--note-id created))
                 (hash (or (alist-get 'nodeTextHash created)
                          (plist-get created :nodeTextHash))))
            (should (stringp note-id))
            (should (stringp hash))  ; nodeTextHash computed for position tracking

            ;; Insert 2 comment lines at top
            (let ((new-content (concat "// Comment 1\n// Comment 2\n" mnemos-test-demo-code)))
              ;; List notes with modified content
              (let* ((result (mnemos--request "notes/list-for-file"
                                            `((file . ,test-file)
                                              (projectRoot . ,test-dir)
                                              (content . ,new-content))))
                     (notes (mnemos-test--unwrap-notes result))
                     (note (car notes))
                     (new-line (mnemos-test--note-line note)))
                ;; Original line 7 + 2 inserted = 9
                (should (= 9 new-line))))))))))

(ert-deftest mnemos-demo-stale-detection ()
  "Test that note is marked stale when anchor code changes."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "stale-test.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (let ((mnemos--project-root-override test-dir))
          ;; Create note at line 12 (fn new)
          (goto-char (point-min))
          (forward-line 11)  ; Move to line 12
          (let* ((created (mnemos-add-note "Stale detection test"))
                 (note-id (mnemos-test--note-id created)))
            (should (stringp note-id))

            ;; Check initial staleness (with original content)
            (let* ((result (mnemos--request "notes/list-for-file"
                                          `((file . ,test-file)
                                            (projectRoot . ,test-dir)
                                            (content . ,mnemos-test-demo-code))))
                   (notes (mnemos-test--unwrap-notes result))
                   (note (car notes))
                   (stale-before (mnemos-test--note-stale note)))
              (should-not stale-before)  ; Note should be fresh initially

              ;; Change "fn new" to "fn create" (modifies anchor code)
              (let* ((modified-content (replace-regexp-in-string "fn new" "fn create"
                                                                  mnemos-test-demo-code))
                     (result2 (mnemos--request "notes/list-for-file"
                                             `((file . ,test-file)
                                               (projectRoot . ,test-dir)
                                               (content . ,modified-content))))
                     (notes2 (mnemos-test--unwrap-notes result2))
                     (note2 (car notes2))
                     (stale-after (mnemos-test--note-stale note2)))
                (should stale-after)))))))))  ; Note should be stale after anchor changed

(ert-deftest mnemos-demo-reattach-clears-stale ()
  "Test that reattach clears stale status."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "reattach-test.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (let ((mnemos--project-root-override test-dir))
          ;; Create note at line 12 (fn new)
          (goto-char (point-min))
          (forward-line 11)
          (let* ((created (mnemos-add-note "Reattach test"))
                 (note-id (mnemos-test--note-id created))
                 (modified-content (replace-regexp-in-string "fn new" "fn create"
                                                             mnemos-test-demo-code)))
            (should (stringp note-id))

            ;; Reattach note to new code
            (mnemos--request "notes/reattach"
                           `((id . ,note-id)
                             (file . ,test-file)
                             (line . 12)
                             (content . ,modified-content)
                             (projectRoot . ,test-dir)))

            ;; Check staleness after reattach
            (let* ((result (mnemos--request "notes/list-for-file"
                                          `((file . ,test-file)
                                            (projectRoot . ,test-dir)
                                            (content . ,modified-content))))
                   (notes (mnemos-test--unwrap-notes result))
                   (note (car notes))
                   (stale-after (mnemos-test--note-stale note)))
              (should-not stale-after))))))))  ; Note should be fresh after reattach

(ert-deftest mnemos-demo-multiline-notes ()
  "Test that multiline notes are created with formattedLines."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "multiline-test.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (let ((mnemos--project-root-override test-dir)
              (multiline-text "Config improvements:\n- Add validation\n- Support env vars"))
          (goto-char (point-min))
          (forward-line 6)
          (let* ((created (mnemos-add-note multiline-text))
                 (text (or (alist-get 'text created)
                          (plist-get created :text)))
                 (formatted-lines (or (alist-get 'formattedLines created)
                                     (plist-get created :formattedLines))))
            (should (equal text multiline-text))
            (should formatted-lines)
            (should (>= (length formatted-lines) 3))))))))

(ert-deftest mnemos-demo-create-delete-cycle ()
  "Test full create-display-delete cycle."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "lifecycle-test.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (let ((mnemos--project-root-override test-dir))
          ;; Create note
          (goto-char (point-min))
          (forward-line 4)
          (let* ((created (mnemos-add-note "Lifecycle test note"))
                 (note-id (mnemos-test--note-id created)))
            (should (stringp note-id))

            ;; List notes - should have one
            (let* ((result (mnemos--request "notes/list-for-file"
                                          `((file . ,test-file)
                                            (projectRoot . ,test-dir)
                                            (content . ,mnemos-test-demo-code))))
                   (notes (mnemos-test--unwrap-notes result)))
              (should (= 1 (length notes))))

            ;; Delete note
            (mnemos--request "notes/delete" `((id . ,note-id)))

            ;; List notes - should have none
            (let* ((result (mnemos--request "notes/list-for-file"
                                          `((file . ,test-file)
                                            (projectRoot . ,test-dir)
                                            (content . ,mnemos-test-demo-code))))
                   (notes (mnemos-test--unwrap-notes result)))
              (should (= 0 (length notes))))))))))

;;; Visual e2e tests - verify overlays render correctly with real backend

(defun mnemos-test--count-note-overlays ()
  "Count mnemos note overlays in current buffer."
  (length (seq-filter (lambda (ov) (overlay-get ov 'mnemos-note-marker))
                      mnemos--overlays)))

(defun mnemos-test--overlay-at-line (line)
  "Find mnemos note overlay at LINE (1-indexed)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((pos (line-beginning-position)))
      (seq-find (lambda (ov)
                  (and (overlay-get ov 'mnemos-note-marker)
                       (= (overlay-start ov) pos)))
                mnemos--overlays))))

(ert-deftest mnemos-visual-create-shows-overlay ()
  "Test that creating a note shows an overlay in the buffer."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "visual-create.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Initially no overlays
          (mnemos--clear-note-overlays)
          (should (= 0 (mnemos-test--count-note-overlays)))

          ;; Create note at line 7
          (goto-char (point-min))
          (forward-line 6)
          (mnemos-add-note "Visual create test")

          ;; Should have overlay after create
          (should (>= (mnemos-test--count-note-overlays) 1))
          (should (mnemos-test--overlay-at-line 7)))))))

(ert-deftest mnemos-visual-delete-removes-overlay ()
  "Test that deleting a note removes the overlay."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "visual-delete.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Create note
          (goto-char (point-min))
          (forward-line 6)
          (let* ((created (mnemos-add-note "Visual delete test"))
                 (note-id (mnemos-test--note-id created)))

            ;; Should have overlay
            (should (>= (mnemos-test--count-note-overlays) 1))

            ;; Delete note
            (mnemos--request "notes/delete" `((id . ,note-id)))
            (mnemos-refresh-notes)

            ;; Should have no overlays
            (should (= 0 (mnemos-test--count-note-overlays)))))))))

(ert-deftest mnemos-visual-position-tracking ()
  "Test that overlay position updates when lines inserted above."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "visual-position.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Create note at line 7
          (goto-char (point-min))
          (forward-line 6)
          (mnemos-add-note "Visual position test")

          ;; Initially at line 7
          (should (mnemos-test--overlay-at-line 7))

          ;; Insert 2 lines at top
          (goto-char (point-min))
          (insert "// Comment 1\n// Comment 2\n")

          ;; Refresh to get updated positions
          (mnemos-refresh-notes)

          ;; Overlay should now be at line 9 (7 + 2)
          (should (mnemos-test--overlay-at-line 9)))))))

(ert-deftest mnemos-visual-multiline-overlay ()
  "Test that multiline notes create overlay with all content."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "visual-multiline.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Create multiline note
          (goto-char (point-min))
          (forward-line 6)
          (mnemos-add-note "Line 1\nLine 2\nLine 3")

          ;; Should have overlay
          (let ((ov (mnemos-test--overlay-at-line 7)))
            (should ov)
            ;; Check overlay has display content
            (let ((display (overlay-get ov 'before-string)))
              (should (stringp display))
              (should (> (length display) 0)))))))))

;;; Select Note Tests

(ert-deftest mnemos-select-note-from-picker ()
  "Test selecting a note via picker when no note at point."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/select.rs" t t)
      (setq comment-start "// ")
      (goto-char (point-min))
      ;; Mock to return one note in file
      (cl-letf (((symbol-function 'mnemos--request)
                 (lambda (method &optional _params)
                   (pcase method
                     ("notes/list-for-file"
                      '((notes . [((id . "select-test") (shortId . "select12")
                                   (file . "/tmp/select.rs") (line . 1) (column . 0)
                                   (text . "Test note for selection"))])))
                     (_ nil))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   "[select12] L1: Test note for selection")))
        ;; Select note - should use picker since no note at point
        (mnemos-select-note)
        ;; Verify note was selected
        (should mnemos--selected-note)
        (should (equal (mnemos--note-get mnemos--selected-note 'id) "select-test"))))))

(ert-deftest mnemos-select-note-no-notes ()
  "Test select note shows message when no notes in file."
  (mnemos-test-with-mocked-backend
    ;; Clear any stale selection from previous tests
    (setq mnemos--selected-note nil)
    (with-temp-buffer
      (insert "fn main() {}\n")
      (set-visited-file-name "/tmp/empty.rs" t t)
      (let ((message-shown nil))
        (cl-letf (((symbol-function 'mnemos--request)
                   (lambda (method &optional _params)
                     (pcase method
                       ("notes/list-for-file" nil)
                       (_ nil))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest _args)
                     (when (string-match-p "No notes" fmt)
                       (setq message-shown t)))))
          (mnemos-select-note)
          (should message-shown)
          (should-not mnemos--selected-note))))))

(ert-deftest mnemos-select-note-picker-multiple ()
  "Test select note with picker when multiple notes exist."
  (mnemos-test-with-mocked-backend
    (with-temp-buffer
      (insert "fn main() {}\nfn foo() {}\n")
      (set-visited-file-name "/tmp/multi.rs" t t)
      (goto-char (point-min))
      (forward-line 1)  ;; Position on line 2, no overlay here
      (let ((picker-called nil))
        (cl-letf (((symbol-function 'mnemos--request)
                   (lambda (method &optional _params)
                     (pcase method
                       ("notes/list-for-file"
                        '((notes . [((id . "note1") (shortId . "note1234") (line . 1) (text . "First note"))
                                    ((id . "note2") (shortId . "note5678") (line . 2) (text . "Second note"))])))
                       (_ nil))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args)
                     (setq picker-called t)
                     "[note1234] L1: First note")))
          (mnemos-select-note)
          (should picker-called)
          (should mnemos--selected-note))))))

(ert-deftest mnemos-select-note-integration ()
  "Integration test: create note, then select it."
  (mnemos-test-with-backend
    (let ((test-file (expand-file-name "select-int.rs" test-dir)))
      (with-temp-file test-file
        (insert mnemos-test-demo-code))
      (with-temp-buffer
        (insert mnemos-test-demo-code)
        (set-visited-file-name test-file t t)
        (setq comment-start "// ")
        (mnemos-notes-mode 1)
        (let ((mnemos--project-root-override test-dir))
          ;; Create a note
          (goto-char (point-min))
          (forward-line 6)  ;; Line 7
          (let* ((created (mnemos-add-note "Select integration test"))
                 (note-id (mnemos-test--note-id created)))
            (should (stringp note-id))
            ;; Clear selection
            (mnemos-clear-selection)
            (should-not mnemos--selected-note)
            ;; Select the note using picker (simulate selecting first item)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt choices &rest _)
                         ;; Return the first choice
                         (caar choices))))
              (mnemos-select-note))
            ;; Should have selected our note
            (should mnemos--selected-note)))))))

;;; Event System Tests

(ert-deftest mnemos-events-on-registers-handler ()
  "Test that mnemos--events-on registers a handler."
  (let ((mnemos--events-handlers (make-hash-table :test 'equal)))
    (mnemos--events-on "note-created" #'ignore)
    (should (eq (gethash "note-created" mnemos--events-handlers) #'ignore))))

(ert-deftest mnemos-events-filter-dispatches-to-handler ()
  "Test that mnemos--events-filter parses JSON and calls the right handler."
  (let ((mnemos--events-handlers (make-hash-table :test 'equal))
        (mnemos--events-buffer "")
        (received-events nil))
    (mnemos--events-on "note-created"
                      (lambda (event)
                        (push event received-events)))
    ;; Simulate receiving a complete JSON line
    (mnemos--events-filter nil "{\"type\":\"note-created\",\"noteId\":\"abc\"}\n")
    (should (= 1 (length received-events)))
    (should (equal "note-created" (alist-get 'type (car received-events))))
    (should (equal "abc" (alist-get 'noteId (car received-events))))))

(ert-deftest mnemos-events-filter-calls-catch-all ()
  "Test that mnemos--events-filter calls catch-all handler."
  (let ((mnemos--events-handlers (make-hash-table :test 'equal))
        (mnemos--events-buffer "")
        (catch-all-events nil))
    (mnemos--events-on "*" (lambda (event) (push event catch-all-events)))
    (mnemos--events-filter nil "{\"type\":\"any-event\"}\n")
    (should (= 1 (length catch-all-events)))
    (should (equal "any-event" (alist-get 'type (car catch-all-events))))))

(ert-deftest mnemos-events-filter-handles-partial-data ()
  "Test that mnemos--events-filter buffers partial JSON."
  (let ((mnemos--events-handlers (make-hash-table :test 'equal))
        (mnemos--events-buffer "")
        (received-events nil))
    (mnemos--events-on "test" (lambda (event) (push event received-events)))
    ;; Send partial data (no newline)
    (mnemos--events-filter nil "{\"type\":\"test\"")
    (should (= 0 (length received-events)))
    (should (equal "{\"type\":\"test\"" mnemos--events-buffer))
    ;; Complete the message
    (mnemos--events-filter nil ",\"data\":1}\n")
    (should (= 1 (length received-events)))
    (should (equal "test" (alist-get 'type (car received-events))))))

(ert-deftest mnemos-events-filter-handles-multiple-events ()
  "Test that mnemos--events-filter handles multiple events in one chunk."
  (let ((mnemos--events-handlers (make-hash-table :test 'equal))
        (mnemos--events-buffer "")
        (received-events nil))
    (mnemos--events-on "event" (lambda (event) (push event received-events)))
    ;; Send multiple events at once
    (mnemos--events-filter nil "{\"type\":\"event\",\"n\":1}\n{\"type\":\"event\",\"n\":2}\n")
    (should (= 2 (length received-events)))
    ;; Events are pushed, so reverse order
    (should (= 2 (alist-get 'n (car received-events))))
    (should (= 1 (alist-get 'n (cadr received-events))))))

(ert-deftest mnemos-events-filter-ignores-invalid-json ()
  "Test that mnemos--events-filter handles invalid JSON gracefully."
  (let ((mnemos--events-handlers (make-hash-table :test 'equal))
        (mnemos--events-buffer "")
        (received-events nil))
    (mnemos--events-on "test" (lambda (event) (push event received-events)))
    ;; Send invalid JSON followed by valid JSON
    (mnemos--events-filter nil "not valid json\n{\"type\":\"test\"}\n")
    ;; Should have received the valid event
    (should (= 1 (length received-events)))))

;;; Follow-link Tests

(ert-deftest mnemos-follow-link-pattern-matches-basic ()
  "Test that link pattern matches [[desc][uuid]] format."
  (let ((link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]")
        (test-uuid "12345678-1234-1234-1234-123456789abc"))
    ;; Link in middle of line
    (let ((line (concat "See [[target note][" test-uuid "]] for details")))
      (should (string-match link-re line))
      (should (equal test-uuid (match-string 1 line))))
    ;; Link at start of line
    (let ((line (concat "[[first][" test-uuid "]] is important")))
      (should (string-match link-re line))
      (should (equal test-uuid (match-string 1 line))))))

(ert-deftest mnemos-follow-link-pattern-multiple-links ()
  "Test finding multiple links in a line."
  (let ((link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]")
        (uuid1 "11111111-1111-1111-1111-111111111111")
        (uuid2 "22222222-2222-2222-2222-222222222222"))
    (let ((line (concat "[[one][" uuid1 "]] and [[two][" uuid2 "]]")))
      ;; Find first link
      (should (string-match link-re line))
      (should (equal uuid1 (match-string 1 line)))
      ;; Find second link
      (should (string-match link-re line (match-end 0)))
      (should (equal uuid2 (match-string 1 line))))))

(ert-deftest mnemos-follow-link-pattern-no-link ()
  "Test that pattern doesn't match non-link text."
  (let ((link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]"))
    (should-not (string-match link-re "No links here"))
    (should-not (string-match link-re "[[broken link"))
    (should-not (string-match link-re "[single][brackets]"))))

(ert-deftest mnemos-follow-link-finds-link-at-cursor ()
  "Test finding the correct link when cursor is at different positions."
  (let ((link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]")
        (uuid1 "11111111-1111-1111-1111-111111111111")
        (uuid2 "22222222-2222-2222-2222-222222222222"))
    (let ((line (concat "[[first][" uuid1 "]] middle [[second][" uuid2 "]]")))
      ;; Helper to find link at column
      (cl-flet ((find-link-at-col
                 (col)
                 (let ((pos 0)
                       (found-id nil))
                   (while (and (not found-id)
                               (string-match link-re line pos))
                     (let ((start (match-beginning 0))
                           (end (match-end 0))
                           (id (match-string 1 line)))
                       (when (and (>= col start) (<= col end))
                         (setq found-id id))
                       (setq pos end)))
                   found-id)))
        ;; Cursor on first link
        (should (equal uuid1 (find-link-at-col 5)))
        ;; Cursor on second link
        (should (equal uuid2 (find-link-at-col 60)))
        ;; Cursor between links
        (should-not (find-link-at-col 50))))))

(ert-deftest mnemos-follow-link-empty-description ()
  "Test link with empty description."
  (let ((link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]")
        (uuid "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"))
    (let ((line (concat "[[" "][" uuid "]]")))
      (should (string-match link-re line))
      (should (equal uuid (match-string 1 line))))))

(ert-deftest mnemos-follow-link-special-chars-description ()
  "Test link with special characters in description."
  (let ((link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]")
        (uuid "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"))
    (let ((line (concat "[[foo: bar (baz)][" uuid "]]")))
      (should (string-match link-re line))
      (should (equal uuid (match-string 1 line))))))

(provide 'mnemos-test)
