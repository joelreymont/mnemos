;;; hemis.el --- Hemis: a second brain for your code  -*- lexical-binding: t; -*-

;; Emacs frontend for the Hemis Rust backend (protocol v2).
;; - JSON-RPC 2.0 over stdio
;; - Notes minor mode with stickies as overlays
;; - Notes list buffer
;; - Project root awareness (for multi-file/project operations)

(require 'jsonrpc)
(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'treesit nil t)

(defgroup hemis nil
  "Hemis – a second brain for your code."
  :group 'tools)

(defconst hemis--default-backend
  (let* ((here (file-name-directory (or load-file-name buffer-file-name)))
         (root (expand-file-name ".." here))
         (candidate (expand-file-name "target/debug/backend" root)))
    (when (file-exists-p candidate) candidate))
  "Default path to the Hemis Rust backend binary (if built locally).")

;; Tree-sitter language remaps for rust-ts-mode -> rust
(when (featurep 'treesit)
  (when (boundp 'treesit-language-remap-alist)
    (add-to-list 'treesit-language-remap-alist '(rust-ts-mode . rust)))
  (when (boundp 'treesit-major-mode-language-alist)
    (add-to-list 'treesit-major-mode-language-alist '(rust-ts-mode . rust))
    (add-to-list 'treesit-major-mode-language-alist '(rust-mode . rust)))
  ;; Treat rust-ts-mode availability as rust availability.
  (when (fboundp 'treesit-language-available-p)
    (defun hemis--advice-treesit-language-available (orig lang &rest args)
      (or (when (eq lang 'rust-ts-mode)
            (apply orig 'rust args))
          (apply orig lang args)))
    (advice-add 'treesit-language-available-p :around
                #'hemis--advice-treesit-language-available)))

(defcustom hemis-backend hemis--default-backend
  "Path to the Hemis backend binary (Rust JSON-RPC over stdio)."
  :type 'string
  :group 'hemis)

(defcustom hemis-log-buffer "*Hemis Log*"
  "Name of the buffer used to log Hemis backend output."
  :type 'string
  :group 'hemis)

(defcustom hemis-backend-env nil
  "List of environment variables (\"KEY=VAL\") passed to the Hemis backend process."
  :type '(repeat string)
  :group 'hemis)

(defcustom hemis-auto-install-treesit-grammars t
  "When non-nil, attempt to install required Tree-sitter grammars (e.g., Rust) automatically."
  :type 'boolean
  :group 'hemis)

(defcustom hemis-request-timeout 60
  "Timeout (seconds) for JSON-RPC requests to the Hemis backend."
  :type 'integer
  :group 'hemis)

(defface hemis-note-marker-face
  '((t :inherit font-lock-comment-face :box t))
  "Face for Hemis sticky note markers."
  :group 'hemis)

(defvar hemis--process nil
  "Process object for the Hemis backend.")

(defvar hemis--conn nil
  "JSON-RPC connection to the Hemis backend.")

(defvar hemis--overlays nil
  "List of Hemis note overlays in the current buffer.")

(defun hemis--git-run (default-directory &rest args)
  "Run git ARGS in DEFAULT-DIRECTORY, returning trimmed output or nil."
  (when (and default-directory (executable-find "git"))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (when (zerop exit)
          (string-trim (buffer-string)))))))

(defun hemis--git-info (file)
  "Return alist with commit/blob for FILE, or nil if not in git."
  (let* ((default-directory (file-name-directory file))
         (root (hemis--git-run default-directory "rev-parse" "--show-toplevel"))
         (commit (and root (hemis--git-run root "rev-parse" "HEAD")))
         (blob (and root (hemis--git-run root "hash-object" file))))
    (when commit
      `((commit . ,commit)
        (blob . ,blob)))))


;;; Process & JSON-RPC management

(defun hemis--start-process ()
  "Start the Hemis backend process if it is not already running."
  (unless (and hemis--process (process-live-p hemis--process))
    (let* ((buf (get-buffer-create hemis-log-buffer))
           (exe (or hemis-backend (error "Set `hemis-backend` to the Rust backend binary")))
           (args nil)
           (process-environment (append hemis-backend-env process-environment))
           (proc (make-process
                  :name "hemis-backend"
                  :buffer buf
                  :command (cons exe args)
                  :connection-type 'pipe
                  :stderr buf
                  :coding 'no-conversion)))
      (set-process-query-on-exit-flag proc nil)
      (setq hemis--process proc)
      (setq hemis--conn
            (jsonrpc-process-connection
             :process proc
             :on-shutdown (lambda (&rest _)
                            (message "Hemis backend shut down")
                            (setq hemis--process nil
                                  hemis--conn nil))))
      (message "Hemis backend started."))))

(defun hemis--ensure-connection ()
  "Ensure that the Hemis backend connection is live."
  (unless (and hemis--conn
               (jsonrpc-running-p hemis--conn))
    (hemis--start-process)))

(defun hemis-shutdown ()
  "Shutdown the Hemis backend."
  (interactive)
  (when (and hemis--conn (jsonrpc-running-p hemis--conn))
    (jsonrpc-notify hemis--conn "shutdown" nil))
  (when (and hemis--process (process-live-p hemis--process))
    (kill-process hemis--process))
  (setq hemis--process nil
        hemis--conn nil)
  (message "Hemis backend stopped."))

(defun hemis--request (method &optional params)
  "Synchronously send JSON-RPC METHOD with PARAMS and return result."
  (hemis--ensure-connection)
  (jsonrpc-request hemis--conn method params :timeout hemis-request-timeout))

(defun hemis--rust-grammar-available-p ()
  "Return non-nil when the Rust Tree-sitter grammar is available."
  (and (featurep 'treesit)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'rust)))

(defun hemis--treesit-install-dir ()
  "Return the directory where grammars are installed or should be installed."
  (expand-file-name "tree-sitter/" user-emacs-directory))

(defun hemis--ensure-treesit-path ()
  "Ensure the grammar install directory exists and is on `treesit-extra-load-path`."
  (when (featurep 'treesit)
    (let ((dir (hemis--treesit-install-dir)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (unless (member dir treesit-extra-load-path)
        (push dir treesit-extra-load-path))
      dir)))

(defun hemis--find-rust-grammar ()
  "Return the path to an installed rust grammar library, if any."
  (when (featurep 'treesit)
    (let* ((dirs (append treesit-extra-load-path (list (hemis--treesit-install-dir))))
           (candidates (seq-mapcat
                        (lambda (dir)
                          (when (file-directory-p dir)
                            (directory-files dir t "^libtree-sitter-rust\\..*$")))
                        dirs)))
      (car candidates))))

(defun hemis--copy-rust-grammar-alias (src)
  "Copy rust grammar SRC to rust-ts-mode alias names alongside it."
  (when (and src (file-exists-p src))
    (let* ((dir (file-name-directory src))
           (stem (file-name-sans-extension (file-name-nondirectory src)))
           (ext  (concat "." (file-name-extension src)))
           (dest (expand-file-name (concat stem "-ts-mode" ext) dir)))
      (condition-case err
          (copy-file src dest t)
        (error
         (message "Hemis: failed to copy rust grammar alias (%s): %s" dest err))))))

(defun hemis--ensure-rust-alias ()
  "Ensure rust grammar has rust-ts-mode alias file present."
  (let ((src (hemis--find-rust-grammar)))
    (when src
      (hemis--copy-rust-grammar-alias src))))

(defun hemis--ensure-rust-grammar (&optional force)
  "Ensure the Rust Tree-sitter grammar is installed.
When FORCE is non-nil, attempt installation even if `major-mode` is not Rust."
  (when (and hemis-auto-install-treesit-grammars
             (featurep 'treesit)
             (fboundp 'treesit-install-language-grammar)
             (or force (memq major-mode '(rust-mode rust-ts-mode))))
    (unless (hemis--rust-grammar-available-p)
      (let ((dir (hemis--ensure-treesit-path)))
        (when dir
          (add-to-list 'treesit-extra-load-path dir))
        ;; Prefer rust-ts-mode mapping to rust language.
        ;; Ensure source entry exists before install.
        (setq treesit-language-source-alist
              (assq-delete-all 'rust treesit-language-source-alist))
        (push '(rust "https://github.com/tree-sitter/tree-sitter-rust")
              treesit-language-source-alist)
        (setq treesit-language-source-alist
              (assq-delete-all 'rust-ts-mode treesit-language-source-alist))
        (push '(rust-ts-mode "https://github.com/tree-sitter/tree-sitter-rust"
                             :symbol "tree_sitter_rust")
              treesit-language-source-alist)
        (when (boundp 'treesit-major-mode-language-alist)
          (setq treesit-major-mode-language-alist
                (assq-delete-all 'rust-ts-mode treesit-major-mode-language-alist))
          (setq treesit-major-mode-language-alist
                (assq-delete-all 'rust-mode treesit-major-mode-language-alist))
          (push '(rust-ts-mode . rust) treesit-major-mode-language-alist)
          (push '(rust-mode . rust) treesit-major-mode-language-alist))
        (when (boundp 'treesit-language-remap-alist)
          (setq treesit-language-remap-alist
                (assq-delete-all 'rust-ts-mode treesit-language-remap-alist))
          (push '(rust-ts-mode . rust) treesit-language-remap-alist))
        (condition-case err
            (treesit-install-language-grammar 'rust)
          (error
           (message "Hemis: failed to install Rust Tree-sitter grammar: %s" err)))
        (hemis--copy-rust-grammar-alias (hemis--find-rust-grammar))
        ;; Re-check after installation.
        (unless (treesit-language-available-p 'rust)
          (message "Hemis: Rust grammar install did not succeed."))))
    ;; Ensure alias files exist even if grammar was already present.
    (hemis--ensure-rust-alias))
  (hemis--rust-grammar-available-p))

(defun hemis--treesit-available-p ()
  "Return non-nil when Tree-sitter is available for the current mode."
  (when (and (featurep 'treesit)
             (fboundp 'treesit-node-at)
             (fboundp 'treesit-ready-p))
    (when (memq major-mode '(rust-mode rust-ts-mode))
      (hemis--ensure-rust-grammar))
    (treesit-ready-p major-mode)))

(defun hemis--node-path-at-point (&optional max-depth)
  "Return list of node types from innermost to outermost at point.
Limits to MAX-DEPTH parents (default 6) to avoid excessively long chains."
  (when (hemis--treesit-available-p)
    (let* ((node (treesit-node-at (point)))
           (depth (or max-depth 6))
           (path '()))
      (while (and node (> depth 0))
        (push (treesit-node-type node) path)
        (setq node (treesit-node-parent node)
              depth (1- depth)))
      (nreverse path))))


;;; Notes data & overlays

(defun hemis--clear-note-overlays ()
  "Remove all Hemis note overlays from the current buffer."
  (when hemis--overlays
    (mapc #'delete-overlay hemis--overlays)
    (setq hemis--overlays nil)))

(defun hemis--make-note-overlay (note)
  "Create an overlay in the current buffer from NOTE.
NOTE is an alist or plist parsed from JSON, keys like :id, :line, :column, :summary."
  (let* ((id     (or (alist-get 'id note)
                     (plist-get note :id)))
         (line   (or (alist-get 'line note)
                     (plist-get note :line)))
         (col    (or (alist-get 'column note)
                     (plist-get note :column)
                     0))
         (text   (or (alist-get 'summary note)
                     (plist-get note :summary)
                     (alist-get 'text note)
                     (plist-get note :text)
                     "Note"))
         (pos    (save-excursion
                   (goto-char (point-min))
                   (forward-line (max 0 (1- (or line 1))))
                   (move-to-column col)
                   (point)))
         (ov     (make-overlay pos pos (current-buffer) t t))
         (marker (propertize (format " [%s]" text)
                             'face 'hemis-note-marker-face)))
    (overlay-put ov 'hemis-note-id id)
    (overlay-put ov 'after-string marker)
    (push ov hemis--overlays)))

(defun hemis--apply-notes (notes)
  "Render NOTES as overlays in the current buffer.
NOTES is a list of note objects (alist/plist) from the backend."
  (hemis--clear-note-overlays)
  (seq-do #'hemis--make-note-overlay notes))


;;; Backend calls

(defun hemis--project-root ()
  "Return the project root for the current buffer, or nil."
  (when-let* ((proj (ignore-errors (project-current))))
    (condition-case nil
        (project-root proj)
      (error nil))))

(defun hemis--buffer-params ()
  "Return an alist describing the current buffer for the backend."
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (root (or (hemis--project-root) default-directory))
         (git (and (buffer-file-name) (hemis--git-info (buffer-file-name)))))
    `((file . ,file)
      (projectRoot . ,root)
      ,@(when git
          `((commit . ,(alist-get 'commit git))
            (blob . ,(alist-get 'blob git)))))))

(defun hemis-index-file (&optional file)
  "Send the current FILE (or current buffer) to the backend index."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
         (root (or (hemis--project-root) default-directory)))
    (unless file
      (user-error "No file to index"))
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (hemis--request "index/add-file"
                      `((file . ,file)
                        (projectRoot . ,root)
                        (content . ,content))))
    (message "Hemis: indexed %s" file)))

(defun hemis-search-project (query)
  "Search QUERY in indexed files for the current project and show results."
  (interactive "sSearch query: ")
  (let* ((root (or (hemis--project-root) default-directory))
         (results (hemis--request "index/search"
                                  `((query . ,query)
                                    (projectRoot . ,root))))
         (buf (get-buffer-create "*Hemis Search*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert (format "Hemis search for \"%s\" in %s\n\n" query root))
        (dolist (hit results)
          (let* ((file (alist-get 'file hit))
                 (line (alist-get 'line hit))
                 (col  (alist-get 'column hit))
                 (text (alist-get 'text hit)))
            (insert (format "%s:%s:%s %s\n" file line col text))
            (add-text-properties (line-beginning-position 0) (line-end-position)
                                 (list 'hemis-search-hit hit)))))
      (goto-char (point-min))
      (hemis-search-results-mode))
    (display-buffer buf)))

(defun hemis-open-project (root)
  "Open Hemis project at ROOT and remember it for subsequent RPCs."
  (interactive "DProject root: ")
  (setq hemis--project-root (expand-file-name root))
  (hemis--request "hemis/open-project" `((projectRoot . ,hemis--project-root)))
  (message "Hemis: project set to %s" hemis--project-root))

(defun hemis-list-files (&optional root)
  "List files under ROOT (defaults to current project)."
  (interactive)
  (let* ((root (or root (hemis--project-root) default-directory))
         (files (hemis--request "hemis/list-files"
                                `((projectRoot . ,root))))
         (buf (get-buffer-create "*Hemis Files*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert (format "Hemis files in %s\n\n" root))
        (dolist (f files)
          (insert f "\n")))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(defun hemis-view-file (file)
  "Fetch FILE content via backend and display in a temp buffer."
  (interactive "FFile: ")
  (let* ((resp (hemis--request "hemis/get-file"
                               `((file . ,(expand-file-name file)))))
         (content (alist-get 'content resp)))
    (with-current-buffer (get-buffer-create "*Hemis File*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun hemis-refresh-notes ()
  "Fetch and render all notes for the current buffer."
  (interactive)
  (when (buffer-file-name)
    (let* ((params (hemis--buffer-params))
           (notes  (hemis--request "notes/list-for-file" params)))
      (hemis--apply-notes notes)
      (message "Hemis: %d notes loaded." (length notes)))))

(defun hemis-add-note (text &optional tags)
  "Create a new Hemis note at point with TEXT and optional TAGS list."
  (interactive "sNote text: ")
  (let* ((node-path (hemis--node-path-at-point))
         (node-path-json (when (and node-path (> (length node-path) 0))
                           (vconcat node-path)))
         (params (append (hemis--buffer-params)
                         `((line . ,(line-number-at-pos))
                           (column . ,(current-column))
                           (text . ,text)
                           (tags . ,tags)
                           (nodePath . ,node-path-json))))
         (note   (hemis--request "notes/create" params)))
    (hemis--make-note-overlay note)
    (message "Hemis: note created.")
    note))

(defun hemis-get-note (id)
  "Fetch a single Hemis note by ID."
  (hemis--request "notes/get" `((id . ,id))))

(defun hemis-notes-for-node (node-path)
  "Return notes for NODE-PATH in the current file/project."
  (let* ((params (hemis--buffer-params))
         (file (alist-get 'file params))
         (proj (alist-get 'projectRoot params))
         (commit (alist-get 'commit params))
         (blob (alist-get 'blob params)))
    (hemis--request "notes/list-by-node"
                    `((file . ,file)
                      (projectRoot . ,proj)
                      (commit . ,commit)
                      (blob . ,blob)
                      (nodePath . ,(and node-path (vconcat node-path)))))))

(defun hemis-list-notes ()
  "List all Hemis notes for the current file in a separate buffer."
  (interactive)
  (let* ((params (hemis--buffer-params))
         (notes  (hemis--request "notes/list-for-file" params))
         (buf    (get-buffer-create "*Hemis Notes*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (hemis-notes-list-mode)
      (let ((inhibit-read-only t))
        (insert (format "Hemis notes for %s\n\n"
                        (cdr (assoc 'file params))))
        (cl-loop for note in notes
                 for idx from 0 do
                 (let* ((id   (or (alist-get 'id note)
                                  (plist-get note :id)))
                        (file (or (alist-get 'file note)
                                  (plist-get note :file)))
                        (line (or (alist-get 'line note)
                                  (plist-get note :line)))
                        (col  (or (alist-get 'column note)
                                  (plist-get note :column)
                                  0))
                        (txt  (or (alist-get 'text note)
                                  (plist-get note :text)
                                  (alist-get 'summary note)
                                  (plist-get note :summary)
                                  "")))
                   (insert (format "%3d [%s] %s L%d,C%d  %s\n"
                                   idx id (or file "") line col txt))
                   (add-text-properties
                    (line-beginning-position 0) (line-end-position)
                    (list 'hemis-note note)))))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun hemis-notes-list-visit ()
  "Visit the note on the current line in its file."
  (interactive)
  (let* ((note (get-text-property (line-beginning-position)
                                  'hemis-note))
         (file (or (alist-get 'file note)
                   (plist-get note :file)))
         (line (or (alist-get 'line note)
                   (plist-get note :line)))
         (col  (or (alist-get 'column note)
                   (plist-get note :column)
                   0)))
    (unless file
      (user-error "No note on this line."))
    (find-file file)
    (goto-char (point-min))
    (forward-line (max 0 (1- (or line 1))))
    (move-to-column col)
    (recenter)
    (message "Hemis: jumped to note %s"
             (or (alist-get 'id note)
                 (plist-get note :id)))))


;;; Minor modes

(defvar hemis-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h a") #'hemis-add-note)
    (define-key map (kbd "C-c h r") #'hemis-refresh-notes)
    (define-key map (kbd "C-c h l") #'hemis-list-notes)
    (define-key map (kbd "C-c h i") #'hemis-index-file)
    (define-key map (kbd "C-c h s") #'hemis-search-project)
    map)
  "Keymap for `hemis-notes-mode'.")

(define-minor-mode hemis-notes-mode
  "Minor mode for displaying and editing Hemis notes (stickies) in code buffers."
  :lighter " Hemis"
  :keymap hemis-notes-mode-map
  (if hemis-notes-mode
      (progn
        (hemis--ensure-connection)
        (hemis-refresh-notes))
    (hemis--clear-note-overlays)))

(defvar hemis-notes-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hemis-notes-list-visit)
    map)
  "Keymap for `hemis-notes-list-mode'.")

(define-derived-mode hemis-notes-list-mode special-mode "Hemis-Notes"
  "Mode for listing Hemis notes."
  (setq buffer-read-only t))

(defvar hemis-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hemis-search-visit)
    map)
  "Keymap for `hemis-search-results-mode'.")

(defun hemis-search-visit ()
  "Visit the search hit on the current line."
  (interactive)
  (let* ((hit (get-text-property (line-beginning-position) 'hemis-search-hit))
         (file (alist-get 'file hit))
         (line (alist-get 'line hit))
         (col  (alist-get 'column hit)))
    (unless file
      (user-error "No search hit on this line"))
    (find-file file)
    (goto-char (point-min))
    (forward-line (max 0 (1- (or line 1))))
    (move-to-column (or col 0))
    (recenter)))

(define-derived-mode hemis-search-results-mode special-mode "Hemis-Search"
  "Mode for Hemis search results."
  (setq buffer-read-only t))

(provide 'hemis)

;;; hemis.el ends here
