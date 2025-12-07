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
         (root (expand-file-name "../.." here))
         (candidate (expand-file-name "target/debug/hemis" root)))
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
  "Path to the Hemis backend binary."
  :type 'string
  :group 'hemis)

(defcustom hemis-dir (expand-file-name "~/.hemis")
  "Directory for Hemis socket, lock, and log files."
  :type 'directory
  :group 'hemis)

(defcustom hemis-log-buffer "*Hemis Log*"
  "Name of the buffer used to log Hemis backend output."
  :type 'string
  :group 'hemis)

(defcustom hemis-backend-env nil
  "List of environment variables (\"KEY=VAL\") passed to the Hemis backend process."
  :type '(repeat string)
  :group 'hemis)

;; Expected protocol version (bump when backend protocol changes)
(defconst hemis--expected-protocol-version 1
  "Expected protocol version. Backend should match this.")

(defcustom hemis-auto-install-treesit-grammars t
  "When non-nil, attempt to install required Tree-sitter grammars (e.g., Rust) automatically."
  :type 'boolean
  :group 'hemis)

(defcustom hemis-request-timeout 60
  "Timeout (seconds) for JSON-RPC requests to the Hemis backend."
  :type 'integer
  :group 'hemis)

(defcustom hemis-debug t
  "When non-nil, log debug information to *Hemis Debug* buffer.
Set to t for basic logging, or 'verbose for detailed RPC payloads."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Basic" t)
                 (const :tag "Verbose" verbose))
  :group 'hemis)

(defvar hemis--debug-buffer "*Hemis Debug*"
  "Buffer name for debug output.")

(defvar hemis--debug-log-file
  (expand-file-name "emacs-debug.log" (or (getenv "HEMIS_DIR") "~/.hemis"))
  "File path for debug log output.")

(defun hemis--debug (fmt &rest args)
  "Log debug message FMT with ARGS if `hemis-debug' is enabled.
Messages are timestamped and written to `hemis--debug-buffer' and log file."
  (when hemis-debug
    (let* ((msg (apply #'format fmt args))
           (ts (format-time-string "%H:%M:%S.%3N"))
           (line (format "[%s] %s\n" ts msg)))
      ;; Write to buffer
      (with-current-buffer (get-buffer-create hemis--debug-buffer)
        (goto-char (point-max))
        (insert line))
      ;; Append to log file
      (let ((dir (file-name-directory hemis--debug-log-file)))
        (unless (file-exists-p dir)
          (make-directory dir t)))
      (append-to-file line nil hemis--debug-log-file))))

(defface hemis-note-marker-face
  '((t :foreground "SteelBlue"
       :underline nil :overline nil :strike-through nil :box nil
       :extend nil))
  "Face for Hemis sticky note markers."
  :group 'hemis)

(defface hemis-note-list-index-face
  '((t :foreground "gray50" :weight normal))
  "Face for note index number in the notes list."
  :group 'hemis)

(defface hemis-note-list-id-face
  '((t :foreground "DarkOrange" :weight bold))
  "Face for note ID in the notes list."
  :group 'hemis)

(defface hemis-note-list-file-face
  '((t :foreground "CadetBlue" :weight normal))
  "Face for file path in the notes list."
  :group 'hemis)

(defface hemis-note-list-location-face
  '((t :foreground "gray60"))
  "Face for line/column location in the notes list."
  :group 'hemis)

(defface hemis-note-list-text-face
  '((t :foreground "gray80"))
  "Face for note text in the notes list."
  :group 'hemis)

(defvar hemis--process nil
  "Process object for the Hemis backend.")

(defvar hemis--conn nil
  "JSON-RPC connection to the Hemis backend.")

(defvar hemis--project-root-override nil
  "Project root set via `hemis-open-project' (takes precedence over `project-current').")

(defvar-local hemis--overlays nil
  "List of Hemis note overlays in the current buffer.")

;;; Event client for push notifications

(defvar hemis--events-process nil
  "Process object for the events socket connection.")

(defvar hemis--events-buffer ""
  "Buffer for accumulating partial event data.")

(defvar hemis--events-handlers (make-hash-table :test 'equal)
  "Hash table of event type -> handler function.")

(defvar hemis--events-reconnect-timer nil
  "Timer for reconnecting to events socket.")

(defun hemis--events-socket-path ()
  "Return the path to the Hemis events socket."
  (expand-file-name "events.sock" hemis-dir))

(defun hemis--events-filter (proc output)
  "Process filter for events socket. Parse JSON lines from OUTPUT."
  (setq hemis--events-buffer (concat hemis--events-buffer output))
  ;; Parse complete lines
  (while (string-match "\n" hemis--events-buffer)
    (let* ((newline-pos (match-beginning 0))
           (line (substring hemis--events-buffer 0 newline-pos)))
      (setq hemis--events-buffer (substring hemis--events-buffer (1+ newline-pos)))
      (when (> (length line) 0)
        (condition-case err
            (let* ((event (json-parse-string line :object-type 'alist))
                   (event-type (alist-get 'type event)))
              (hemis--debug "Event: %s" event-type)
              (let ((handler (gethash event-type hemis--events-handlers)))
                (when handler
                  (funcall handler event)))
              ;; Also call catch-all handler
              (let ((catch-all (gethash "*" hemis--events-handlers)))
                (when catch-all
                  (funcall catch-all event))))
          (error
           (hemis--debug "Event parse error: %s" (error-message-string err))))))))

(defun hemis--events-sentinel (proc event)
  "Process sentinel for events socket. Reconnect on disconnect."
  (hemis--debug "Events socket: %s" (string-trim event))
  (when (memq (process-status proc) '(closed exit signal))
    (setq hemis--events-process nil)
    (hemis--events-schedule-reconnect)))

(defun hemis--events-schedule-reconnect ()
  "Schedule reconnection to events socket."
  (unless hemis--events-reconnect-timer
    (setq hemis--events-reconnect-timer
          (run-at-time 2 nil #'hemis--events-try-connect))))

(defun hemis--events-try-connect ()
  "Try to connect to the events socket."
  (setq hemis--events-reconnect-timer nil)
  (if (file-exists-p (hemis--events-socket-path))
      (condition-case err
          (let ((proc (make-network-process
                       :name "hemis-events"
                       :family 'local
                       :service (hemis--events-socket-path)
                       :coding 'utf-8
                       :noquery t
                       :filter #'hemis--events-filter
                       :sentinel #'hemis--events-sentinel)))
            (setq hemis--events-process proc)
            (setq hemis--events-buffer "")
            (hemis--debug "Connected to events socket"))
        (error
         (hemis--debug "Events connect error: %s" (error-message-string err))
         (hemis--events-schedule-reconnect)))
    (hemis--events-schedule-reconnect)))

(defun hemis--events-start ()
  "Start the events client."
  (unless (and hemis--events-process (process-live-p hemis--events-process))
    (hemis--events-try-connect)))

(defun hemis--events-stop ()
  "Stop the events client."
  (when hemis--events-reconnect-timer
    (cancel-timer hemis--events-reconnect-timer)
    (setq hemis--events-reconnect-timer nil))
  (when (and hemis--events-process (process-live-p hemis--events-process))
    (delete-process hemis--events-process))
  (setq hemis--events-process nil)
  (setq hemis--events-buffer ""))

(defun hemis--events-on (event-type handler)
  "Register HANDLER for EVENT-TYPE events.
EVENT-TYPE is a string like \"note-created\" or \"*\" for all events."
  (puthash event-type handler hemis--events-handlers))

(defun hemis--kill-backend-processes ()
  "Kill any running Hemis backend processes and clear state."
  (dolist (proc (process-list))
    (when (and (process-live-p proc)
               (string-match-p "^hemis-backend" (process-name proc)))
      (ignore-errors
        (kill-process proc)
        (accept-process-output proc 0.05))))
  (setq hemis--process nil
        hemis--conn nil))

;; NOTE: Git helpers removed - server auto-computes commit/blob/projectRoot from file


;;; Process & JSON-RPC management (Unix socket mode)

(defun hemis--socket-path ()
  "Return the path to the Hemis socket."
  (expand-file-name "hemis.sock" hemis-dir))

(defun hemis--lock-path ()
  "Return the path to the Hemis lock file."
  (expand-file-name "hemis.lock" hemis-dir))

(defun hemis--log-path ()
  "Return the path to the Hemis log file."
  (expand-file-name "hemis.log" hemis-dir))

(defun hemis--socket-exists-p ()
  "Return non-nil if the socket file exists."
  (file-exists-p (hemis--socket-path)))

(defun hemis--read-lock-pid ()
  "Read and return the PID from the lock file, or nil."
  (let ((lock (hemis--lock-path)))
    (when (file-exists-p lock)
      (with-temp-buffer
        (insert-file-contents lock)
        (string-to-number (buffer-substring-no-properties
                           (point-min)
                           (line-end-position)))))))

(defun hemis--process-alive-p (pid)
  "Return non-nil if process with PID is alive."
  (when pid
    (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid)))))

(defun hemis--try-acquire-lock ()
  "Try to acquire the lock file. Return non-nil if successful."
  (let ((lock (hemis--lock-path)))
    (condition-case nil
        (progn
          ;; Ensure hemis-dir exists
          (make-directory hemis-dir t)
          ;; Try to create lock file exclusively
          (with-temp-file lock
            (insert (format "%d\n" (emacs-pid))))
          t)
      (error nil))))

(defun hemis--start-server ()
  "Start the Hemis server process in the background.
The server runs detached with output redirected to `hemis--log-path'.
Use `hemis-shutdown' to stop it (sends shutdown RPC)."
  (let* ((exe (or hemis-backend (error "Set `hemis-backend' to the Rust backend binary")))
         (exe-abs (expand-file-name exe))
         (log (hemis--log-path))
         ;; Build environment variables as shell exports
         (env-exports (mapconcat
                       (lambda (e)
                         (format "export %s;" (shell-quote-argument e)))
                       hemis-backend-env
                       " "))
         (cmd (format "%s %s --serve >> %s 2>&1 &"
                      env-exports
                      (shell-quote-argument exe-abs)
                      (shell-quote-argument log))))
    (make-directory hemis-dir t)
    (message "Hemis: starting server: %s --serve (log: %s)" exe-abs log)
    ;; Run detached via shell - server stops via shutdown RPC
    (call-process-shell-command cmd nil 0)
    (message "Hemis: started backend server")))

(defun hemis--wait-for-socket (timeout-secs)
  "Wait up to TIMEOUT-SECS for the socket to appear. Return non-nil if found."
  (let ((deadline (+ (float-time) timeout-secs)))
    (while (and (not (hemis--socket-exists-p))
                (< (float-time) deadline))
      (sleep-for 0.1))
    (hemis--socket-exists-p)))

(defun hemis--connect-socket ()
  "Connect to the Hemis socket. Return the process or nil on failure."
  (condition-case err
      (let* ((buf (get-buffer-create hemis-log-buffer))
             (proc (make-network-process
                    :name "hemis-backend"
                    :buffer buf
                    :family 'local
                    :service (hemis--socket-path)
                    :coding 'no-conversion
                    :noquery t)))
        proc)
    (error
     (message "Hemis: failed to connect to socket: %s" (error-message-string err))
     nil)))

(defun hemis--start-process ()
  "Connect to the Hemis backend server, starting it if necessary."
  (unless (and hemis--process (process-live-p hemis--process))
    ;; Clean up any existing connection
    (hemis--kill-backend-processes)

    ;; Try to connect to existing socket
    (let ((connected nil))
      (when (hemis--socket-exists-p)
        (let ((proc (hemis--connect-socket)))
          (when proc
            (hemis--setup-connection proc)
            (setq connected t))))

      (unless connected
        ;; Socket doesn't exist or connection failed - need to start server
        ;; Check if stale lock/socket
        (when (hemis--socket-exists-p)
          (let ((pid (hemis--read-lock-pid)))
            (unless (hemis--process-alive-p pid)
              ;; Stale, clean up
              (message "Hemis: cleaning up stale socket")
              (ignore-errors (delete-file (hemis--socket-path)))
              (ignore-errors (delete-file (hemis--lock-path))))))

        ;; Try to start the server
        (if (hemis--try-acquire-lock)
            (progn
              (hemis--start-server)
              (unless (hemis--wait-for-socket 5)
                (ignore-errors (delete-file (hemis--lock-path)))
                (error "Hemis: server failed to create socket"))
              (sleep-for 0.1)
              (let ((proc (hemis--connect-socket)))
                (unless proc
                  (error "Hemis: failed to connect after starting server"))
                (hemis--setup-connection proc)))
          ;; Someone else is starting the server, wait for socket
          (message "Hemis: waiting for server to start...")
          (unless (hemis--wait-for-socket 5)
            (error "Hemis: timeout waiting for server"))
          (sleep-for 0.1)
          (let ((proc (hemis--connect-socket)))
            (unless proc
              (error "Hemis: failed to connect to server"))
            (hemis--setup-connection proc)))))))

(cl-defun hemis--setup-connection (proc)
  "Set up the jsonrpc connection using PROC."
  (hemis--debug "Setting up connection, proc=%s" (process-name proc))
  (setq hemis--process proc)
  (setq hemis--conn
        (jsonrpc-process-connection
         :process proc
         :on-shutdown (lambda (&rest _)
                        (hemis--debug "Connection shutdown callback")
                        (message "Hemis backend shut down")
                        (setq hemis--process nil
                              hemis--conn nil))))
  ;; Check version
  (condition-case err
      (let ((info (hemis--request "hemis/version")))
        (when info
          (let ((proto (alist-get 'protocolVersion info)))
            (hemis--debug "Connected: protocol=%s hash=%s"
                         (or proto "?") (or (alist-get 'gitHash info) "?"))
            (cond
             ((and proto (> proto hemis--expected-protocol-version))
              (message "Hemis: backend is newer. Consider updating the plugin."))
             ((and proto (< proto hemis--expected-protocol-version))
              (display-warning 'hemis
                               "Backend is outdated. Run: pkill -f 'hemis --serve'"
                               :warning)))
            (message "Hemis: connected to backend v%s (%s)"
                     (or proto "?")
                     (or (alist-get 'gitHash info) "?")))))
    (error
     (hemis--debug "Version check failed: %s" (error-message-string err))
     (message "Hemis: version check failed: %s" (error-message-string err)))))

(defun hemis--ensure-connection ()
  "Ensure that the Hemis backend connection is live."
  (unless (and hemis--conn
               (jsonrpc-running-p hemis--conn))
    (hemis--start-process)))

(defun hemis-shutdown ()
  "Shutdown the Hemis backend server.
Sends shutdown RPC to stop the detached server process."
  (interactive)
  (hemis--events-stop)
  (when (and hemis--conn (jsonrpc-running-p hemis--conn))
    (ignore-errors (jsonrpc-request hemis--conn "shutdown" nil :timeout 1)))
  (when (and hemis--process (process-live-p hemis--process))
    (delete-process hemis--process))
  (setq hemis--process nil
        hemis--conn nil)
  (message "Hemis backend stopped."))

(defun hemis-disconnect ()
  "Disconnect from the Hemis backend (does not shutdown server)."
  (interactive)
  (when (and hemis--process (process-live-p hemis--process))
    (delete-process hemis--process))
  (setq hemis--process nil
        hemis--conn nil)
  (message "Hemis: disconnected."))

;; Disconnect when Emacs exits (server keeps running)
(add-hook 'kill-emacs-hook #'hemis-disconnect)

(defun hemis-reload ()
  "Reload the Hemis Emacs package after code changes.
This disconnects from the backend, unloads the feature, and reloads it."
  (interactive)
  (hemis-disconnect)
  (unload-feature 'hemis t)
  (require 'hemis)
  (message "Hemis reloaded."))

(defun hemis--request (method &optional params)
  "Synchronously send JSON-RPC METHOD with PARAMS and return result."
  (hemis--ensure-connection)
  (hemis--debug "RPC>> %s" method)
  (when (eq hemis-debug 'verbose)
    (hemis--debug "  params: %S" params))
  (let ((result (jsonrpc-request hemis--conn method params :timeout hemis-request-timeout)))
    (hemis--debug "RPC<< %s ok" method)
    (when (eq hemis-debug 'verbose)
      (hemis--debug "  result: %S" result))
    result))

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
             (fboundp 'treesit-ready-p)
             (memq major-mode '(rust-mode rust-ts-mode)))
    (hemis--ensure-rust-grammar)
    (treesit-ready-p major-mode)))

;; NOTE: hemis--node-path-at-point removed - server computes nodePath from content

(defun hemis--anchor-position (line col)
  "Return buffer position at LINE and COL.
Server computes actual anchor position from content when creating notes."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- (or line 1))))
    (move-to-column (or col 0))
    (point)))

(defun hemis--note-anchor ()
  "Return plist with current cursor line and column.
Server computes anchor position, nodePath, and nodeTextHash from content."
  (list :line (line-number-at-pos)
        :column (current-column)))

(defun hemis--line-indentation (pos)
  "Return indentation (whitespace) of the line at POS."
  (save-excursion
    (goto-char pos)
    (buffer-substring (line-beginning-position)
                      (progn (back-to-indentation) (point)))))

(defun hemis--format-note-texts (texts pos &optional formatted-lines)
  "Format TEXTS as comment lines above POS using server-provided FORMATTED-LINES.
Server provides formattedLines when content is sent; falls back to raw text."
  (let ((indent (hemis--line-indentation pos)))
    (if formatted-lines
        (concat (mapconcat #'identity formatted-lines "\n") "\n" indent)
      ;; Fallback to raw text if formattedLines missing
      (concat "// " (car texts) "\n" indent))))


;;; Notes list mode (defined early so hemis-list-notes can use it)

(defun hemis-notes-list-next ()
  "Move to the next note in the notes list buffer."
  (interactive)
  (let ((start (point))
        (current-note (get-text-property (point) 'hemis-note)))
    ;; Skip past current note's region
    (while (and (not (eobp))
                (eq (get-text-property (point) 'hemis-note) current-note))
      (forward-line 1))
    ;; Find next note
    (while (and (not (eobp))
                (not (get-text-property (point) 'hemis-note)))
      (forward-line 1))
    (if (get-text-property (point) 'hemis-note)
        (beginning-of-line)
      (goto-char start)
      (user-error "No more notes"))))

(defun hemis-notes-list-prev ()
  "Move to the previous note in the notes list buffer."
  (interactive)
  (let ((start (point))
        (current-note (get-text-property (point) 'hemis-note)))
    ;; Move up one line first
    (forward-line -1)
    ;; Skip past current note's region going backwards
    (while (and (not (bobp))
                (eq (get-text-property (point) 'hemis-note) current-note))
      (forward-line -1))
    ;; Now we might be in a previous note or in a gap - find the note start
    (let ((note (get-text-property (point) 'hemis-note)))
      (if note
          ;; Go to the start of this note block
          (while (and (not (bobp))
                      (eq (get-text-property (1- (line-beginning-position)) 'hemis-note) note))
            (forward-line -1))
        ;; No note here, go back to start
        (goto-char start)
        (user-error "No previous notes")))))

(defvar hemis-notes-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'hemis-notes-list-visit)
    (define-key map (kbd "v")   #'hemis-view-note)
    (define-key map (kbd "b")   #'hemis-show-backlinks)
    (define-key map (kbd "n")   #'hemis-notes-list-next)
    (define-key map (kbd "p")   #'hemis-notes-list-prev)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `hemis-notes-list-mode'.")

(define-derived-mode hemis-notes-list-mode special-mode "Hemis-Notes"
  "Mode for listing Hemis notes.
\\{hemis-notes-list-mode-map}"
  :keymap hemis-notes-list-mode-map
  (setq buffer-read-only t)
  ;; Set up evil-mode bindings if available
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'hemis-notes-list-mode 'emacs)))

;; Evil-mode integration: define keys in normal and motion states
(with-eval-after-load 'evil
  (evil-define-key* '(normal motion emacs) hemis-notes-list-mode-map
    (kbd "n") #'hemis-notes-list-next
    (kbd "p") #'hemis-notes-list-prev
    (kbd "RET") #'hemis-notes-list-visit
    (kbd "v") #'hemis-view-note
    (kbd "b") #'hemis-show-backlinks
    (kbd "q") #'quit-window
    (kbd "j") #'hemis-notes-list-next
    (kbd "k") #'hemis-notes-list-prev))

;;; Notes data & overlays

(defvar hemis--note-input-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-c") #'exit-minibuffer)
    (define-key map (kbd "C-c C-k") #'abort-recursive-edit)
    map)
  "Keymap for multi-line Hemis note entry (RET inserts newline, C-c C-c to save).")

(defvar hemis--note-history nil
  "Input history for Hemis note text.")

(defun hemis--read-note-text ()
  "Read multi-line note text from the minibuffer.
RET inserts a newline; use C-c C-c to finish or C-c C-k to cancel."
  (condition-case nil
      (read-from-minibuffer
       "Hemis note (RET=newline, C-c C-c to save, C-c C-k to cancel):\n"
       nil hemis--note-input-map nil 'hemis--note-history)
    (quit nil)))

(defun hemis--note-get (note key)
  "Get KEY from NOTE, handling symbol, keyword, and string keys."
  (or (plist-get note (intern (concat ":" (symbol-name key))))
      (alist-get key note)
      (alist-get (symbol-name key) note nil nil #'equal)))

(defun hemis--note-text (note)
  "Extract note text or summary from NOTE."
  (or (hemis--note-get note 'text)
      (hemis--note-get note 'summary)))

(defun hemis--clear-note-overlays ()
  "Remove all Hemis note overlays from the current buffer."
  (when hemis--overlays
    (mapc #'delete-overlay hemis--overlays)
    (setq hemis--overlays nil)))

(defun hemis--make-note-overlay (note)
  "Create an overlay in the current buffer from NOTE.
NOTE is an alist or plist parsed from JSON, keys like :id, :line, :column, :summary.
When server provides displayLine, use that instead of stored line.
When server provides formattedLines, use that for display instead of local formatting."
  (let* ((id     (hemis--note-get note 'id))
         ;; Prefer server-computed displayLine when available
         (line   (or (hemis--note-get note 'displayLine)
                     (hemis--note-get note 'line)))
         (col    (or (hemis--note-get note 'column) 0))
         (text   (or (hemis--note-text note) "Note"))
         ;; Server-provided formatted lines (as vector from JSON)
         (formatted-raw (hemis--note-get note 'formattedLines))
         (formatted (when formatted-raw
                      (if (vectorp formatted-raw)
                          (append formatted-raw nil)
                        formatted-raw)))
         (pos (hemis--anchor-position line col))
         (line-bol (save-excursion (goto-char pos) (line-beginning-position)))
         ;; Drop stale overlays from other buffers or dead overlays.
         (valid (seq-filter (lambda (ov)
                              (and (overlay-buffer ov)
                                   (eq (overlay-buffer ov) (current-buffer))
                                   (overlay-start ov)))
                            hemis--overlays))
         (marker-ov (seq-find (lambda (ov)
                                (and (= (overlay-start ov) pos)
                                     (overlay-get ov 'hemis-note-marker)))
                              valid))
         (new-list valid))
    (unless marker-ov
      (setq marker-ov (make-overlay line-bol line-bol (current-buffer) t t))
      (overlay-put marker-ov 'hemis-note-marker t)
      (overlay-put marker-ov 'hemis-note-count 0)
      (overlay-put marker-ov 'hemis-note-ids nil)
      (overlay-put marker-ov 'hemis-note-texts nil)
      (overlay-put marker-ov 'hemis-note-formatted nil)
      (overlay-put marker-ov 'priority 9999)
      (overlay-put marker-ov 'evaporate nil)
      (push marker-ov new-list))
    (let* ((count (1+ (or (overlay-get marker-ov 'hemis-note-count) 0)))
           (ids (cons id (overlay-get marker-ov 'hemis-note-ids)))
           (texts (append (overlay-get marker-ov 'hemis-note-texts)
                          (list text)))
           ;; Collect all formatted lines from all notes at this position
           (all-formatted (append (overlay-get marker-ov 'hemis-note-formatted)
                                  formatted))
           ;; Use server-formatted lines if available, otherwise format locally
           (display (hemis--format-note-texts texts line-bol all-formatted)))
      (overlay-put marker-ov 'hemis-note-count count)
      (overlay-put marker-ov 'hemis-note-ids ids)
      (overlay-put marker-ov 'hemis-note-texts texts)
      (overlay-put marker-ov 'hemis-note-formatted all-formatted)
      (overlay-put marker-ov 'before-string
                   (propertize display
                               'face '(:foreground "SteelBlue"
                                       :underline nil :overline nil
                                       :strike-through nil :box nil))))
    ;; Per-note overlay (no marker) for downstream consumers.
    (let ((note-ov (make-overlay line-bol line-bol (current-buffer) t t)))
      (overlay-put note-ov 'hemis-note-id id)
      (overlay-put note-ov 'priority 9999)
      (overlay-put note-ov 'evaporate nil)
      (push note-ov new-list))
    (setq hemis--overlays new-list)))

(defun hemis--apply-notes (notes)
  "Render NOTES as overlays in the current buffer.
NOTES is a list of note objects (alist/plist) from the backend."
  (hemis--debug "apply-notes: clearing %d old overlays" (length hemis--overlays))
  (hemis--clear-note-overlays)
  (hemis--debug "apply-notes: creating overlays for %d notes" (length notes))
  (seq-do #'hemis--make-note-overlay notes)
  (hemis--debug "apply-notes: created %d overlays" (length hemis--overlays))
  ;; Fallback: ensure at least one marker if notes exist but overlays evaporated.
  (when (and notes (null hemis--overlays))
    (let ((pos (point-min)))
      (let ((marker-ov (make-overlay pos pos (current-buffer) t t)))
        (overlay-put marker-ov 'hemis-note-marker t)
        (overlay-put marker-ov 'hemis-note-count (length notes))
        (overlay-put marker-ov 'hemis-note-texts (mapcar #'hemis--note-text notes))
        (overlay-put marker-ov 'before-string
                     (propertize (hemis--format-note-texts (mapcar #'hemis--note-text notes) pos)
                                 'face '(:foreground "SteelBlue"
                                         :underline nil :overline nil
                                         :strike-through nil :box nil)))
        (overlay-put marker-ov 'priority 9999)
        (overlay-put marker-ov 'evaporate t)
        (push marker-ov hemis--overlays)))))


;;; Backend calls

(defun hemis--project-root ()
  "Return the project root for the current buffer, or nil."
  (or hemis--project-root-override
      (when-let* ((proj (ignore-errors (project-current))))
        (condition-case nil
            (project-root proj)
          (error nil)))))

(defun hemis--buffer-content ()
  "Return the current buffer content as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun hemis--buffer-params (&optional include-content)
  "Return an alist describing the current buffer for the backend.
When INCLUDE-CONTENT is non-nil, include buffer content for server-side
position tracking.  Server auto-computes projectRoot, commit, and blob
from the file path when not provided."
  (let ((file (or (buffer-file-name) (buffer-name))))
    `((file . ,file)
      ,@(when include-content
          `((content . ,(hemis--buffer-content)))))))

(defun hemis-index-project (&optional root include-ai)
  "Index all files under ROOT (defaults to project root) and show progress.
With prefix arg or INCLUDE-AI non-nil, also run AI analysis."
  (interactive (list nil current-prefix-arg))
  (let ((root (or root (hemis--project-root) default-directory)))
    (message "Hemis: indexing project %s%s..."
             root (if include-ai " with AI analysis" ""))
    (let* ((params `((projectRoot . ,root)))
           (_ (when include-ai
                (setq params (append params '((includeAI . t))))))
           (resp (hemis--request "hemis/index-project" params))
           (indexed (alist-get 'indexed resp))
           (status-msg (alist-get 'statusMessage resp))
           (ai-info (alist-get 'ai resp)))
      (if status-msg
          (message "Hemis: %s in %s" status-msg root)
        (if ai-info
            (let ((analyzed (alist-get 'analyzed ai-info))
                  (provider (alist-get 'provider ai-info))
                  (error-msg (alist-get 'error ai-info)))
              (if analyzed
                  (message "Hemis: indexed %s files, analyzed with %s in %s"
                           (or indexed "?") provider root)
                (message "Hemis: indexed %s files (AI analysis failed: %s)"
                         (or indexed "?") error-msg)))
          (message "Hemis: indexed %s files in %s"
                   (or indexed "?") root))))))

(defun hemis-index-project-ai (&optional root)
  "Index all files under ROOT and run AI analysis."
  (interactive)
  (hemis-index-project root t))

(defun hemis-project-meta ()
  "Display project metadata (indexing and AI analysis status)."
  (interactive)
  (let* ((root (or (hemis--project-root) default-directory))
         (resp (hemis--request "hemis/project-meta"
                               `((projectRoot . ,root)))))
    (with-current-buffer (get-buffer-create "*Hemis Project*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Project: %s\n\n" root))
      (insert (format "Indexed: %s\n"
                      (if (alist-get 'indexed resp) "Yes" "No")))
      (when-let ((ts (or (alist-get 'formattedIndexedAt resp)
                         (when (alist-get 'indexedAt resp)
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                               (alist-get 'indexedAt resp))))))
        (insert (format "  Last indexed: %s\n" ts)))
      (insert "\n")
      (insert (format "AI Analysis: %s\n"
                      (or (alist-get 'analysisStatusDisplay resp)
                          (cond
                           ((alist-get 'analyzed resp)
                            (if (alist-get 'analysisStale resp)
                                "Stale (commit changed)"
                              "Up to date"))
                           ((alist-get 'hasAnalysisFile resp)
                            "Has file but not tracked")
                           (t "Not analyzed")))))
      (when (alist-get 'analysisProvider resp)
        (insert (format "  Provider: %s\n" (alist-get 'analysisProvider resp))))
      (insert "\n")
      (insert (format "AI Available: %s\n"
                      (if (alist-get 'aiAvailable resp) "Yes" "No")))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun hemis-search-project (query)
  "Search QUERY in indexed files/notes for the current project and show results."
  (interactive "sSearch query: ")
  (let* ((root (or (hemis--project-root) default-directory))
         (results (let ((r (hemis--request "hemis/search"
                                           `((query . ,query)
                                             (projectRoot . ,root)
                                             (includeNotes . t)))))
                    (if (vectorp r) (append r nil) r)))
         (buf (get-buffer-create "*Hemis Search*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert (format "Hemis search for \"%s\" in %s\n\n" query root))
        (insert (format "%-6s %-6s %-40s %s\n" "Kind" "Score" "Location" "Text"))
        (insert (make-string 80 ?-))
        (insert "\n")
        (dolist (hit results)
          (let* ((file (hemis--note-get hit 'file))
                 (line (hemis--note-get hit 'line))
                 (col  (hemis--note-get hit 'column))
                 (text (hemis--note-get hit 'text))
                 (score (hemis--note-get hit 'score))
                 (kind (or (hemis--note-get hit 'kind) "file")))
            (insert (format "%-6s %-6.2f %-40s %s\n"
                            kind (or score 0.0)
                            (format "%s:%s:%s" file line col)
                            (or text "")))
            (add-text-properties (line-beginning-position 0) (line-end-position)
                                 (list 'hemis-search-hit hit)))))
      (goto-char (point-min))
      (hemis-search-results-mode))
    (display-buffer buf)))

(defvar hemis--link-search-buffer "*Hemis Link Search*"
  "Buffer name for note link search results.")

(defun hemis--render-link-search (results)
  "Render note search RESULTS into the link search buffer."
  (let ((buf (get-buffer-create hemis--link-search-buffer))
        (notes (if (vectorp results) (append results nil) results)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert "Hemis note link search results\n\n")
        (dolist (note notes)
          (let ((id (hemis--note-get note 'id))
                (summary (or (hemis--note-get note 'summary)
                             (hemis--note-get note 'text)))
                (file (hemis--note-get note 'file)))
            (insert (format "%s\t%s\t%s\n" id summary file)))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(defun hemis-insert-note-link (&optional query)
  "Search existing notes and insert a link of the form [[DESC][ID]]."
  (interactive)
  (let* ((query (or query (read-string "Link search query: ")))
         (root (or (hemis--project-root) default-directory))
         (results (let ((r (hemis--request "notes/search"
                                           `((query . ,query)
                                             (projectRoot . ,root)))))
                    (if (vectorp r) (append r nil) r))))
    (hemis--render-link-search results)
    (unless results
      (user-error "No notes found"))
    (let* ((choices (mapcar (lambda (note)
                              (cons (format "%s (%s)"
                                            (or (hemis--note-get note 'summary)
                                                (hemis--note-get note 'text)
                                                "")
                                            (hemis--note-get note 'id))
                                    note))
                            results))
           (chosen (cdr (assoc (completing-read "Select note: " choices nil t)
                               choices)))
           (default-desc (or (hemis--note-get chosen 'summary)
                             (hemis--note-get chosen 'text)
                             ""))
           (desc (read-string "Description: " default-desc))
           (id (hemis--note-get chosen 'id)))
      (insert (format "[[%s][%s]]" desc id)))))

(defun hemis--maybe-trigger-link ()
  "Trigger note link search when user types [[ in notes mode."
  (when (and (eq last-command-event ?\[)
             (>= (point) 2)
             (string= "[["
                      (buffer-substring-no-properties
                       (max (point-min) (- (point) 2))
                       (point))))
    (delete-char -2)
    (hemis-insert-note-link)))

(defun hemis-open-project (root)
  "Open Hemis project at ROOT and remember it for subsequent RPCs."
  (interactive "DProject root: ")
  (setq hemis--project-root-override (expand-file-name root))
  (hemis--request "hemis/open-project"
                  `((projectRoot . ,hemis--project-root-override)))
  (message "Hemis: project set to %s" hemis--project-root-override))

(defun hemis-reattach-note ()
  "Reattach the stale note at point to the current cursor position.
Use this when a note becomes stale due to code changes."
  (interactive)
  (let* ((note (or (get-text-property (point) 'hemis-note)
                   (hemis--note-at-overlay (point))))
         (id (and note (hemis--note-get note 'id))))
    (unless id
      (user-error "No note at point"))
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (column (current-column))
           (content (buffer-substring-no-properties (point-min) (point-max)))
           (params `((id . ,id)
                     (file . ,file)
                     (line . ,line)
                     (column . ,column)
                     (content . ,content))))
      (hemis--request "notes/reattach" params)
      (message "Hemis: note reattached")
      (hemis-refresh-notes))))

(defun hemis-explain-region (beg end &optional use-ai)
  "Request an explanation for the region from BEG to END.
With prefix arg or USE-AI non-nil, use AI to explain."
  (interactive "r\nP")
  (unless (and (buffer-file-name) beg end)
    (user-error "No region or file to explain"))
  (let* ((file (buffer-file-name))
         (start-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (project-root (hemis--project-root))
         (params `((file . ,file)
                   (startLine . ,start-line)
                   (endLine . ,end-line)
                   (content . ,content)))
         (_ (when project-root
              (setq params (append params `((projectRoot . ,project-root))))))
         (_ (when use-ai
              (setq params (append params '((useAI . t))))))
         (resp (hemis--request "hemis/explain-region" params))
         (snippet (alist-get 'content resp))
         (explanation (alist-get 'explanation resp))
         (ai-info (alist-get 'ai resp)))
    (with-current-buffer (get-buffer-create "*Hemis Explain*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Explanation for %s:%d-%d\n" file start-line end-line))
      (when ai-info
        (let ((status-display (alist-get 'statusDisplay ai-info))
              (provider (alist-get 'provider ai-info))
              (error-msg (alist-get 'error ai-info))
              (had-context (alist-get 'hadContext ai-info)))
          (cond
           (status-display
            (insert (format "%s\n" status-display)))
           (provider
            (insert (format "[AI: %s%s]\n"
                            provider
                            (if had-context " + project context" ""))))
           (error-msg
            (insert (format "[AI error: %s]\n" error-msg))))))
      (insert "\n")
      (if explanation
          (insert explanation)
        (insert snippet))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun hemis-explain-region-ai (beg end)
  "Request an AI-powered explanation for the region from BEG to END."
  (interactive "r")
  (hemis-explain-region beg end t))

(defun hemis-refresh-notes ()
  "Fetch and render all notes for the current buffer.
Sends buffer content so server can compute displayLine positions."
  (interactive)
  (when (buffer-file-name)
    (hemis--debug "refresh-notes: file=%s buf=%s" (buffer-file-name) (buffer-name))
    (let* ((params (append (hemis--buffer-params t) '((includeStale . t))))
           (notes  (let ((r (hemis--request "notes/list-for-file" params)))
                     (if (vectorp r) (append r nil) r))))
      (hemis--debug "refresh-notes: got %d notes" (length notes))
      (hemis--apply-notes notes)
      (when (and notes (null hemis--overlays))
        ;; Fallback if overlays evaporated; place a marker at point-min.
        (let ((pos (point-min)))
          (let ((marker-ov (make-overlay pos pos (current-buffer) t t)))
            (overlay-put marker-ov 'hemis-note-marker t)
            (overlay-put marker-ov 'hemis-note-count (length notes))
            (overlay-put marker-ov 'before-string
                         (propertize (format "n%d" (length notes))
                                     'face '(:foreground "SteelBlue"
                                             :underline nil :overline nil
                                             :strike-through nil :box nil)))
            (overlay-put marker-ov 'priority 9999)
            (overlay-put marker-ov 'evaporate nil)
            (push marker-ov hemis--overlays))))
      (unless hemis--overlays
        (let ((ov (make-overlay (point-min) (point-min))))
          (overlay-put ov 'priority 9999)
          (push ov hemis--overlays)))
      (message "Hemis: %d notes loaded." (length notes)))))

(defun hemis-add-note (text &optional tags)
  "Create a new Hemis note at point with TEXT and optional TAGS list.
Sends buffer content so server can compute nodeTextHash."
  (interactive
   (let ((note-text (hemis--read-note-text)))
     (list (or note-text (user-error "Note entry canceled")))))
  (let* ((anchor (hemis--note-anchor))
         (params (append (hemis--buffer-params t) ; include content
                         `((line . ,(plist-get anchor :line))
                           (column . ,(plist-get anchor :column))
                           (text . ,text)
                           (tags . ,tags)))))
    (hemis--debug "add-note: line=%d col=%d file=%s"
                 (plist-get anchor :line)
                 (plist-get anchor :column)
                 (buffer-file-name))
    (let ((note (hemis--request "notes/create" params)))
      (hemis--debug "add-note: created id=%s" (hemis--note-get note 'id))
      (hemis--make-note-overlay note)
      (message "Hemis: note created.")
      note)))

(defun hemis-get-note (id)
  "Fetch a single Hemis note by ID."
  (hemis--request "notes/get" `((id . ,id))))

(defun hemis-update-note (id text)
  "Update note ID with new TEXT."
  (hemis--request "notes/update" `((id . ,id) (text . ,text))))

(defun hemis-delete-note (id)
  "Delete note with ID."
  (hemis--request "notes/delete" `((id . ,id))))

(defun hemis-status ()
  "Display Hemis backend status: note/file/embedding counts."
  (interactive)
  (let* ((resp (hemis--request "hemis/status" nil))
         (counts (or (alist-get 'counts resp) resp))
         (notes (or (alist-get 'notes counts) 0))
         (files (or (alist-get 'files counts) 0))
         (embeddings (or (alist-get 'embeddings counts) 0)))
    (message "Hemis: %d notes, %d files, %d embeddings" notes files embeddings)))

(defun hemis-edit-note-at-point ()
  "Edit the note at point (overlay or notes list)."
  (interactive)
  (let* ((note (or (get-text-property (point) 'hemis-note)
                   (hemis--note-at-overlay (point))))
         (id (and note (hemis--note-get note 'id)))
         (text (and note (hemis--note-text note))))
    (unless id
      (user-error "No note at point"))
    (let ((new-text (hemis--read-note-text text)))
      (when (and new-text (not (string= new-text text)))
        (hemis-update-note id new-text)
        (message "Note updated")
        (hemis-refresh-notes)))))

(defvar-local hemis--edit-buffer-note-id nil
  "The note ID being edited in this buffer.")

(defun hemis-edit-note-buffer ()
  "Edit the note at point in a dedicated buffer.
Opens a new buffer with the note content for editing longer notes.
Use C-c C-c to save, C-c C-k to cancel."
  (interactive)
  (let* ((note (or (get-text-property (point) 'hemis-note)
                   (hemis--note-at-overlay (point))))
         (id (and note (hemis--note-get note 'id)))
         (text (and note (hemis--note-text note))))
    (unless id
      (user-error "No note at point"))
    (let* ((short-id (or (hemis--note-get note 'shortId) (substring id 0 8)))
           (buf (get-buffer-create (format "*Hemis Edit: %s*" short-id))))
      (pop-to-buffer buf)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (markdown-mode)
      (setq-local hemis--edit-buffer-note-id id)
      (local-set-key (kbd "C-c C-c") #'hemis--edit-buffer-save)
      (local-set-key (kbd "C-c C-k") #'hemis--edit-buffer-cancel)
      (set-buffer-modified-p nil)
      (message "Edit note. C-c C-c to save, C-c C-k to cancel."))))

(defun hemis--edit-buffer-save ()
  "Save the note being edited in the current buffer."
  (interactive)
  (let ((id hemis--edit-buffer-note-id)
        (text (buffer-string))) ; Backend trims
    (unless id
      (user-error "No note ID in this buffer"))
    (when (string-empty-p (string-trim text))
      (user-error "Note text cannot be empty"))
    (hemis-update-note id text)
    (set-buffer-modified-p nil)
    (message "Note saved")
    (kill-buffer-and-window)
    (hemis-refresh-notes)))

(defun hemis--edit-buffer-cancel ()
  "Cancel editing and close the buffer."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard changes? "))
    (set-buffer-modified-p nil)
    (kill-buffer-and-window)))

(defun hemis-delete-note-at-point ()
  "Delete the note at point (overlay or notes list)."
  (interactive)
  (let* ((note (or (get-text-property (point) 'hemis-note)
                   (hemis--note-at-overlay (point))))
         (id (and note (hemis--note-get note 'id))))
    (unless id
      (user-error "No note at point"))
    (when (yes-or-no-p "Delete this note? ")
      (hemis-delete-note id)
      (message "Note deleted")
      (hemis-refresh-notes))))

(defun hemis--note-at-overlay (pos)
  "Return the hemis note at POS from overlays, if any."
  (let ((result nil))
    (dolist (ov hemis--overlays)
      (when (and (overlay-get ov 'hemis-note-marker)
                 (<= (overlay-start ov) pos)
                 (< pos (overlay-end ov)))
        (let ((notes (overlay-get ov 'hemis-notes)))
          (when notes
            (setq result (car notes))))))
    result))

(defun hemis-notes-for-node (node-path)
  "Return notes for NODE-PATH in the current file/project."
  (let* ((params (hemis--buffer-params))
         (file (alist-get 'file params))
         (proj (alist-get 'projectRoot params))
         (commit (alist-get 'commit params))
         (blob (alist-get 'blob params))
         (result (hemis--request "notes/list-by-node"
                                 `((file . ,file)
                                   (projectRoot . ,proj)
                                   (commit . ,commit)
                                   (blob . ,blob)
                                   (includeStale . t)
                                   (nodePath . ,(and node-path (vconcat node-path)))))))
    (if (vectorp result) (append result nil) result)))

(defun hemis-list-notes ()
  "List all Hemis notes for the current file in a separate buffer."
  (interactive)
  (let* ((params (hemis--buffer-params))
         (notes  (let ((result (hemis--request "notes/list-for-file"
                                               (append params '((includeStale . t))))))
                   (if (vectorp result) (append result nil) result)))
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
                 (let* ((id   (hemis--note-get note 'id))
                        (file (hemis--note-get note 'file))
                        (line (or (hemis--note-get note 'line) 0))
                        (col  (or (hemis--note-get note 'column) 0))
                        (txt  (or (hemis--note-text note) ""))
                        (start (point))
                        (short-id (or (hemis--note-get note 'shortId)
                                      (if (> (length id) 8) (substring id 0 8) id)))
                        (short-file (file-name-nondirectory (or file ""))))
                   ;; Header line with faces
                   (insert (propertize (format "%3d " idx)
                                       'face 'hemis-note-list-index-face))
                   (insert (propertize (format "[%s] " short-id)
                                       'face 'hemis-note-list-id-face))
                   (insert (propertize short-file
                                       'face 'hemis-note-list-file-face))
                   (insert (propertize (format " L%d,C%d" line col)
                                       'face 'hemis-note-list-location-face))
                   (insert "\n")
                   ;; Note text indented with face
                   (dolist (text-line (split-string txt "\n"))
                     (insert (propertize (format "    %s\n" text-line)
                                         'face 'hemis-note-list-text-face)))
                   (insert "\n")
                   ;; Mark the whole note block with the property
                   (add-text-properties start (point)
                                        (list 'hemis-note note)))))
      (goto-char (point-min))
      ;; Move to first note if any
      (when notes
        (hemis-notes-list-next)))
    (pop-to-buffer buf)))

(defun hemis-view-note (&optional note)
  "View NOTE text in a Markdown buffer.
If NOTE is nil, use the note at point in *Hemis Notes*, or prompt for an id."
  (interactive)
  (let* ((note (or note
                   (get-text-property (line-beginning-position) 'hemis-note)
                   (let ((id (read-string "Note id: ")))
                     (hemis-get-note id))))
         (text (or (hemis--note-text note) "")))
    (with-current-buffer (get-buffer-create "*Hemis Note*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (if (fboundp 'markdown-mode)
          (markdown-mode)
        (text-mode))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun hemis-notes-list-visit ()
  "Visit the note on the current line in its file.
Opens in another window if available, keeping the notes list visible."
  (interactive)
  (let* ((note (get-text-property (line-beginning-position)
                                  'hemis-note))
         (file (hemis--note-get note 'file))
         (line (or (hemis--note-get note 'line) 1))
         (col  (or (hemis--note-get note 'column) 0)))
    (unless file
      (user-error "No note on this line."))
    ;; Open in other window if multiple windows exist
    (if (> (count-windows) 1)
        (find-file-other-window file)
      (find-file file))
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (move-to-column col)
    (recenter)
    (message "Hemis: jumped to note %s"
             (hemis--note-get note 'id))))

(defun hemis-show-backlinks (&optional note-or-id)
  "Show all notes that link TO the given NOTE-OR-ID.
If called interactively in *Hemis Notes*, use the note at point.
Otherwise, prompt for a note ID."
  (interactive)
  (let* ((note (cond
                ((and (null note-or-id)
                      (get-text-property (line-beginning-position) 'hemis-note))
                 (get-text-property (line-beginning-position) 'hemis-note))
                ((stringp note-or-id) nil)
                (t note-or-id)))
         (id (cond
              ((stringp note-or-id) note-or-id)
              (note (hemis--note-get note 'id))
              (t (read-string "Note id: "))))
         (backlinks (let ((result (hemis--request "notes/backlinks" `((id . ,id)))))
                      (if (vectorp result) (append result nil) result)))
         (buf (get-buffer-create "*Hemis Backlinks*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (hemis-notes-list-mode)
      (let ((inhibit-read-only t))
        (insert (format "Backlinks to note %s\n" id))
        (insert (format "(%d notes link to this note)\n\n"
                        (length backlinks)))
        (if (null backlinks)
            (insert "No backlinks found.\n")
          (cl-loop for note in backlinks
                   for idx from 0 do
                   (let* ((nid   (hemis--note-get note 'id))
                          (file (hemis--note-get note 'file))
                          (line (or (hemis--note-get note 'line) 0))
                          (col  (or (hemis--note-get note 'column) 0))
                          (txt  (or (hemis--note-text note) ""))
                          (start (point))
                          (short-id (or (hemis--note-get note 'shortId)
                                        (if (> (length nid) 8) (substring nid 0 8) nid)))
                          (short-file (file-name-nondirectory (or file ""))))
                     (insert (propertize (format "%3d " idx)
                                         'face 'hemis-note-list-index-face))
                     (insert (propertize (format "[%s] " short-id)
                                         'face 'hemis-note-list-id-face))
                     (insert (propertize short-file
                                         'face 'hemis-note-list-file-face))
                     (insert (propertize (format " L%d,C%d" line col)
                                         'face 'hemis-note-list-location-face))
                     (insert "\n")
                     (dolist (text-line (split-string txt "\n"))
                       (insert (propertize (format "    %s\n" text-line)
                                           'face 'hemis-note-list-text-face)))
                     (insert "\n")
                     (add-text-properties start (point)
                                          (list 'hemis-note note))))))
      (goto-char (point-min))
      (when backlinks
        (hemis-notes-list-next)))
    (pop-to-buffer buf)))


;;; Minor modes

(defvar hemis-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h a") #'hemis-add-note)
    (define-key map (kbd "C-c h r") #'hemis-refresh-notes)
    (define-key map (kbd "C-c h l") #'hemis-list-notes)
    (define-key map (kbd "C-c h p") #'hemis-index-project)
    (define-key map (kbd "C-c h s") #'hemis-search-project)
    (define-key map (kbd "C-c h k") #'hemis-insert-note-link)
    (define-key map (kbd "C-c h e") #'hemis-edit-note-at-point)
    (define-key map (kbd "C-c h E") #'hemis-edit-note-buffer)
    (define-key map (kbd "C-c h d") #'hemis-delete-note-at-point)
    (define-key map (kbd "C-c h b") #'hemis-show-backlinks)
    (define-key map (kbd "C-c h x") #'hemis-explain-region)
    (define-key map (kbd "C-c h X") #'hemis-explain-region-ai)
    (define-key map (kbd "C-c h R") #'hemis-reattach-note)
    (define-key map (kbd "C-c h S") #'hemis-status)
    (define-key map (kbd "C-c h ?") #'hemis-help)
    map)
  "Keymap for `hemis-notes-mode'.")

(defun hemis--ensure-notes-mode-keymap ()
  "Ensure `hemis-notes-mode-map` is a valid keymap (handles reloads)."
  (unless (keymapp hemis-notes-mode-map)
    (setq hemis-notes-mode-map (make-sparse-keymap))
    (define-key hemis-notes-mode-map (kbd "C-c h a") #'hemis-add-note)
    (define-key hemis-notes-mode-map (kbd "C-c h r") #'hemis-refresh-notes)
    (define-key hemis-notes-mode-map (kbd "C-c h l") #'hemis-list-notes)
    (define-key hemis-notes-mode-map (kbd "C-c h p") #'hemis-index-project)
    (define-key hemis-notes-mode-map (kbd "C-c h s") #'hemis-search-project)
    (define-key hemis-notes-mode-map (kbd "C-c h k") #'hemis-insert-note-link)
    (define-key hemis-notes-mode-map (kbd "C-c h e") #'hemis-edit-note-at-point)
    (define-key hemis-notes-mode-map (kbd "C-c h E") #'hemis-edit-note-buffer)
    (define-key hemis-notes-mode-map (kbd "C-c h d") #'hemis-delete-note-at-point)
    (define-key hemis-notes-mode-map (kbd "C-c h b") #'hemis-show-backlinks)
    (define-key hemis-notes-mode-map (kbd "C-c h x") #'hemis-explain-region)
    (define-key hemis-notes-mode-map (kbd "C-c h X") #'hemis-explain-region-ai)
    (define-key hemis-notes-mode-map (kbd "C-c h R") #'hemis-reattach-note)
    (define-key hemis-notes-mode-map (kbd "C-c h S") #'hemis-status)
    (define-key hemis-notes-mode-map (kbd "C-c h ?") #'hemis-help)))

;; Ensure keymap is valid before defining the minor mode.
(hemis--ensure-notes-mode-keymap)

(define-minor-mode hemis-notes-mode
  "Minor mode for displaying and editing Hemis notes (stickies) in code buffers."
  :lighter " Hemis"
  :keymap hemis-notes-mode-map
  (if hemis-notes-mode
      (progn
        (hemis--debug "hemis-notes-mode: enabling in %s" (buffer-name))
        (hemis--ensure-connection)
        (hemis-refresh-notes)
        (add-hook 'post-self-insert-hook #'hemis--maybe-trigger-link nil t))
    (hemis--debug "hemis-notes-mode: disabling in %s" (buffer-name))
    (remove-hook 'post-self-insert-hook #'hemis--maybe-trigger-link t)
    (hemis--clear-note-overlays)))

(defun hemis--maybe-enable-notes ()
  "Enable `hemis-notes-mode' in eligible buffers."
  (when (and buffer-file-name
             (derived-mode-p 'prog-mode))
    (hemis-notes-mode 1)))

(define-globalized-minor-mode hemis-notes-global-mode
  hemis-notes-mode hemis--maybe-enable-notes
  :group 'hemis)

;; Do NOT enable global mode by default - users should opt in via their config:
;;   (hemis-notes-global-mode 1)
;; This avoids unexpected backend connections and allows testing isolation.

(defun hemis--ensure-notes-list-keymap ()
  "Ensure `hemis-notes-list-mode-map` is a valid keymap with all bindings."
  (unless (keymapp hemis-notes-list-mode-map)
    (setq hemis-notes-list-mode-map (make-sparse-keymap))
    (set-keymap-parent hemis-notes-list-mode-map special-mode-map))
  ;; Always ensure all keybindings are present
  (define-key hemis-notes-list-mode-map (kbd "RET") #'hemis-notes-list-visit)
  (define-key hemis-notes-list-mode-map (kbd "v")   #'hemis-view-note)
  (define-key hemis-notes-list-mode-map (kbd "b")   #'hemis-show-backlinks)
  (define-key hemis-notes-list-mode-map (kbd "e")   #'hemis-edit-note-at-point)
  (define-key hemis-notes-list-mode-map (kbd "d")   #'hemis-delete-note-at-point)
  (define-key hemis-notes-list-mode-map (kbd "n")   #'hemis-notes-list-next)
  (define-key hemis-notes-list-mode-map (kbd "p")   #'hemis-notes-list-prev)
  (define-key hemis-notes-list-mode-map (kbd "q")   #'quit-window))

(defun hemis-reset-keymaps-and-enable ()
  "Repair Hemis keymaps after reloads and ensure global mode is enabled."
  (interactive)
  (hemis--ensure-notes-mode-keymap)
  (hemis--ensure-notes-list-keymap)
  (unless hemis-notes-global-mode
    (hemis-notes-global-mode 1)))

;; Fix up keymaps on reload (after defvars are in place) and ensure global mode.
(hemis-reset-keymaps-and-enable)

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

(defun hemis-help ()
  "Display Hemis keybindings and help."
  (interactive)
  (let ((buf (get-buffer-create "*Hemis Help*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize "Hemis - A Second Brain for Your Code\n"
                          'face '(:weight bold :height 1.2)))
      (insert (make-string 40 ?-) "\n\n")
      (insert (propertize "Code Buffer (hemis-notes-mode)\n"
                          'face '(:weight bold :underline t)))
      (insert "
  C-c h a    Add a note at point
  C-c h r    Refresh notes (reload from backend)
  C-c h l    List notes for current file
  C-c h e    Edit note at point
  C-c h E    Edit note in buffer (for longer notes)
  C-c h d    Delete note at point
  C-c h R    Reattach stale note to current position
  C-c h b    Show backlinks to note
  C-c h p    Index entire project
  C-c h s    Search project (files and notes)
  C-c h k    Insert a link to another note
  C-c h x    Explain selected region
  C-c h X    Explain region with AI
  C-c h S    Show status (note/file counts)
  C-c h ?    Show this help

")
      (insert (propertize "Notes List Buffer (*Hemis Notes*)\n"
                          'face '(:weight bold :underline t)))
      (insert "
  n, j       Next note
  p, k       Previous note
  RET        Visit note in file (other window if split)
  v          View note text in separate buffer
  b          Show backlinks
  e          Edit note
  d          Delete note
  q          Close notes list

")
      (insert (propertize "Search Results Buffer (*Hemis Search*)\n"
                          'face '(:weight bold :underline t)))
      (insert "
  RET        Visit search hit
  q          Close search results

")
      (insert (propertize "Note Entry\n"
                          'face '(:weight bold :underline t)))
      (insert "
  RET        Insert newline (notes can be multi-line)
  C-c C-c    Save note
  C-c C-k    Cancel note entry
  [[         Start inserting a link to another note

")
      (insert (propertize "Commands\n"
                          'face '(:weight bold :underline t)))
      (insert "
  M-x hemis-open-project      Set project root
  M-x hemis-explain-region    Explain selected code (requires LLM)
  M-x hemis-shutdown          Stop Hemis backend
  M-x hemis-reload            Reload Hemis (after code changes)
")
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

;;; Event handlers setup

(defun hemis--setup-event-handlers ()
  "Set up handlers for backend events."
  ;; Handle note position changes
  (hemis--events-on "note-position-changed"
    (lambda (event)
      (let ((file (alist-get 'file event)))
        (when-let ((buf (get-file-buffer file)))
          (with-current-buffer buf
            (hemis-refresh-notes))))))

  ;; Handle note created
  (hemis--events-on "note-created"
    (lambda (event)
      (let ((file (alist-get 'file event)))
        (when-let ((buf (get-file-buffer file)))
          (with-current-buffer buf
            (hemis-refresh-notes))))))

  ;; Handle note updated
  (hemis--events-on "note-updated"
    (lambda (_event)
      ;; Full refresh since we don't know which buffer
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf) (buffer-live-p buf))
          (with-current-buffer buf
            (when hemis--overlays
              (hemis-refresh-notes)))))))

  ;; Handle note deleted
  (hemis--events-on "note-deleted"
    (lambda (_event)
      ;; Full refresh since we don't know which buffer
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf) (buffer-live-p buf))
          (with-current-buffer buf
            (when hemis--overlays
              (hemis-refresh-notes))))))))

;; Initialize event handlers and start event client
(hemis--setup-event-handlers)
(add-hook 'emacs-startup-hook #'hemis--events-start)

(provide 'hemis)

;;; hemis.el ends here
