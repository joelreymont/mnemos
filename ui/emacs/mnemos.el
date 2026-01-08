;;; mnemos.el --- Mnemos: a second brain for your code  -*- lexical-binding: t; -*-

;; Emacs frontend for the Mnemos Zig backend (protocol v1).
;; - JSON-RPC 2.0 over stdio
;; - Notes minor mode with stickies as overlays
;; - Notes list buffer
;; - Project root awareness (for multi-file/project operations)

(require 'jsonrpc)
(require 'cl-lib)
(require 'project)
(require 'seq)

(defgroup mnemos nil
  "Mnemos – a second brain for your code."
  :group 'tools)

(defconst mnemos--default-backend
  (let* ((here (file-name-directory (or load-file-name buffer-file-name)))
         (root (expand-file-name "../.." here))
         (candidates (list (expand-file-name "zig-out/bin/mnemos" root))))
    (cl-find-if #'file-exists-p candidates))
  "Default path to the Mnemos backend binary (if built locally).")

(defcustom mnemos-backend mnemos--default-backend
  "Path to the Mnemos backend binary."
  :type 'string
  :group 'mnemos)

(defcustom mnemos-dir (expand-file-name "~/.mnemos")
  "Directory for Mnemos socket, lock, and log files."
  :type 'directory
  :group 'mnemos)

(defcustom mnemos-log-buffer "*Mnemos Log*"
  "Name of the buffer used to log Mnemos backend output."
  :type 'string
  :group 'mnemos)

(defcustom mnemos-backend-env nil
  "List of environment variables (\"KEY=VAL\") passed to the Mnemos backend process."
  :type '(repeat string)
  :group 'mnemos)

;; Expected protocol version (bump when backend protocol changes)
(defconst mnemos--expected-protocol-version 1
  "Expected protocol version. Backend should match this.")

(defcustom mnemos-request-timeout 60
  "Timeout (seconds) for JSON-RPC requests to the Mnemos backend."
  :type 'integer
  :group 'mnemos)

(defcustom mnemos-debug nil
  "When non-nil, log debug information to *Mnemos Debug* buffer.
Set to t for basic logging, or 'verbose for detailed RPC payloads."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Basic" t)
                 (const :tag "Verbose" verbose))
  :group 'mnemos)

(defvar mnemos--debug-buffer "*Mnemos Debug*"
  "Buffer name for debug output.")

(defvar mnemos--debug-log-file
  (expand-file-name "emacs-debug.log" (or (getenv "MNEMOS_DIR") "~/.mnemos"))
  "File path for debug log output.")

(defun mnemos--debug (fmt &rest args)
  "Log debug message FMT with ARGS if `mnemos-debug' is enabled.
Messages are timestamped and written to `mnemos--debug-buffer' and log file."
  (when mnemos-debug
    (let* ((msg (apply #'format fmt args))
           (ts (format-time-string "%H:%M:%S.%3N"))
           (line (format "[%s] %s\n" ts msg)))
      ;; Write to buffer
      (with-current-buffer (get-buffer-create mnemos--debug-buffer)
        (goto-char (point-max))
        (insert line))
      ;; Append to log file
      (let ((dir (file-name-directory mnemos--debug-log-file)))
        (unless (file-exists-p dir)
          (make-directory dir t)))
      (append-to-file line nil mnemos--debug-log-file))))

(defface mnemos-note-marker-face
  '((t :foreground "SteelBlue"
       :underline nil :overline nil :strike-through nil :box nil
       :extend nil))
  "Face for Mnemos sticky note markers."
  :group 'mnemos)

(defface mnemos-note-list-index-face
  '((t :foreground "gray50" :weight normal))
  "Face for note index number in the notes list."
  :group 'mnemos)

(defface mnemos-note-list-id-face
  '((t :foreground "DarkOrange" :weight bold))
  "Face for note ID in the notes list."
  :group 'mnemos)

(defface mnemos-note-list-file-face
  '((t :foreground "CadetBlue" :weight normal))
  "Face for file path in the notes list."
  :group 'mnemos)

(defface mnemos-note-list-location-face
  '((t :foreground "gray60"))
  "Face for line/column location in the notes list."
  :group 'mnemos)

(defface mnemos-note-list-text-face
  '((t :foreground "gray80"))
  "Face for note text in the notes list."
  :group 'mnemos)

(defvar mnemos--process nil
  "Process object for the Mnemos backend.")

(defvar mnemos--conn nil
  "JSON-RPC connection to the Mnemos backend.")

(defvar mnemos--project-root-override nil
  "Project root set via `mnemos-open-project' (takes precedence over `project-current').")

(defvar-local mnemos--overlays nil
  "List of Mnemos note overlays in the current buffer.")

(defvar mnemos--selected-note nil
  "The currently selected note (global, persists across buffers).")

;;; Event client for push notifications

(defvar mnemos--events-process nil
  "Process object for the events socket connection.")

(defvar mnemos--events-buffer ""
  "Buffer for accumulating partial event data.")

(defvar mnemos--events-handlers (make-hash-table :test 'equal)
  "Hash table of event type -> handler function.")

(defvar mnemos--events-reconnect-timer nil
  "Timer for reconnecting to events socket.")

(defun mnemos--events-socket-path ()
  "Return the path to the Mnemos events socket."
  (expand-file-name "events.sock" mnemos-dir))

(defun mnemos--events-filter (proc output)
  "Process filter for events socket. Parse JSON lines from OUTPUT."
  (setq mnemos--events-buffer (concat mnemos--events-buffer output))
  ;; Parse complete lines
  (while (string-match "\n" mnemos--events-buffer)
    (let* ((newline-pos (match-beginning 0))
           (line (substring mnemos--events-buffer 0 newline-pos)))
      (setq mnemos--events-buffer (substring mnemos--events-buffer (1+ newline-pos)))
      (when (> (length line) 0)
        (condition-case err
            (let* ((event (json-parse-string line :object-type 'alist))
                   (event-type (alist-get 'type event)))
              (mnemos--debug "Event: %s" event-type)
              (let ((handler (gethash event-type mnemos--events-handlers)))
                (when handler
                  (funcall handler event)))
              ;; Also call catch-all handler
              (let ((catch-all (gethash "*" mnemos--events-handlers)))
                (when catch-all
                  (funcall catch-all event))))
          (error
           (mnemos--debug "Event parse error: %s" (error-message-string err))))))))

(defun mnemos--events-sentinel (proc event)
  "Process sentinel for events socket. Reconnect on disconnect."
  (mnemos--debug "Events socket: %s" (string-trim event))
  (when (memq (process-status proc) '(closed exit signal))
    (setq mnemos--events-process nil)
    (mnemos--events-schedule-reconnect)))

(defun mnemos--events-schedule-reconnect ()
  "Schedule reconnection to events socket."
  (unless mnemos--events-reconnect-timer
    (setq mnemos--events-reconnect-timer
          (run-at-time 2 nil #'mnemos--events-try-connect))))

(defun mnemos--events-try-connect ()
  "Try to connect to the events socket."
  (setq mnemos--events-reconnect-timer nil)
  (if (file-exists-p (mnemos--events-socket-path))
      (condition-case err
          (let ((proc (make-network-process
                       :name "mnemos-events"
                       :family 'local
                       :service (mnemos--events-socket-path)
                       :coding 'utf-8
                       :noquery t
                       :filter #'mnemos--events-filter
                       :sentinel #'mnemos--events-sentinel)))
            (setq mnemos--events-process proc)
            (setq mnemos--events-buffer "")
            (mnemos--debug "Connected to events socket"))
        (error
         (mnemos--debug "Events connect error: %s" (error-message-string err))
         (mnemos--events-schedule-reconnect)))
    (mnemos--events-schedule-reconnect)))

(defun mnemos--events-start ()
  "Start the events client."
  (unless (and mnemos--events-process (process-live-p mnemos--events-process))
    (mnemos--events-try-connect)))

(defun mnemos--events-stop ()
  "Stop the events client."
  (when mnemos--events-reconnect-timer
    (cancel-timer mnemos--events-reconnect-timer)
    (setq mnemos--events-reconnect-timer nil))
  (when (and mnemos--events-process (process-live-p mnemos--events-process))
    (delete-process mnemos--events-process))
  (setq mnemos--events-process nil)
  (setq mnemos--events-buffer ""))

(defun mnemos--events-on (event-type handler)
  "Register HANDLER for EVENT-TYPE events.
EVENT-TYPE is a string like \"note-created\" or \"*\" for all events."
  (puthash event-type handler mnemos--events-handlers))

(defun mnemos--kill-backend-processes ()
  "Kill any running Mnemos backend processes and clear state."
  (dolist (proc (process-list))
    (when (and (process-live-p proc)
               (string-match-p "^mnemos-backend" (process-name proc)))
      (ignore-errors
        (kill-process proc)
        (accept-process-output proc 0.05))))
  (setq mnemos--process nil
        mnemos--conn nil))

;; NOTE: Git helpers removed - server auto-computes commit/blob/projectRoot from file


;;; Process & JSON-RPC management (Unix socket mode)

(defun mnemos--socket-path ()
  "Return the path to the Mnemos socket."
  (expand-file-name "mnemos.sock" mnemos-dir))

(defun mnemos--lock-path ()
  "Return the path to the Mnemos lock file."
  (expand-file-name "mnemos.lock" mnemos-dir))

(defun mnemos--log-path ()
  "Return the path to the Mnemos log file."
  (expand-file-name "mnemos.log" mnemos-dir))

(defun mnemos--check-startup-error ()
  "Check log file for startup errors and return user-friendly message, or nil."
  (let ((log-path (mnemos--log-path)))
    (when (file-exists-p log-path)
      (with-temp-buffer
        (insert-file-contents log-path)
        ;; Get last 10 lines
        (goto-char (point-max))
        (forward-line -10)
        (let ((tail (buffer-substring-no-properties (point) (point-max))))
          (cond
           ((string-match "Error:\\|FATAL\\|panic" tail)
            (concat "Backend failed to start.\n\n"
                    "Check " log-path " for details:\n" tail))
           (t nil)))))))

(defun mnemos--socket-exists-p ()
  "Return non-nil if the socket file exists."
  (file-exists-p (mnemos--socket-path)))

(defun mnemos--read-lock-pid ()
  "Read and return the PID from the lock file, or nil."
  (let ((lock (mnemos--lock-path)))
    (when (file-exists-p lock)
      (with-temp-buffer
        (insert-file-contents lock)
        (string-to-number (buffer-substring-no-properties
                           (point-min)
                           (line-end-position)))))))

(defun mnemos--process-alive-p (pid)
  "Return non-nil if process with PID is alive."
  (when pid
    (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid)))))

(defun mnemos--try-acquire-lock ()
  "Try to acquire the lock file. Return non-nil if successful."
  (let ((lock (mnemos--lock-path)))
    (condition-case nil
        (progn
          ;; Ensure mnemos-dir exists
          (make-directory mnemos-dir t)
          ;; Try to create lock file exclusively
          (with-temp-file lock
            (insert (format "%d\n" (emacs-pid))))
          t)
      (error nil))))

(defun mnemos--start-server ()
  "Start the Mnemos server process in the background.
The server runs detached with output redirected to `mnemos--log-path'.
Use `mnemos-shutdown' to stop it (sends shutdown RPC)."
  (let* ((exe (or mnemos-backend (error "Set `mnemos-backend' to the Mnemos backend binary")))
         (exe-abs (expand-file-name exe))
         (log (mnemos--log-path))
         ;; Build environment variables as shell exports
         (env-exports (mapconcat
                       (lambda (e)
                         (format "export %s;" (shell-quote-argument e)))
                       mnemos-backend-env
                       " "))
         (cmd (format "%s %s --serve >> %s 2>&1 &"
                      env-exports
                      (shell-quote-argument exe-abs)
                      (shell-quote-argument log))))
    (make-directory mnemos-dir t)
    (message "Mnemos: starting server: %s --serve (log: %s)" exe-abs log)
    ;; Run detached via shell - server stops via shutdown RPC
    (call-process-shell-command cmd nil 0)
    (message "Mnemos: started backend server")))

(defun mnemos--wait-for-socket (timeout-secs)
  "Wait up to TIMEOUT-SECS for the socket to appear. Return non-nil if found."
  (let ((deadline (+ (float-time) timeout-secs)))
    (while (and (not (mnemos--socket-exists-p))
                (< (float-time) deadline))
      (sleep-for 0.1))
    (mnemos--socket-exists-p)))

(defun mnemos--connect-socket ()
  "Connect to the Mnemos socket. Return the process or nil on failure."
  (condition-case err
      (let* ((buf (get-buffer-create mnemos-log-buffer))
             (proc (make-network-process
                    :name "mnemos-backend"
                    :buffer buf
                    :family 'local
                    :service (mnemos--socket-path)
                    :coding 'no-conversion
                    :noquery t)))
        proc)
    (error
     (message "Mnemos: failed to connect to socket: %s" (error-message-string err))
     nil)))

(defun mnemos--start-process ()
  "Connect to the Mnemos backend server, starting it if necessary."
  (unless (and mnemos--process (process-live-p mnemos--process))
    ;; Clean up any existing connection
    (mnemos--kill-backend-processes)

    ;; Try to connect to existing socket
    (let ((connected nil))
      (when (mnemos--socket-exists-p)
        (let ((proc (mnemos--connect-socket)))
          (when proc
            (mnemos--setup-connection proc)
            (setq connected t))))

      (unless connected
        ;; Socket doesn't exist or connection failed - need to start server
        ;; Check if stale lock/socket
        (when (mnemos--socket-exists-p)
          (let ((pid (mnemos--read-lock-pid)))
            (unless (mnemos--process-alive-p pid)
              ;; Stale, clean up
              (message "Mnemos: cleaning up stale socket")
              (ignore-errors (delete-file (mnemos--socket-path)))
              (ignore-errors (delete-file (mnemos--lock-path))))))

        ;; Try to start the server
        (if (mnemos--try-acquire-lock)
            (progn
              (mnemos--start-server)
              (unless (mnemos--wait-for-socket 5)
                (ignore-errors (delete-file (mnemos--lock-path)))
                ;; Check log for startup errors (e.g., schema version mismatch)
                (let ((startup-err (mnemos--check-startup-error)))
                  (if startup-err
                      (error "Mnemos: %s" startup-err)
                    (error "Mnemos: server failed to create socket"))))
              (sleep-for 0.1)
              (let ((proc (mnemos--connect-socket)))
                (unless proc
                  (error "Mnemos: failed to connect after starting server"))
                (mnemos--setup-connection proc)))
          ;; Someone else is starting the server, wait for socket
          (message "Mnemos: waiting for server to start...")
          (unless (mnemos--wait-for-socket 5)
            ;; Check log for startup errors
            (let ((startup-err (mnemos--check-startup-error)))
              (if startup-err
                  (error "Mnemos: %s" startup-err)
                (error "Mnemos: timeout waiting for server"))))
          (sleep-for 0.1)
          (let ((proc (mnemos--connect-socket)))
            (unless proc
              (error "Mnemos: failed to connect to server"))
            (mnemos--setup-connection proc)))))))

(cl-defun mnemos--setup-connection (proc)
  "Set up the jsonrpc connection using PROC."
  (mnemos--debug "Setting up connection, proc=%s" (process-name proc))
  (setq mnemos--process proc)
  (setq mnemos--conn
        (jsonrpc-process-connection
         :process proc
         :on-shutdown (lambda (&rest _)
                        (mnemos--debug "Connection shutdown callback")
                        (message "Mnemos backend shut down")
                        (setq mnemos--process nil
                              mnemos--conn nil))))
  ;; Check version
  (condition-case err
      (let ((info (mnemos--request "mnemos/version")))
        (when info
          (let ((proto (alist-get 'protocolVersion info)))
            (mnemos--debug "Connected: protocol=%s hash=%s"
                         (or proto "?") (or (alist-get 'gitHash info) "?"))
            (cond
             ((and proto (> proto mnemos--expected-protocol-version))
              (message "Mnemos: backend is newer. Consider updating the plugin."))
             ((and proto (< proto mnemos--expected-protocol-version))
              (display-warning 'mnemos
                               "Backend is outdated. Run: pkill -f 'mnemos --serve'"
                               :warning)))
            (message "Mnemos: connected to backend v%s (%s)"
                     (or proto "?")
                     (or (alist-get 'gitHash info) "?")))))
    (error
     (mnemos--debug "Version check failed: %s" (error-message-string err))
     (message "Mnemos: version check failed: %s" (error-message-string err)))))

(defun mnemos--ensure-connection ()
  "Ensure that the Mnemos backend connection is live."
  (unless (and mnemos--conn
               (jsonrpc-running-p mnemos--conn))
    (mnemos--start-process)))

(defun mnemos-shutdown ()
  "Shutdown the Mnemos backend server.
Sends shutdown RPC to stop the detached server process."
  (interactive)
  (mnemos--events-stop)
  (when (and mnemos--conn (jsonrpc-running-p mnemos--conn))
    (ignore-errors (jsonrpc-request mnemos--conn "shutdown" nil :timeout 1)))
  (when (and mnemos--process (process-live-p mnemos--process))
    (delete-process mnemos--process))
  (setq mnemos--process nil
        mnemos--conn nil)
  (message "Mnemos backend stopped."))

(defun mnemos-disconnect ()
  "Disconnect from the Mnemos backend (does not shutdown server)."
  (interactive)
  (when (and mnemos--process (process-live-p mnemos--process))
    (delete-process mnemos--process))
  (setq mnemos--process nil
        mnemos--conn nil)
  (message "Mnemos: disconnected."))

;; Disconnect when Emacs exits (server keeps running)
(add-hook 'kill-emacs-hook #'mnemos-disconnect)

(defun mnemos-reload ()
  "Reload the Mnemos Emacs package after code changes.
This disconnects from the backend, unloads the feature, and reloads it."
  (interactive)
  (mnemos-disconnect)
  (unload-feature 'mnemos t)
  (require 'mnemos)
  (message "Mnemos reloaded."))

(defun mnemos--request (method &optional params)
  "Synchronously send JSON-RPC METHOD with PARAMS and return result."
  (mnemos--ensure-connection)
  (mnemos--debug "RPC>> %s" method)
  (when (eq mnemos-debug 'verbose)
    (mnemos--debug "  params: %S" params))
  (let ((result (jsonrpc-request mnemos--conn method params :timeout mnemos-request-timeout)))
    (mnemos--debug "RPC<< %s ok" method)
    (when (eq mnemos-debug 'verbose)
      (mnemos--debug "  result: %S" result))
    result))

(defun mnemos--anchor-position (line col)
  "Return buffer position at LINE and COL.
Server computes actual anchor position from content when creating notes."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- (or line 1))))
    (move-to-column (or col 0))
    (point)))

(defun mnemos--note-anchor ()
  "Return plist with current cursor line and column.
Server computes anchor position, nodePath, and nodeTextHash from content."
  (list :line (line-number-at-pos)
        :column (current-column)))

(defun mnemos--line-indentation (pos)
  "Return indentation (whitespace) of the line at POS."
  (save-excursion
    (goto-char pos)
    (buffer-substring (line-beginning-position)
                      (progn (back-to-indentation) (point)))))

(defun mnemos--format-note-texts (texts pos &optional formatted-lines)
  "Format TEXTS as comment lines above POS using server-provided FORMATTED-LINES.
Backend guarantees formattedLines is always present.  Falls back to local
formatting with comment prefix if formatted-lines not provided (e.g., in tests)."
  (let* ((indent (mnemos--line-indentation pos))
         ;; Use server-provided lines, or format locally with comment prefix
         (lines (or formatted-lines
                    (let ((prefix (or comment-start "// ")))
                      ;; Ensure prefix ends with space
                      (unless (string-suffix-p " " prefix)
                        (setq prefix (concat prefix " ")))
                      ;; Double semicolon for Lisp
                      (when (string-match-p "^;\\s-*$" prefix)
                        (setq prefix ";; "))
                      ;; Split each text into lines and add prefix to each
                      (apply #'append
                             (mapcar (lambda (text)
                                       (mapcar (lambda (line)
                                                 (concat indent prefix line))
                                               (split-string text "\n" t)))
                                     texts))))))
    (concat (mapconcat #'identity lines "\n") "\n" indent)))


;;; Notes list mode (defined early so mnemos-list-notes can use it)

(defun mnemos-notes-list-next ()
  "Move to the next note in the notes list buffer."
  (interactive)
  (let ((start (point))
        (current-note (get-text-property (point) 'mnemos-note)))
    ;; Skip past current note's region
    (while (and (not (eobp))
                (eq (get-text-property (point) 'mnemos-note) current-note))
      (forward-line 1))
    ;; Find next note
    (while (and (not (eobp))
                (not (get-text-property (point) 'mnemos-note)))
      (forward-line 1))
    (if (get-text-property (point) 'mnemos-note)
        (beginning-of-line)
      (goto-char start)
      (user-error "No more notes"))))

(defun mnemos-notes-list-prev ()
  "Move to the previous note in the notes list buffer."
  (interactive)
  (let ((start (point))
        (current-note (get-text-property (point) 'mnemos-note)))
    ;; Move up one line first
    (forward-line -1)
    ;; Skip past current note's region going backwards
    (while (and (not (bobp))
                (eq (get-text-property (point) 'mnemos-note) current-note))
      (forward-line -1))
    ;; Now we might be in a previous note or in a gap - find the note start
    (let ((note (get-text-property (point) 'mnemos-note)))
      (if note
          ;; Go to the start of this note block
          (while (and (not (bobp))
                      (eq (get-text-property (1- (line-beginning-position)) 'mnemos-note) note))
            (forward-line -1))
        ;; No note here, go back to start
        (goto-char start)
        (user-error "No previous notes")))))

(defvar mnemos-notes-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'mnemos-notes-list-visit)
    (define-key map (kbd "v")   #'mnemos-view-note)
    (define-key map (kbd "b")   #'mnemos-show-backlinks)
    (define-key map (kbd "n")   #'mnemos-notes-list-next)
    (define-key map (kbd "p")   #'mnemos-notes-list-prev)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `mnemos-notes-list-mode'.")

(define-derived-mode mnemos-notes-list-mode special-mode "Mnemos-Notes"
  "Mode for listing Mnemos notes.
\\{mnemos-notes-list-mode-map}"
  :keymap mnemos-notes-list-mode-map
  (setq buffer-read-only t)
  ;; Set up evil-mode bindings if available
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'mnemos-notes-list-mode 'emacs)))

;; Evil-mode integration: define keys in normal and motion states
(with-eval-after-load 'evil
  (evil-define-key* '(normal motion emacs) mnemos-notes-list-mode-map
    (kbd "n") #'mnemos-notes-list-next
    (kbd "p") #'mnemos-notes-list-prev
    (kbd "RET") #'mnemos-notes-list-visit
    (kbd "v") #'mnemos-view-note
    (kbd "b") #'mnemos-show-backlinks
    (kbd "q") #'quit-window
    (kbd "j") #'mnemos-notes-list-next
    (kbd "k") #'mnemos-notes-list-prev))

;;; Notes data & overlays

(defvar mnemos--note-input-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "RET") #'newline)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-c") #'exit-minibuffer)
    (define-key map (kbd "C-c C-k") #'abort-recursive-edit)
    map)
  "Keymap for multi-line Mnemos note entry (RET inserts newline, C-c C-c to save).")

(defvar mnemos--note-history nil
  "Input history for Mnemos note text.")

(defun mnemos--read-note-text ()
  "Read multi-line note text from the minibuffer.
RET inserts a newline; use C-c C-c to finish or C-c C-k to cancel."
  (condition-case nil
      (read-from-minibuffer
       "Mnemos note (RET=newline, C-c C-c to save, C-c C-k to cancel):\n"
       nil mnemos--note-input-map nil 'mnemos--note-history)
    (quit nil)))

(defun mnemos--note-get (note key)
  "Get KEY from NOTE, handling symbol, keyword, and string keys."
  (or (plist-get note (intern (concat ":" (symbol-name key))))
      (alist-get key note)
      (alist-get (symbol-name key) note nil nil #'equal)))

(defun mnemos--note-text (note)
  "Extract note text or summary from NOTE."
  (or (mnemos--note-get note 'text)
      (mnemos--note-get note 'summary)))

(defun mnemos--clear-note-overlays ()
  "Remove all Mnemos note overlays from the current buffer."
  (when mnemos--overlays
    (mapc #'delete-overlay mnemos--overlays)
    (setq mnemos--overlays nil)))

(defun mnemos--make-note-overlay (note)
  "Create an overlay in the current buffer from NOTE.
NOTE is an alist or plist parsed from JSON, keys like :id, :line, :column, :summary.
When server provides displayLine, use that instead of stored line.
Server guarantees formattedLines is always present."
  (let* ((id     (mnemos--note-get note 'id))
         ;; Prefer server-computed displayLine when available
         (line   (or (mnemos--note-get note 'displayLine)
                     (mnemos--note-get note 'line)))
         (col    (or (mnemos--note-get note 'column) 0))
         (text   (or (mnemos--note-text note) "Note"))
         ;; Server-provided formatted lines (as vector from JSON)
         (formatted-raw (mnemos--note-get note 'formattedLines))
         (formatted (when formatted-raw
                      (if (vectorp formatted-raw)
                          (append formatted-raw nil)
                        formatted-raw)))
         (pos (mnemos--anchor-position line col))
         (line-bol (save-excursion (goto-char pos) (line-beginning-position)))
         ;; Drop stale overlays from other buffers or dead overlays.
         (valid (seq-filter (lambda (ov)
                              (and (overlay-buffer ov)
                                   (eq (overlay-buffer ov) (current-buffer))
                                   (overlay-start ov)))
                            mnemos--overlays))
         (marker-ov (seq-find (lambda (ov)
                                (and (= (overlay-start ov) pos)
                                     (overlay-get ov 'mnemos-note-marker)))
                              valid))
         (new-list valid))
    (unless marker-ov
      (setq marker-ov (make-overlay line-bol line-bol (current-buffer) t t))
      (overlay-put marker-ov 'mnemos-note-marker t)
      (overlay-put marker-ov 'mnemos-note-count 0)
      (overlay-put marker-ov 'mnemos-note-ids nil)
      (overlay-put marker-ov 'mnemos-notes nil)
      (overlay-put marker-ov 'mnemos-note-texts nil)
      (overlay-put marker-ov 'mnemos-note-formatted nil)
      (overlay-put marker-ov 'priority 9999)
      (overlay-put marker-ov 'evaporate nil)
      (push marker-ov new-list))
    (let* ((count (1+ (or (overlay-get marker-ov 'mnemos-note-count) 0)))
           (ids (cons id (overlay-get marker-ov 'mnemos-note-ids)))
           (notes (cons note (overlay-get marker-ov 'mnemos-notes)))
           (texts (append (overlay-get marker-ov 'mnemos-note-texts)
                          (list text)))
           ;; Collect all formatted lines from all notes at this position
           (all-formatted (append (overlay-get marker-ov 'mnemos-note-formatted)
                                  formatted))
           ;; Use server-formatted lines if available, otherwise format locally
           (display (mnemos--format-note-texts texts line-bol all-formatted)))
      (overlay-put marker-ov 'mnemos-note-count count)
      (overlay-put marker-ov 'mnemos-note-ids ids)
      (overlay-put marker-ov 'mnemos-notes notes)
      (overlay-put marker-ov 'mnemos-note-texts texts)
      (overlay-put marker-ov 'mnemos-note-formatted all-formatted)
      (overlay-put marker-ov 'before-string
                   (propertize display
                               'face '(:foreground "SteelBlue"
                                       :underline nil :overline nil
                                       :strike-through nil :box nil))))
    ;; Per-note overlay (no marker) for downstream consumers.
    (let ((note-ov (make-overlay line-bol line-bol (current-buffer) t t)))
      (overlay-put note-ov 'mnemos-note-id id)
      (overlay-put note-ov 'priority 9999)
      (overlay-put note-ov 'evaporate nil)
      (push note-ov new-list))
    (setq mnemos--overlays new-list)))

(defun mnemos--apply-notes (notes)
  "Render NOTES as overlays in the current buffer.
NOTES is a list of note objects (alist/plist) from the backend."
  (mnemos--debug "apply-notes: clearing %d old overlays" (length mnemos--overlays))
  (mnemos--clear-note-overlays)
  (mnemos--debug "apply-notes: creating overlays for %d notes" (length notes))
  (seq-do #'mnemos--make-note-overlay notes)
  (mnemos--debug "apply-notes: created %d overlays" (length mnemos--overlays))
  ;; Fallback: ensure at least one marker if notes exist but overlays evaporated.
  (when (and notes (null mnemos--overlays))
    (let ((pos (point-min)))
      (let ((marker-ov (make-overlay pos pos (current-buffer) t t)))
        (overlay-put marker-ov 'mnemos-note-marker t)
        (overlay-put marker-ov 'mnemos-note-count (length notes))
        (overlay-put marker-ov 'mnemos-note-texts (mapcar #'mnemos--note-text notes))
        (overlay-put marker-ov 'before-string
                     (propertize (mnemos--format-note-texts (mapcar #'mnemos--note-text notes) pos)
                                 'face '(:foreground "SteelBlue"
                                         :underline nil :overline nil
                                         :strike-through nil :box nil)))
        (overlay-put marker-ov 'priority 9999)
        (overlay-put marker-ov 'evaporate t)
        (push marker-ov mnemos--overlays)))))


;;; Backend calls

(defun mnemos--project-root ()
  "Return the project root for the current buffer, or nil."
  (or mnemos--project-root-override
      (when-let* ((proj (ignore-errors (project-current))))
        (condition-case nil
            (project-root proj)
          (error nil)))))

(defun mnemos--buffer-content ()
  "Return the current buffer content as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun mnemos--buffer-params (&optional include-content)
  "Return an alist describing the current buffer for the backend.
When INCLUDE-CONTENT is non-nil, include buffer content for server-side
position tracking.  Server auto-computes projectRoot, commit, and blob
from the file path when not provided."
  (let ((file (or (buffer-file-name) (buffer-name))))
    `((file . ,file)
      ,@(when include-content
          `((content . ,(mnemos--buffer-content)))))))

(defun mnemos-index-project (&optional root include-ai)
  "Index all files under ROOT (defaults to project root) and show progress.
With prefix arg or INCLUDE-AI non-nil, also run AI analysis."
  (interactive (list nil current-prefix-arg))
  (let ((root (or root (mnemos--project-root) default-directory)))
    (message "Mnemos: indexing project %s%s..."
             root (if include-ai " with AI analysis" ""))
    (let* ((params `((projectRoot . ,root)))
           (_ (when include-ai
                (setq params (append params '((includeAI . t))))))
           (resp (mnemos--request "mnemos/index-project" params))
           (indexed (alist-get 'indexed resp))
           ;; Backend guarantees statusMessage
           (status-msg (alist-get 'statusMessage resp)))
      (message "Mnemos: %s in %s" status-msg root))))

(defun mnemos-index-project-ai (&optional root)
  "Index all files under ROOT and run AI analysis."
  (interactive)
  (mnemos-index-project root t))

(defun mnemos-project-meta ()
  "Display project metadata (indexing and AI analysis status)."
  (interactive)
  (let* ((root (or (mnemos--project-root) default-directory))
         (resp (mnemos--request "mnemos/project-meta"
                               `((projectRoot . ,root)))))
    (with-current-buffer (get-buffer-create "*Mnemos Project*")
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
      (insert (format "AI Analysis: %s\n" (alist-get 'analysisStatusDisplay resp)))
      (when (alist-get 'analysisProvider resp)
        (insert (format "  Provider: %s\n" (alist-get 'analysisProvider resp))))
      (insert "\n")
      (insert (format "AI Available: %s\n"
                      (if (alist-get 'aiAvailable resp) "Yes" "No")))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun mnemos-search-project (query)
  "Search QUERY in indexed files/notes for the current project and show results."
  (interactive "sSearch query: ")
  (let* ((root (or (mnemos--project-root) default-directory))
         (results (let ((r (mnemos--request "mnemos/search"
                                           `((query . ,query)
                                             (projectRoot . ,root)
                                             (includeNotes . t)))))
                    (if (vectorp r) (append r nil) r)))
         (buf (get-buffer-create "*Mnemos Search*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert (format "Mnemos search for \"%s\" in %s\n\n" query root))
        (insert (format "%-6s %-6s %-40s %s\n" "Kind" "Score" "Location" "Text"))
        (insert (make-string 80 ?-))
        (insert "\n")
        (dolist (hit results)
          (let* ((file (mnemos--note-get hit 'file))
                 (line (mnemos--note-get hit 'line))
                 (col  (mnemos--note-get hit 'column))
                 (text (mnemos--note-get hit 'text))
                 (score (mnemos--note-get hit 'score))
                 (kind (or (mnemos--note-get hit 'kind) "file")))
            (insert (format "%-6s %-6.2f %-40s %s\n"
                            kind (or score 0.0)
                            (format "%s:%s:%s" file line col)
                            (or text "")))
            (add-text-properties (line-beginning-position 0) (line-end-position)
                                 (list 'mnemos-search-hit hit)))))
      (goto-char (point-min))
      (mnemos-search-results-mode))
    (display-buffer buf)))

(defvar mnemos--link-search-buffer "*Mnemos Link Search*"
  "Buffer name for note link search results.")

(defun mnemos--render-link-search (results)
  "Render note search RESULTS into the link search buffer."
  (let ((buf (get-buffer-create mnemos--link-search-buffer))
        (notes (if (vectorp results) (append results nil) results)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert "Mnemos note link search results\n\n")
        (dolist (note notes)
          (let ((id (mnemos--note-get note 'id))
                (summary (or (mnemos--note-get note 'summary)
                             (mnemos--note-get note 'text)))
                (file (mnemos--note-get note 'file)))
            (insert (format "%s\t%s\t%s\n" id summary file)))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(defun mnemos-insert-note-link (&optional query)
  "Search existing notes and insert a link of the form [[DESC][ID]]."
  (interactive)
  (let* ((query (or query (read-string "Link search query: ")))
         (root (or (mnemos--project-root) default-directory))
         (results (let ((r (mnemos--request "notes/search"
                                           `((query . ,query)
                                             (projectRoot . ,root)))))
                    (if (vectorp r) (append r nil) r))))
    (mnemos--render-link-search results)
    (unless results
      (user-error "No notes found"))
    (let* ((choices (mapcar (lambda (note)
                              (cons (format "%s (%s)"
                                            (or (mnemos--note-get note 'summary)
                                                (mnemos--note-get note 'text)
                                                "")
                                            (mnemos--note-get note 'id))
                                    note))
                            results))
           (chosen (cdr (assoc (completing-read "Select note: " choices nil t)
                               choices)))
           (default-desc (or (mnemos--note-get chosen 'summary)
                             (mnemos--note-get chosen 'text)
                             ""))
           (desc (read-string "Description: " default-desc))
           (id (mnemos--note-get chosen 'id)))
      (insert (format "[[%s][%s]]" desc id)))))

(defun mnemos--maybe-trigger-link ()
  "Trigger note link search when user types [[ in notes mode."
  (when (and (eq last-command-event ?\[)
             (>= (point) 2)
             (string= "[["
                      (buffer-substring-no-properties
                       (max (point-min) (- (point) 2))
                       (point))))
    (delete-char -2)
    (mnemos-insert-note-link)))

(defun mnemos-follow-link ()
  "Follow the note link at point.
Links have the format [[description][uuid]].  Navigate to the linked note."
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (col (current-column))
         (link-re "\\[\\[[^]]*\\]\\[\\([a-f0-9-]+\\)\\]\\]")
         (found-id nil))
    ;; Find link at cursor position
    (save-match-data
      (let ((pos 0))
        (while (and (not found-id)
                    (string-match link-re line pos))
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (id (match-string 1 line)))
            (when (and (>= col start) (<= col end))
              (setq found-id id))
            (setq pos end)))))
    (unless found-id
      (user-error "No link at point"))
    ;; Get the note
    (let ((note (mnemos--request "notes/get" `((id . ,found-id)))))
      (unless note
        (user-error "Note not found: %s" found-id))
      (let ((file (mnemos--note-get note 'file))
            (line (mnemos--note-get note 'line)))
        (unless file
          (user-error "Note has no file location"))
        ;; Push mark for navigation back
        (push-mark nil t)
        ;; Navigate to note
        (find-file file)
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))
        (message "Followed link to: %s"
                 (or (mnemos--note-get note 'summary) found-id))))))

(defun mnemos-navigate-back ()
  "Navigate back after following a link.
Uses the Emacs mark ring."
  (interactive)
  (pop-global-mark))

(defun mnemos-open-project (root)
  "Open Mnemos project at ROOT and remember it for subsequent RPCs."
  (interactive "DProject root: ")
  (setq mnemos--project-root-override (expand-file-name root))
  (mnemos--request "mnemos/open-project"
                  `((projectRoot . ,mnemos--project-root-override)))
  (message "Mnemos: project set to %s" mnemos--project-root-override))

(defun mnemos-reattach-note ()
  "Reattach the stale note at point to the current cursor position.
Use this when a note becomes stale due to code changes.
Shows picker if multiple notes at same position."
  (interactive)
  (let* ((note (mnemos--note-at-point-with-picker))
         (id (and note (mnemos--note-get note 'id))))
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
      (mnemos--request "notes/reattach" params)
      (message "Mnemos: note reattached")
      (mnemos-refresh-notes))))

(defun mnemos-explain-region (beg end &optional use-ai detailed)
  "Request an explanation for the region from BEG to END.
With prefix arg or USE-AI non-nil, use AI to explain.
With DETAILED non-nil, request a more thorough explanation."
  (interactive "r\nP")
  (unless (and (buffer-file-name) beg end)
    (user-error "No region or file to explain"))
  (let* ((file (buffer-file-name))
         (start-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (project-root (mnemos--project-root))
         (params `((file . ,file)
                   (startLine . ,start-line)
                   (endLine . ,end-line)
                   (content . ,content)))
         (_ (when project-root
              (setq params (append params `((projectRoot . ,project-root))))))
         (_ (when use-ai
              (setq params (append params '((useAI . t))))))
         (_ (when detailed
              (setq params (append params '((detailed . t))))))
         (resp (mnemos--request "mnemos/explain-region" params))
         (snippet (alist-get 'content resp))
         (explanation (alist-get 'explanation resp))
         (ai-info (alist-get 'ai resp)))
    (with-current-buffer (get-buffer-create "*Mnemos Explain*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Explanation for %s:%d-%d\n" file start-line end-line))
      ;; Backend guarantees statusDisplay when AI is used
      (when ai-info
        (insert (format "%s\n" (alist-get 'statusDisplay ai-info))))
      (insert "\n")
      (cond
       (explanation (insert explanation))
       (snippet (insert snippet))
       (t (insert "(No explanation available)")))
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun mnemos-explain-region-ai (beg end)
  "Request an AI-powered explanation for the region from BEG to END.
Creates a note at the region start with the AI explanation."
  (interactive "r")
  (mnemos--explain-region-create-note beg end nil))

(defun mnemos-explain-region-ai-detailed (beg end)
  "Request a detailed AI-powered explanation for the region from BEG to END.
Creates a note at the region start with the detailed AI explanation."
  (interactive "r")
  (mnemos--explain-region-create-note beg end t))

;; AI timer for explain-region
(defvar mnemos--ai-timer nil "Timer for AI thinking message.")
(defvar mnemos--ai-start-time nil "Start time for AI request.")
(defvar mnemos--ai-message nil "Base message for AI timer.")

(defun mnemos--ai-timer-update ()
  "Update AI thinking message with elapsed time."
  (when mnemos--ai-start-time
    (let ((elapsed (truncate (float-time (time-subtract (current-time) mnemos--ai-start-time)))))
      (message "%s %ds" mnemos--ai-message elapsed))))

(defun mnemos--ai-timer-start (msg)
  "Start AI timer with MSG."
  (mnemos--ai-timer-stop)
  (setq mnemos--ai-message msg
        mnemos--ai-start-time (current-time))
  (message "%s 0s" msg)
  (setq mnemos--ai-timer (run-with-timer 1 1 #'mnemos--ai-timer-update)))

(defun mnemos--ai-timer-stop ()
  "Stop AI timer."
  (when mnemos--ai-timer
    (cancel-timer mnemos--ai-timer)
    (setq mnemos--ai-timer nil))
  (setq mnemos--ai-start-time nil
        mnemos--ai-message nil))

(defun mnemos--explain-region-create-note (beg end detailed)
  "Request AI explanation for region BEG to END and create a note.
If DETAILED is non-nil, request a more thorough explanation.
Uses async request so timer updates are visible."
  (unless (and (buffer-file-name) beg end)
    (user-error "No region or file to explain"))
  (mnemos--ensure-connection)
  ;; Capture context before async call
  (let* ((buf (current-buffer))
         (file (buffer-file-name))
         (start-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (project-root (mnemos--project-root))
         (target-pos beg)
         (params `((file . ,file)
                   (startLine . ,start-line)
                   (endLine . ,end-line)
                   (content . ,content)
                   (useAI . t))))
    (when project-root
      (setq params (append params `((projectRoot . ,project-root)))))
    (when detailed
      (setq params (append params '((detailed . t)))))
    ;; Start timer - will update while async request runs
    (mnemos--ai-timer-start (if detailed "AI thinking deeply..." "AI thinking..."))
    ;; Async request allows timer to fire
    (jsonrpc-async-request
     mnemos--conn "mnemos/explain-region" params
     :success-fn
     (lambda (resp)
       (mnemos--ai-timer-stop)
       (let ((explanation (plist-get resp :explanation))
             (ai-info (plist-get resp :ai)))
         (if (not explanation)
             (message "No AI explanation available")
           (with-current-buffer buf
             (save-excursion
               (goto-char target-pos)
               (let* ((status-display (plist-get ai-info :statusDisplay))
                      (note-text (if status-display
                                     (format "%s %s" status-display explanation)
                                   explanation))
                      (anchor (mnemos--note-anchor))
                      (note-params (append (mnemos--buffer-params t)
                                           `((line . ,(plist-get anchor :line))
                                             (column . ,(plist-get anchor :column))
                                             (text . ,note-text)))))
                 (let ((note (mnemos--request "notes/create" note-params)))
                   (mnemos--debug "explain-region: created note id=%s" (mnemos--note-get note 'id))
                   (mnemos--make-note-overlay note)
                   (message "Mnemos: AI note created."))))))))
     :error-fn
     (lambda (err)
       (mnemos--ai-timer-stop)
       (message "AI explain failed: %s" (plist-get err :message)))
     :timeout mnemos-request-timeout)))

(defun mnemos-refresh-notes ()
  "Fetch and render all notes for the current buffer.
Sends buffer content so server can compute displayLine positions."
  (interactive)
  (when (buffer-file-name)
    (mnemos--debug "refresh-notes: file=%s buf=%s" (buffer-file-name) (buffer-name))
    (let* ((params (append (mnemos--buffer-params t) '((includeStale . t))))
           (notes  (let ((r (mnemos--request "notes/list-for-file" params)))
                     ;; Backend returns {notes: [...]} wrapper - unwrap it
                     (when r
                       (let ((unwrapped (or (plist-get r :notes)
                                            (alist-get 'notes r)
                                            (and (listp r) (not (alist-get 'notes r)) r))))
                         (if (vectorp unwrapped) (append unwrapped nil) unwrapped))))))
      (mnemos--debug "refresh-notes: got %d notes" (length notes))
      (mnemos--apply-notes notes)
      (when (and notes (null mnemos--overlays))
        ;; Fallback if overlays evaporated; place a marker at point-min.
        (let ((pos (point-min)))
          (let ((marker-ov (make-overlay pos pos (current-buffer) t t)))
            (overlay-put marker-ov 'mnemos-note-marker t)
            (overlay-put marker-ov 'mnemos-note-count (length notes))
            (overlay-put marker-ov 'before-string
                         (propertize (format "n%d" (length notes))
                                     'face '(:foreground "SteelBlue"
                                             :underline nil :overline nil
                                             :strike-through nil :box nil)))
            (overlay-put marker-ov 'priority 9999)
            (overlay-put marker-ov 'evaporate nil)
            (push marker-ov mnemos--overlays))))
      (unless mnemos--overlays
        (let ((ov (make-overlay (point-min) (point-min))))
          (overlay-put ov 'priority 9999)
          (push ov mnemos--overlays)))
      (message "Mnemos: %d notes loaded." (length notes)))))

(defun mnemos-add-note (text &optional tags)
  "Create a new Mnemos note at point with TEXT and optional TAGS list.
Sends buffer content so server can compute nodeTextHash."
  (interactive
   (let* ((buf (current-buffer))
          (note-text (mnemos--read-note-text)))
     ;; Restore buffer after minibuffer (for demo automation)
     (set-buffer buf)
     (list (or note-text (user-error "Note entry canceled")))))
  (let* ((anchor (mnemos--note-anchor))
         (params (append (mnemos--buffer-params t) ; include content
                         `((line . ,(plist-get anchor :line))
                           (column . ,(plist-get anchor :column))
                           (text . ,text)
                           (tags . ,tags)))))
    (mnemos--debug "add-note: line=%d col=%d file=%s"
                 (plist-get anchor :line)
                 (plist-get anchor :column)
                 (buffer-file-name))
    (let ((note (mnemos--request "notes/create" params)))
      (mnemos--debug "add-note: created id=%s" (mnemos--note-get note 'id))
      (mnemos--make-note-overlay note)
      (message "Mnemos: note created.")
      note)))

(defun mnemos-get-note (id)
  "Fetch a single Mnemos note by ID."
  (mnemos--request "notes/get" `((id . ,id))))

(defun mnemos-update-note (id text)
  "Update note ID with new TEXT."
  (mnemos--request "notes/update" `((id . ,id) (text . ,text))))

(defun mnemos-delete-note (id)
  "Delete note with ID."
  (mnemos--request "notes/delete" `((id . ,id))))

(defun mnemos-status ()
  "Display Mnemos backend status: note/file/embedding counts."
  (interactive)
  (let* ((resp (mnemos--request "mnemos/status" nil))
         (counts (or (alist-get 'counts resp) resp))
         (notes (or (alist-get 'notes counts) 0))
         (files (or (alist-get 'files counts) 0))
         (embeddings (or (alist-get 'embeddings counts) 0)))
    (message "Mnemos: %d notes, %d files, %d embeddings" notes files embeddings)))

(defun mnemos-edit-note-at-point ()
  "Edit the note at point (overlay or notes list).
Shows picker if multiple notes at same position."
  (interactive)
  (let* ((note (mnemos--note-at-point-with-picker))
         (id (and note (mnemos--note-get note 'id)))
         (text (and note (mnemos--note-text note))))
    (unless id
      (user-error "No note at point"))
    (let ((new-text (mnemos--read-note-text text)))
      (when (and new-text (not (string= new-text text)))
        (mnemos-update-note id new-text)
        (message "Note updated")
        (mnemos-refresh-notes)))))

(defvar-local mnemos--edit-buffer-note-id nil
  "The note ID being edited in this buffer.")

(defun mnemos-edit-note-buffer ()
  "Edit the note at point in a dedicated buffer.
Opens a new buffer with the note content for editing longer notes.
Use C-c C-c to save, C-c C-k to cancel.
Uses selected note if set, otherwise shows picker if multiple notes."
  (interactive)
  (let* ((note (or mnemos--selected-note (mnemos--note-at-point-with-picker)))
         (id (and note (mnemos--note-get note 'id)))
         (text (and note (mnemos--note-text note))))
    (unless id
      (user-error "No note at point"))
    ;; Backend guarantees shortId is always present
    (let* ((short-id (mnemos--note-get note 'shortId))
           (buf (get-buffer-create (format "*Mnemos Edit: %s*" short-id))))
      (pop-to-buffer buf)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (markdown-mode)
      (setq-local mnemos--edit-buffer-note-id id)
      (local-set-key (kbd "C-c C-c") #'mnemos--edit-buffer-save)
      (local-set-key (kbd "C-c C-k") #'mnemos--edit-buffer-cancel)
      (set-buffer-modified-p nil)
      (message "Edit note. C-c C-c to save, C-c C-k to cancel."))))

(defun mnemos--edit-buffer-save ()
  "Save the note being edited in the current buffer."
  (interactive)
  (let ((id mnemos--edit-buffer-note-id)
        (text (buffer-string))) ; Backend trims
    (unless id
      (user-error "No note ID in this buffer"))
    (when (string-empty-p (string-trim text))
      (user-error "Note text cannot be empty"))
    (mnemos-update-note id text)
    (set-buffer-modified-p nil)
    (message "Note saved")
    (kill-buffer-and-window)
    (mnemos-refresh-notes)))

(defun mnemos--edit-buffer-cancel ()
  "Cancel editing and close the buffer."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard changes? "))
    (set-buffer-modified-p nil)
    (kill-buffer-and-window)))

(defun mnemos-delete-note-at-point ()
  "Delete the note at point (overlay or notes list).
Shows picker if multiple notes at same position."
  (interactive)
  (let* ((note (mnemos--note-at-point-with-picker))
         (id (and note (mnemos--note-get note 'id))))
    (unless id
      (user-error "No note at point"))
    (when (yes-or-no-p "Delete this note? ")
      (mnemos-delete-note id)
      (message "Note deleted")
      (mnemos-refresh-notes))))

(defun mnemos-get-selected-note ()
  "Return the currently selected note, or nil."
  mnemos--selected-note)

(defun mnemos-set-selected-note (note)
  "Set the selected NOTE (or nil to clear)."
  (setq mnemos--selected-note note)
  (force-mode-line-update t))

(defun mnemos-select-note ()
  "Select the note at point, or choose from notes in current file.
If cursor is on a note, select it. If multiple notes at cursor,
show picker for those. Otherwise show picker for all notes in file."
  (interactive)
  (let* ((note-at-point (mnemos--note-at-point-with-picker)))
    (if note-at-point
        ;; Selected note at cursor (possibly via picker)
        ;; Backend guarantees shortId is always present
        (let* ((short-id (mnemos--note-get note-at-point 'shortId))
               (summary (or (mnemos--note-text note-at-point) "")))
          (mnemos-set-selected-note note-at-point)
          (message "Selected note: %s - %s" short-id (truncate-string-to-width summary 40)))
      ;; No note at cursor, show picker for all notes in file
      (let* ((params (mnemos--buffer-params))
             (notes (let ((result (mnemos--request "notes/list-for-file"
                                                   (append params '((includeStale . t))))))
                      ;; Backend returns {notes: [...]} wrapper - unwrap it
                      (when result
                        (let ((unwrapped (or (plist-get result :notes)
                                             (alist-get 'notes result)
                                             (and (listp result) (not (alist-get 'notes result)) result))))
                          (if (vectorp unwrapped) (append unwrapped nil) unwrapped))))))
        (if (null notes)
            (message "No notes in this file")
          (let* ((choices (mapcar (lambda (note)
                                    (let* ((short-id (mnemos--note-get note 'shortId))
                                           (line (or (mnemos--note-get note 'line) 0))
                                           (summary (or (mnemos--note-text note) "")))
                                      (cons (format "[%s] L%d: %s" short-id line
                                                    (truncate-string-to-width summary 40))
                                            note)))
                                  notes))
                 (chosen (cdr (assoc (completing-read "Select note: " choices nil t)
                                     choices))))
            (when chosen
              (let* ((short-id (mnemos--note-get chosen 'shortId)))
                (mnemos-set-selected-note chosen)
                (message "Selected note: %s" short-id)))))))))

(defun mnemos-clear-selection ()
  "Clear the current note selection."
  (interactive)
  (when mnemos--selected-note
    (message "Note selection cleared"))
  (mnemos-set-selected-note nil))

(defun mnemos--note-at-overlay (pos)
  "Return the mnemos note at POS from overlays, if any."
  (let ((result nil)
        (line-bol (save-excursion (goto-char pos) (line-beginning-position))))
    (dolist (ov mnemos--overlays)
      ;; Check marker overlays at start of line (zero-width overlays at line-bol)
      (when (and (overlay-get ov 'mnemos-note-marker)
                 (= (overlay-start ov) line-bol))
        (let ((notes (overlay-get ov 'mnemos-notes)))
          (when notes
            (setq result (car notes))))))
    result))

(defun mnemos--notes-at-overlay (pos)
  "Return ALL mnemos notes at POS from overlays."
  (let ((result nil)
        (line-bol (save-excursion (goto-char pos) (line-beginning-position))))
    (dolist (ov mnemos--overlays)
      ;; Check marker overlays at start of line (zero-width overlays at line-bol)
      (when (and (overlay-get ov 'mnemos-note-marker)
                 (= (overlay-start ov) line-bol))
        (let ((notes (overlay-get ov 'mnemos-notes)))
          (when notes
            (setq result (append result notes))))))
    result))

(defun mnemos--note-at-point-with-picker ()
  "Return note at point, showing picker if multiple notes at same position.
Returns nil if no note or user cancelled."
  (let* ((text-prop-note (get-text-property (point) 'mnemos-note))
         (overlay-notes (mnemos--notes-at-overlay (point)))
         (all-notes (if text-prop-note
                        (cons text-prop-note overlay-notes)
                      overlay-notes)))
    (cond
     ((null all-notes) nil)
     ((= (length all-notes) 1) (car all-notes))
     (t
      ;; Multiple notes - show picker
      (let* ((choices (mapcar (lambda (note)
                                (let* ((short-id (mnemos--note-get note 'shortId))
                                       (summary (or (mnemos--note-text note) "")))
                                  (cons (format "[%s] %s" short-id
                                                (truncate-string-to-width summary 40))
                                        note)))
                              all-notes))
             (chosen (cdr (assoc (completing-read "Multiple notes here: " choices nil t)
                                 choices))))
        chosen)))))

(defun mnemos-notes-for-node (node-path)
  "Return notes for NODE-PATH in the current file/project."
  (let* ((params (mnemos--buffer-params))
         (file (alist-get 'file params))
         (proj (alist-get 'projectRoot params))
         (commit (alist-get 'commit params))
         (blob (alist-get 'blob params))
         (result (mnemos--request "notes/list-by-node"
                                 `((file . ,file)
                                   (projectRoot . ,proj)
                                   (commit . ,commit)
                                   (blob . ,blob)
                                   (includeStale . t)
                                   (nodePath . ,(and node-path (vconcat node-path)))))))
    (if (vectorp result) (append result nil) result)))

(defun mnemos-list-notes ()
  "List all Mnemos notes for the current file in a separate buffer."
  (interactive)
  (let* ((params (mnemos--buffer-params))
         (notes  (let ((result (mnemos--request "notes/list-for-file"
                                               (append params '((includeStale . t))))))
                   ;; Backend returns {notes: [...]} wrapper - unwrap it
                   (when result
                     (let ((unwrapped (or (plist-get result :notes)
                                          (alist-get 'notes result)
                                          (and (listp result) (not (alist-get 'notes result)) result))))
                       (if (vectorp unwrapped) (append unwrapped nil) unwrapped)))))
         (buf    (get-buffer-create "*Mnemos Notes*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mnemos-notes-list-mode)
      (let ((inhibit-read-only t))
        (insert (format "Mnemos notes for %s\n\n"
                        (cdr (assoc 'file params))))
        (cl-loop for note in notes
                 for idx from 0 do
                 ;; Backend guarantees shortId is always present
                 (let* ((file (mnemos--note-get note 'file))
                        (line (or (mnemos--note-get note 'line) 0))
                        (col  (or (mnemos--note-get note 'column) 0))
                        (txt  (or (mnemos--note-text note) ""))
                        (start (point))
                        (short-id (mnemos--note-get note 'shortId))
                        (short-file (file-name-nondirectory (or file ""))))
                   ;; Header line with faces
                   (insert (propertize (format "%3d " idx)
                                       'face 'mnemos-note-list-index-face))
                   (insert (propertize (format "[%s] " short-id)
                                       'face 'mnemos-note-list-id-face))
                   (insert (propertize short-file
                                       'face 'mnemos-note-list-file-face))
                   (insert (propertize (format " L%d,C%d" line col)
                                       'face 'mnemos-note-list-location-face))
                   (insert "\n")
                   ;; Note text indented with face
                   (dolist (text-line (split-string txt "\n"))
                     (insert (propertize (format "    %s\n" text-line)
                                         'face 'mnemos-note-list-text-face)))
                   (insert "\n")
                   ;; Mark the whole note block with the property
                   (add-text-properties start (point)
                                        (list 'mnemos-note note)))))
      (goto-char (point-min))
      ;; Move to first note if any
      (when notes
        (mnemos-notes-list-next)))
    (pop-to-buffer buf)))

(defun mnemos-view-note (&optional note)
  "View NOTE text in a Markdown buffer.
If NOTE is nil, use the note at point in *Mnemos Notes*, or prompt for an id."
  (interactive)
  (let* ((note (or note
                   (get-text-property (line-beginning-position) 'mnemos-note)
                   (let ((id (read-string "Note id: ")))
                     (mnemos-get-note id))))
         (text (or (mnemos--note-text note) "")))
    (with-current-buffer (get-buffer-create "*Mnemos Note*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (if (fboundp 'markdown-mode)
          (markdown-mode)
        (text-mode))
      (view-mode 1)
      (display-buffer (current-buffer)))))

(defun mnemos-notes-list-visit ()
  "Visit the note on the current line in its file.
Opens in another window if available, keeping the notes list visible."
  (interactive)
  (let* ((note (get-text-property (line-beginning-position)
                                  'mnemos-note))
         (file (mnemos--note-get note 'file))
         (line (or (mnemos--note-get note 'line) 1))
         (col  (or (mnemos--note-get note 'column) 0)))
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
    (message "Mnemos: jumped to note %s"
             (mnemos--note-get note 'id))))

(defun mnemos-show-backlinks (&optional note-or-id)
  "Show all notes that link TO the given NOTE-OR-ID.
If called interactively in *Mnemos Notes*, use the note at point.
Otherwise, prompt for a note ID.
Shows picker if multiple notes at same position."
  (interactive)
  (let* ((note (cond
                ((and (null note-or-id)
                      (get-text-property (line-beginning-position) 'mnemos-note))
                 (get-text-property (line-beginning-position) 'mnemos-note))
                ((null note-or-id)
                 ;; Not in notes list, try picker at point
                 (mnemos--note-at-point-with-picker))
                ((stringp note-or-id) nil)
                (t note-or-id)))
         (id (cond
              ((stringp note-or-id) note-or-id)
              (note (mnemos--note-get note 'id))
              (t (read-string "Note id: "))))
         (backlinks (let ((result (mnemos--request "notes/backlinks" `((id . ,id)))))
                      (if (vectorp result) (append result nil) result)))
         (buf (get-buffer-create "*Mnemos Backlinks*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mnemos-notes-list-mode)
      (let ((inhibit-read-only t))
        (insert (format "Backlinks to note %s\n" id))
        (insert (format "(%d notes link to this note)\n\n"
                        (length backlinks)))
        (if (null backlinks)
            (insert "No backlinks found.\n")
          (cl-loop for note in backlinks
                   for idx from 0 do
                   ;; Backend guarantees shortId is always present
                   (let* ((file (mnemos--note-get note 'file))
                          (line (or (mnemos--note-get note 'line) 0))
                          (col  (or (mnemos--note-get note 'column) 0))
                          (txt  (or (mnemos--note-text note) ""))
                          (start (point))
                          (short-id (mnemos--note-get note 'shortId))
                          (short-file (file-name-nondirectory (or file ""))))
                     (insert (propertize (format "%3d " idx)
                                         'face 'mnemos-note-list-index-face))
                     (insert (propertize (format "[%s] " short-id)
                                         'face 'mnemos-note-list-id-face))
                     (insert (propertize short-file
                                         'face 'mnemos-note-list-file-face))
                     (insert (propertize (format " L%d,C%d" line col)
                                         'face 'mnemos-note-list-location-face))
                     (insert "\n")
                     (dolist (text-line (split-string txt "\n"))
                       (insert (propertize (format "    %s\n" text-line)
                                           'face 'mnemos-note-list-text-face)))
                     (insert "\n")
                     (add-text-properties start (point)
                                          (list 'mnemos-note note))))))
      (goto-char (point-min))
      (when backlinks
        (mnemos-notes-list-next)))
    (pop-to-buffer buf)))


;;; Minor modes

(defvar mnemos-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m a") #'mnemos-add-note)
    (define-key map (kbd "C-c m r") #'mnemos-refresh-notes)
    (define-key map (kbd "C-c m l") #'mnemos-list-notes)
    (define-key map (kbd "C-c m p") #'mnemos-index-project)
    (define-key map (kbd "C-c m f") #'mnemos-search-project)
    (define-key map (kbd "C-c m k") #'mnemos-insert-note-link)
    (define-key map (kbd "C-c m ]") #'mnemos-follow-link)
    (define-key map (kbd "C-c m [") #'mnemos-navigate-back)
    (define-key map (kbd "C-c m e") #'mnemos-edit-note-at-point)
    (define-key map (kbd "C-c m E") #'mnemos-edit-note-buffer)
    (define-key map (kbd "C-c m d") #'mnemos-delete-note-at-point)
    (define-key map (kbd "C-c m b") #'mnemos-show-backlinks)
    (define-key map (kbd "C-c m x") #'mnemos-explain-region-ai)
    (define-key map (kbd "C-c m X") #'mnemos-explain-region-ai-detailed)
    (define-key map (kbd "C-c m R") #'mnemos-reattach-note)
    (define-key map (kbd "C-c m S") #'mnemos-status)
    (define-key map (kbd "C-c m s") #'mnemos-select-note)
    (define-key map (kbd "C-c m <escape>") #'mnemos-clear-selection)
    (define-key map (kbd "C-c m ?") #'mnemos-help)
    map)
  "Keymap for `mnemos-notes-mode'.")

(defun mnemos--ensure-notes-mode-keymap ()
  "Ensure `mnemos-notes-mode-map` is a valid keymap (handles reloads)."
  (unless (keymapp mnemos-notes-mode-map)
    (setq mnemos-notes-mode-map (make-sparse-keymap))
    (define-key mnemos-notes-mode-map (kbd "C-c m a") #'mnemos-add-note)
    (define-key mnemos-notes-mode-map (kbd "C-c m r") #'mnemos-refresh-notes)
    (define-key mnemos-notes-mode-map (kbd "C-c m l") #'mnemos-list-notes)
    (define-key mnemos-notes-mode-map (kbd "C-c m p") #'mnemos-index-project)
    (define-key mnemos-notes-mode-map (kbd "C-c m f") #'mnemos-search-project)
    (define-key mnemos-notes-mode-map (kbd "C-c m k") #'mnemos-insert-note-link)
    (define-key mnemos-notes-mode-map (kbd "C-c m ]") #'mnemos-follow-link)
    (define-key mnemos-notes-mode-map (kbd "C-c m [") #'mnemos-navigate-back)
    (define-key mnemos-notes-mode-map (kbd "C-c m e") #'mnemos-edit-note-at-point)
    (define-key mnemos-notes-mode-map (kbd "C-c m E") #'mnemos-edit-note-buffer)
    (define-key mnemos-notes-mode-map (kbd "C-c m d") #'mnemos-delete-note-at-point)
    (define-key mnemos-notes-mode-map (kbd "C-c m b") #'mnemos-show-backlinks)
    (define-key mnemos-notes-mode-map (kbd "C-c m x") #'mnemos-explain-region-ai)
    (define-key mnemos-notes-mode-map (kbd "C-c m X") #'mnemos-explain-region-ai-detailed)
    (define-key mnemos-notes-mode-map (kbd "C-c m R") #'mnemos-reattach-note)
    (define-key mnemos-notes-mode-map (kbd "C-c m S") #'mnemos-status)
    (define-key mnemos-notes-mode-map (kbd "C-c m s") #'mnemos-select-note)
    (define-key mnemos-notes-mode-map (kbd "C-c m <escape>") #'mnemos-clear-selection)
    (define-key mnemos-notes-mode-map (kbd "C-c m ?") #'mnemos-help)))

;; Ensure keymap is valid before defining the minor mode.
(mnemos--ensure-notes-mode-keymap)

(define-minor-mode mnemos-notes-mode
  "Minor mode for displaying and editing Mnemos notes (stickies) in code buffers."
  :lighter " Mnemos"
  :keymap mnemos-notes-mode-map
  (if mnemos-notes-mode
      (progn
        (mnemos--debug "mnemos-notes-mode: enabling in %s" (buffer-name))
        (mnemos--ensure-connection)
        (mnemos-refresh-notes)
        (add-hook 'post-self-insert-hook #'mnemos--maybe-trigger-link nil t))
    (mnemos--debug "mnemos-notes-mode: disabling in %s" (buffer-name))
    (remove-hook 'post-self-insert-hook #'mnemos--maybe-trigger-link t)
    (mnemos--clear-note-overlays)))

(defun mnemos--maybe-enable-notes ()
  "Enable `mnemos-notes-mode' in eligible buffers."
  (when (and buffer-file-name
             (derived-mode-p 'prog-mode))
    (mnemos-notes-mode 1)))

(define-globalized-minor-mode mnemos-notes-global-mode
  mnemos-notes-mode mnemos--maybe-enable-notes
  :group 'mnemos)

;; Do NOT enable global mode by default - users should opt in via their config:
;;   (mnemos-notes-global-mode 1)
;; This avoids unexpected backend connections and allows testing isolation.

(defun mnemos--ensure-notes-list-keymap ()
  "Ensure `mnemos-notes-list-mode-map` is a valid keymap with all bindings."
  (unless (keymapp mnemos-notes-list-mode-map)
    (setq mnemos-notes-list-mode-map (make-sparse-keymap))
    (set-keymap-parent mnemos-notes-list-mode-map special-mode-map))
  ;; Always ensure all keybindings are present
  (define-key mnemos-notes-list-mode-map (kbd "RET") #'mnemos-notes-list-visit)
  (define-key mnemos-notes-list-mode-map (kbd "v")   #'mnemos-view-note)
  (define-key mnemos-notes-list-mode-map (kbd "b")   #'mnemos-show-backlinks)
  (define-key mnemos-notes-list-mode-map (kbd "e")   #'mnemos-edit-note-at-point)
  (define-key mnemos-notes-list-mode-map (kbd "d")   #'mnemos-delete-note-at-point)
  (define-key mnemos-notes-list-mode-map (kbd "n")   #'mnemos-notes-list-next)
  (define-key mnemos-notes-list-mode-map (kbd "p")   #'mnemos-notes-list-prev)
  (define-key mnemos-notes-list-mode-map (kbd "q")   #'quit-window))

(defun mnemos-reset-keymaps-and-enable ()
  "Repair Mnemos keymaps after reloads and ensure global mode is enabled."
  (interactive)
  (mnemos--ensure-notes-mode-keymap)
  (mnemos--ensure-notes-list-keymap)
  (unless mnemos-notes-global-mode
    (mnemos-notes-global-mode 1)))

;; Fix up keymaps on reload (after defvars are in place) and ensure global mode.
(mnemos-reset-keymaps-and-enable)

(defvar mnemos-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mnemos-search-visit)
    map)
  "Keymap for `mnemos-search-results-mode'.")

(defun mnemos-search-visit ()
  "Visit the search hit on the current line."
  (interactive)
  (let* ((hit (get-text-property (line-beginning-position) 'mnemos-search-hit))
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

(define-derived-mode mnemos-search-results-mode special-mode "Mnemos-Search"
  "Mode for Mnemos search results."
  (setq buffer-read-only t))

(defun mnemos-help ()
  "Display Mnemos keybindings and help."
  (interactive)
  (let ((buf (get-buffer-create "*Mnemos Help*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize "Mnemos - A Second Brain for Your Code\n"
                          'face '(:weight bold :height 1.2)))
      (insert (make-string 40 ?-) "\n\n")
      (insert (propertize "Code Buffer (mnemos-notes-mode)\n"
                          'face '(:weight bold :underline t)))
      (insert "
  C-c m a    Add a note at point
  C-c m r    Refresh notes (reload from backend)
  C-c m l    List notes for current file
  C-c m e    Edit note at point
  C-c m E    Edit note in buffer (for longer notes)
  C-c m d    Delete note at point
  C-c m R    Reattach stale note to current position
  C-c m b    Show backlinks to note
  C-c m p    Index entire project
  C-c m f    Search project (files and notes)
  C-c m k    Insert a link to another note
  C-c m ]    Follow link at point
  C-c m [    Navigate back
  C-c m x    Explain region with AI
  C-c m X    Explain region with AI (detailed)
  C-c m S    Show status (note/file counts)
  C-c m s    Select note at cursor or from list
  C-c m ESC  Clear note selection
  C-c m ?    Show this help

")
      (insert (propertize "Notes List Buffer (*Mnemos Notes*)\n"
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
      (insert (propertize "Search Results Buffer (*Mnemos Search*)\n"
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
  M-x mnemos-open-project      Set project root
  M-x mnemos-explain-region    Explain selected code (requires LLM)
  M-x mnemos-shutdown          Stop Mnemos backend
  M-x mnemos-reload            Reload Mnemos (after code changes)
")
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

;;; Event handlers setup

(defun mnemos--find-buffer-by-path (file-path)
  "Find buffer visiting FILE-PATH, handling path canonicalization.
Server sends canonicalized paths (e.g., /private/tmp/foo on macOS)
but buffers may have non-canonicalized names (e.g., /tmp/foo)."
  ;; Try direct lookup first (fast path)
  (or (get-file-buffer file-path)
      ;; Compare canonical paths for all file-visiting buffers
      (let ((canonical-event (file-truename file-path)))
        (cl-find-if
         (lambda (buf)
           (when-let ((buf-file (buffer-file-name buf)))
             (string= (file-truename buf-file) canonical-event)))
         (buffer-list)))))

(defun mnemos--setup-event-handlers ()
  "Set up handlers for backend events."
  ;; Handle note position changes
  (mnemos--events-on "note-position-changed"
    (lambda (event)
      (let ((file (alist-get 'file event)))
        (when-let ((buf (mnemos--find-buffer-by-path file)))
          (with-current-buffer buf
            (mnemos-refresh-notes))))))

  ;; Handle note created
  (mnemos--events-on "note-created"
    (lambda (event)
      (let ((file (alist-get 'file event)))
        (when-let ((buf (mnemos--find-buffer-by-path file)))
          (with-current-buffer buf
            (mnemos-refresh-notes))))))

  ;; Handle note updated
  (mnemos--events-on "note-updated"
    (lambda (_event)
      ;; Full refresh since we don't know which buffer
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf) (buffer-live-p buf))
          (with-current-buffer buf
            (when mnemos--overlays
              (mnemos-refresh-notes)))))))

  ;; Handle note deleted
  (mnemos--events-on "note-deleted"
    (lambda (_event)
      ;; Full refresh since we don't know which buffer
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf) (buffer-live-p buf))
          (with-current-buffer buf
            (when mnemos--overlays
              (mnemos-refresh-notes))))))))

;; Initialize event handlers and start event client
(mnemos--setup-event-handlers)
(add-hook 'emacs-startup-hook #'mnemos--events-start)

(provide 'mnemos)

;;; mnemos.el ends here
