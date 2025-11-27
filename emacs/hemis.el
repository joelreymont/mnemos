;;; hemis.el --- Hemis: a second brain for your code  -*- lexical-binding: t; -*-

;; Emacs frontend for the Hemis Lisp backend (protocol v2).
;; - JSON-RPC 2.0 over stdio
;; - Notes minor mode with stickies as overlays
;; - Notes list buffer
;; - Project root awareness (for multi-file/project operations)

(require 'jsonrpc)
(require 'cl-lib)
(require 'project)

(defgroup hemis nil
  "Hemis – a second brain for your code."
  :group 'tools)

(defconst hemis--default-backend-script
  (expand-file-name "../hemis-project/backend/hemis.lisp"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Default path to the Hemis backend script bundled with this Emacs client.")

(defcustom hemis-executable "sbcl"
  "Path to the Hemis Lisp executable (or SBCL with --script)."
  :type 'string
  :group 'hemis)

(defcustom hemis-backend-script hemis--default-backend-script
  "Path to the Hemis backend script (JSON-RPC over stdio)."
  :type 'string
  :group 'hemis)

(defcustom hemis-executable-args nil
  "Arguments passed to `hemis-executable` when starting the server.
When nil, defaults to `(\"--script\" hemis-backend-script)`."
  :type '(choice (const :tag "Default" nil) (repeat string))
  :group 'hemis)

(defcustom hemis-log-buffer "*Hemis Log*"
  "Name of the buffer used to log Hemis backend output."
  :type 'string
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


;;; Process & JSON-RPC management

(defun hemis--start-process ()
  "Start the Hemis backend process if it is not already running."
  (unless (and hemis--process (process-live-p hemis--process))
    (let* ((buf (get-buffer-create hemis-log-buffer))
           (args (or hemis-executable-args
                     (list "--script" hemis-backend-script)))
           (proc (apply #'start-process
                        "hemis-backend" buf
                        hemis-executable args)))
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
  (jsonrpc-request hemis--conn method params))

(defun hemis--treesit-available-p ()
  "Return non-nil when Tree-sitter is available for the current mode."
  (and (fboundp 'treesit-node-at)
       (fboundp 'treesit-ready-p)
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
  (dolist (note notes)
    (hemis--make-note-overlay note)))


;;; Backend calls

(defun hemis--project-root ()
  "Return the project root for the current buffer, or nil."
  (when-let* ((proj (ignore-errors (project-current))))
    (condition-case nil
        (project-root proj)
      (error nil))))

(defun hemis--buffer-params ()
  "Return an alist describing the current buffer for the backend."
  (let ((file (or (buffer-file-name) (buffer-name)))
        (root (or (hemis--project-root) default-directory)))
    `((file . ,file)
      (projectRoot . ,root))))

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
         (params (append (hemis--buffer-params)
                         `((line . ,(line-number-at-pos))
                           (column . ,(current-column))
                           (text . ,text)
                           (tags . ,tags)
                           (nodePath . ,node-path))))
         (note   (hemis--request "notes/create" params)))
    (hemis--make-note-overlay note)
    (message "Hemis: note created.")))

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

(provide 'hemis)

;;; hemis.el ends here
