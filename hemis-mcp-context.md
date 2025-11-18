\
# Hemis / Lauri – Local MCP + Lisp Setup (Conversation Context)

This file captures the **essential context and decisions** from our conversation about building an AI-driven interactive development environment for Hemis / Lauri using **Common Lisp**, **MCP**, and **Codex**, plus the final MCP server implementation we converged on.

---

## 1. Overall Goal

You want to experiment with **interactive development with AI** in a way that:

- Avoids long compilation cycles (e.g., Rust).
- Lets an AI (Codex / ChatGPT-like model) **directly drive a running system**.
- Is especially suited for building your tools:
  - **Lauri**
  - **Hemis: a second brain for your codebase**

Key idea: use **Common Lisp + REPL** as the “live brain” and connect it to an MCP client so the AI can:

- Evaluate code in a running image.
- Reload systems and files.
- Manipulate project files.
- Reload UI logic live.

---

## 2. Language & Architecture Choices

### Common Lisp vs Rust / compiled languages

We concluded that Lisp is much better aligned with **AI-driven, highly interactive dev** than Rust, because:

- Lisp has:
  - A live **REPL**.
  - Fast incremental compilation.
  - Hot code redefinition (functions, classes, macros) in a running process.
- The AI can iterate in a loop:
  1. Edit a small piece of code.
  2. Evaluate / reload just that piece.
  3. Observe behavior.
  4. Refine.

Rust, by contrast:

- Has stronger safety and performance, but:
  - Compile times.
  - Strict type system and borrow checker friction.
  - Less natural for “let the AI experiment constantly”.
- Great for **polished, deployed artifacts**, but not ideal as the primary “AI playground” layer.

**Hybrid strategy** we discussed:

- Prototype and explore in Lisp (with AI driving via MCP).
- Later, move stable, performance-critical pieces into Rust or Swift if desired.

### Codex vs ChatGPT for the MCP client

We decided **not** to expose a powerful REPL-like MCP endpoint to the public internet (ChatGPT) because:

- It’s effectively **remote code execution** on your machine.
- Even with auth, that’s a big attack surface.

Instead, we chose to:

- Use **Codex CLI / Codex in VS Code** as the MCP client.
- Keep everything **local on your laptop**:
  - No tunnels.
  - No public HTTPS endpoints.
  - No external auth.
- This allows the MCP server to be **“simple and all-powerful”** safely, because only you (locally) can access it.

Resulting architecture:

```text
Codex (CLI / VS Code)
   ↕ MCP over stdio
Lisp MCP server (SBCL)
   ↳ Jonathan for JSON
   ↳ Slynk server for Emacs/SLY
   ↳ Your Hemis / Lauri code
```

---

## 3. Jonathan vs cl-json

For the MCP server’s JSON handling, we compared **cl-json** and **Jonathan**:

- **cl-json**
  - Older, widely used, solid.
  - Fine for small control-plane use.
  - More “legacy” feel and less optimized.

- **Jonathan**
  - More modern, designed for **speed and efficiency**.
  - Better suited to parsing frequent protocol messages.
  - Good default choice for a JSON-heavy MCP server.

We chose **Jonathan** as the JSON library and wrapped it in small helpers (`json-decode`, `json-encode`, `jget`) so it’s easy to swap or adjust later.

---

## 4. Slynk Integration

You want to:

- Run **Emacs with SLY** against the same Lisp image.
- Have **SLY + MCP** attached to one process, so you can:
  - Inspect and manipulate state manually via SLY.
  - Let Codex perform higher-level operations via tools.

So the SBCL process should:

- Start **Slynk** on some port (e.g. `4008`).
- Then enter the MCP stdio loop, serving Codex.

You connect from Emacs via:

- `M-x sly-connect`
- Host: `127.0.0.1`
- Port: `4008` (or whatever you configured).

---

## 5. MCP Tools – First Batch & Design

We designed a **first batch of MCP tools** that support your workflow:

### 5.1 Generic dev tools

1. `lisp_eval`
   - Args: `{ "form": string }`
   - Evaluates a single Lisp form in the running image.
   - Returns printed values as text.
   - Useful for:
     - Small REPL experiments.
     - Letting Codex probe the environment.

2. `lisp_describe`
   - Args: `{ "symbol": string }`
   - Calls `DESCRIBE` on a symbol (e.g., `CL:MAPCAR`, `HEMIS:NOTE`).
   - Useful for introspection, teaching the model about the API surface.

### 5.2 Filesystem tools (for code editing)

3. `fs_read_text`
   - Args: `{ "path": string }`
   - Reads a UTF-8 text file.
   - Lets Codex read `.lisp` files and other text assets.

4. `fs_write_text`
   - Args: `{ "path": string, "content": string }`
   - Writes a UTF-8 text file (overwrite / create).
   - Lets Codex update source files after editing them.

With these two, Codex can do the classic loop:
1. `fs_read_text` a file.
2. Edit it in context.
3. `fs_write_text` the new version.
4. Call a reload tool (`hemis_reload_file` or `hemis_reload_system`).

### 5.3 Hemis / Lauri tools

These assume you’ll define the underlying functions in your Hemis/Lauri codebase (e.g. `HEMIS:RELOAD-UI`).

5. `hemis_reload_system`
   - Args: `{ "system": string }` (e.g. `"hemis"`).
   - Calls `ASDF:LOAD-SYSTEM` with `:force t`.
   - Use for bigger structural changes across many files.

6. `hemis_reload_file`
   - Args: `{ "path": string }`.
   - Calls `(LOAD path)`.
   - Use for quick single-file reloads.

7. `hemis_reload_ui`
   - Args: `{}`.
   - Calls `(HEMIS:RELOAD-UI)` or identical UI-refresh entry point.
   - Use after UI/layout changes.

8. `hemis_apply_patch` (stub)
   - Args: `{ "path": string, "patch": string }`.
   - Intended to apply a unified diff patch to a file and then reload it.
   - Currently left as a stub; you’ll implement actual patching logic later.
   - Once implemented, it becomes a safer and more structured way for Codex to modify files than “freeform write text”.

---

## 6. Final MCP + Slynk Server Implementation (Lisp)

This is the **single-file implementation** we converged on. It:

- Uses **Jonathan** for JSON.
- Implements MCP methods `tools/list` and `tools/call` over **stdio**.
- Sets up a **tool registry** with `defmcp-tool` for easy extension.
- Starts **Slynk** so you can attach with SLY.
- Implements the first batch of tools listed above.

File: `mcp-lisp-server.lisp`

```lisp
;;;; mcp-lisp-server.lisp
;;;; MCP server (stdio) + Slynk in one Lisp image, using Jonathan for JSON.

(defpackage :mcp-lisp
  (:use :cl)
  (:export :main))
(in-package :mcp-lisp)

;;; --------------------------------------------------------------------------
;;; 0. Quicklisp + deps
;;; --------------------------------------------------------------------------

(defun ensure-quicklisp ()
  (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                (user-homedir-pathname))))
    (when (probe-file setup)
      (load setup))))

(ensure-quicklisp)

(ql:quickload '(:jonathan :slynk))

;;; --------------------------------------------------------------------------
;;; 1. JSON helpers (Jonathan)
;;; --------------------------------------------------------------------------

(defun json-decode (string)
  "Decode JSON string → Lisp (alist/hash etc., depending on Jonathan config)."
  (jonathan:parse string))

(defun json-encode (obj)
  "Encode Lisp structure → JSON string."
  (jonathan:to-json obj))

(defun jget (object key)
  "Get value for KEY (string) from an alist / hash with string keys."
  (etypecase object
    (hash-table (gethash key object))
    (list (cdr (assoc key object :test #'string=)))))

;;; --------------------------------------------------------------------------
;;; 2. Tool registry + macro
;;; --------------------------------------------------------------------------

(defstruct tool
  name          ;; string
  description   ;; string
  schema        ;; Lisp object representing JSON Schema
  handler)      ;; function: plist-of-args → result plist/string

(defparameter *tools* (make-hash-table :test 'equal))

(defun register-tool (name description schema handler)
  (setf (gethash name *tools*)
        (make-tool :name name
                   :description description
                   :schema schema
                   :handler handler)))

(defmacro defmcp-tool (name (args-sym) docstring schema &body body)
  "Define and register an MCP tool.
NAME       – string, tool name exposed to MCP clients.
ARGS-SYM   – symbol, plist of keyword args.
DOCSTRING  – string, human description.
SCHEMA     – Lisp structure describing JSON Schema for inputs.
BODY       – should return either a string or a plist describing result."
  (let ((fn (gensym "TOOL-FN-")))
    `(let ((,fn (lambda (,args-sym)
                  (declare (ignorable ,args-sym))
                  ,@body)))
       (register-tool ,name ,docstring ,schema ,fn))))

;;; --------------------------------------------------------------------------
;;; 3. Core dev tools: lisp_eval, lisp_describe
;;; --------------------------------------------------------------------------

(defmcp-tool
  "lisp_eval" (args)
  "Evaluate a Common Lisp form in this image. Args: {\"form\": string}."
  '(:type "object"
    :properties (("form" . (:type "string"
                           :description "A single Lisp form, e.g. (print 42).")))
    :required ("form"))
  (let ((form-string (getf args :form)))
    (handler-case
        (let* ((form (read-from-string form-string))
               (values-list (multiple-value-list (eval form)))
               (printed (with-output-to-string (out)
                          (loop for v in values-list
                                for i from 0
                                do (progn
                                     (when (> i 0) (write-char #\Space out))
                                     (prin1 v out))))))
          ;; Convention: return plist with :ok and :text
          (list :ok t :text printed))
      (error (e)
        (list :ok nil :text (princ-to-string e))))))

(defmcp-tool
  "lisp_describe" (args)
  "Describe a symbol. Args: {\"symbol\": string}."
  '(:type "object"
    :properties (("symbol" . (:type "string"
                             :description "Symbol name, e.g. CL:MAPCAR.")))
    :required ("symbol"))
  (let ((sym-name (getf args :symbol)))
    (handler-case
        (let* ((sym (read-from-string sym-name))
               (text (with-output-to-string (out)
                       (describe sym out))))
          (list :ok t :text text))
      (error (e)
        (list :ok nil :text (princ-to-string e))))))

;;; --------------------------------------------------------------------------
;;; 4. Filesystem tools (for code editing via Codex)
;;; --------------------------------------------------------------------------

(defmcp-tool
  "fs_read_text" (args)
  "Read a UTF-8 text file. Args: {\"path\": string}."
  '(:type "object"
    :properties (("path" . (:type "string"
                            :description "Relative or absolute file path.")))
    :required ("path"))
  (let* ((path (getf args :path))
         (truename (truename path)))
    (handler-case
        (with-open-file (in truename :direction :input :external-format :utf-8)
          (let ((contents (make-string (file-length in))))
            (read-sequence contents in)
            (list :ok t :text contents)))
      (error (e)
        (list :ok nil :text (format nil "fs_read_text error (~A): ~A" truename e))))))

(defmcp-tool
  "fs_write_text" (args)
  "Write a UTF-8 text file (overwrite). Args: {\"path\": string, \"content\": string}."
  '(:type "object"
    :properties (("path"    . (:type "string"))
                 ("content" . (:type "string")))
    :required ("path" "content"))
  (let* ((path (getf args :path))
         (content (getf args :content)))
    (handler-case
        (progn
          (with-open-file (out path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
            (write-string content out))
          (list :ok t :text (format nil "Wrote ~A (~D chars)." path (length content))))
      (error (e)
        (list :ok nil :text (format nil "fs_write_text error (~A): ~A" path e))))))

;;; --------------------------------------------------------------------------
;;; 5. Hemis / Lauri tools (first batch)
;;; --------------------------------------------------------------------------

(defmcp-tool
  "hemis_reload_system" (args)
  "Reload an ASDF system, e.g. \"hemis\". Args: {\"system\": string}."
  '(:type "object"
    :properties (("system" . (:type "string"
                              :description "System name, e.g. \"hemis\".")))
    :required ("system"))
  (let* ((system-name (getf args :system)))
    (handler-case
        (progn
          (asdf:load-system system-name :force t)
          (list :ok t :text (format nil "Reloaded ASDF system ~A." system-name)))
      (error (e)
        (list :ok nil :text (format nil "hemis_reload_system(~A) error: ~A"
                                    system-name e))))))

(defmcp-tool
  "hemis_reload_file" (args)
  "Reload a single Lisp file with LOAD. Args: {\"path\": string}."
  '(:type "object"
    :properties (("path" . (:type "string"
                            :description "File path to LOAD.")))
    :required ("path"))
  (let ((path (getf args :path)))
    (handler-case
        (progn
          (load path)
          (list :ok t :text (format nil "Loaded ~A." path)))
      (error (e)
        (list :ok nil :text (format nil "hemis_reload_file(~A) error: ~A" path e))))))

(defmcp-tool
  "hemis_reload_ui" (args)
  "Reload Hemis/Lauri UI (calls HEMIS:RELOAD-UI). Args: {}."
  '(:type "object" :properties () :required ())
  (declare (ignore args))
  (handler-case
      (progn
        (funcall (intern "RELOAD-UI" "HEMIS"))
        (list :ok t :text "Hemis UI reloaded."))
    (error (e)
      (list :ok nil :text (format nil "hemis_reload_ui error: ~A" e))))))

(defmcp-tool
  "hemis_apply_patch" (args)
  "Apply a unified diff patch to a file and reload it.
Args: {\"path\": string, \"patch\": string}. (PATCH logic is a stub for now.)"
  '(:type "object"
    :properties (("path"  . (:type "string"))
                 ("patch" . (:type "string"
                             :description "Unified diff patch text.")))
    :required ("path" "patch"))
  (let ((path  (getf args :path))
        (patch (getf args :patch)))
    ;; TODO: implement actual patch application.
    (declare (ignore patch))
    (list :ok nil :text
          (format nil "hemis_apply_patch stub: path=~A. Implement patch logic." path))))

;;; --------------------------------------------------------------------------
;;; 6. MCP protocol: tools/list, tools/call
;;; --------------------------------------------------------------------------

(defun tool->json (tool)
  "Convert internal TOOL struct to MCP tool description (alist)."
  (list
   "name"        (tool-name tool)
   "description" (tool-description tool)
   "inputSchema" (tool-schema tool)))

(defun list-tools-result ()
  "Result payload for tools/list."
  (let ((tools '()))
    (maphash
     (lambda (_name tool)
       (declare (ignore _name))
       (push (tool->json tool) tools))
     *tools*)
    (list "tools" (nreverse tools))))

(defun make-json-response (id result &key error)
  "Wrap result/error in JSON-RPC-like response with correct shape."
  (let ((base (list "jsonrpc" "2.0"
                    "id"      id)))
    (json-encode
     (cond
       (error
        (append base (list "error" error)))
       (t
        (append base (list "result" result)))))))

(defun plist->text-content (plist)
  "Convert our result plist (e.g., (:ok t :text \"...\"))
   into MCP content: [{\"type\":\"text\",\"text\":...}]."
  (let ((text (or (getf plist :text)
                  (with-output-to-string (out)
                    (pprint plist out)))))
    (list (list "type" "text"
                "text" text))))

(defun call-tool-result (params)
  "Handle tools/call. PARAMS is decoded JSON (alist/hash)."
  (let* ((name   (jget params "name"))
         (args   (or (jget params "arguments") '()))
         (tool   (gethash name *tools*)))
    (if (null tool)
        (list "content"
              (list (list "type" "text"
                          "text" (format nil "Unknown tool ~A" name))))
        (let* ((plist-args
                 (loop for (k . v) in args
                       append (list (intern (string-upcase k) :keyword) v)))
               (raw-result (funcall (tool-handler tool) plist-args))
               (content    (plist->text-content
                            (if (stringp raw-result)
                                (list :ok t :text raw-result)
                                raw-result))))
          (list "content" content)))))

(defun handle-request (req)
  "REQ is decoded JSON (alist/hash)."
  (let* ((id     (jget req "id"))
         (method (jget req "method"))
         (params (or (jget req "params") '())))
    (cond
      ((string= method "tools/list")
       (make-json-response id (list-tools-result)))
      ((string= method "tools/call")
       (let ((res (call-tool-result params)))
         (make-json-response id res)))
      (t
       (make-json-response id nil
                           :error (list "code" -32601
                                        "message" (format nil "Method ~A not found" method)))))))

;;; --------------------------------------------------------------------------
;;; 7. MCP stdio main loop
;;; --------------------------------------------------------------------------

(defun mcp-main-loop ()
  "Run MCP server over stdio. One JSON request per line (simplified)."
  (loop
    (handler-case
        (let ((line (read-line *standard-input* nil nil)))
          (when (null line)
            (return))
          (let* ((req   (json-decode line))
                 (resp  (handle-request req)))
            (write-string resp *standard-output*)
            (write-char #\Newline *standard-output*)
            (finish-output *standard-output*)))
      (error (e)
        (format *error-output* "MCP error: ~A~%" e)))))

;;; --------------------------------------------------------------------------
;;; 8. Slynk integration
;;; --------------------------------------------------------------------------

(defun start-slynk (&key (port 4008) (dont-close t))
  "Start a Slynk server so SLY can connect via M-x sly-connect."
  (format *error-output* "Starting Slynk on port ~A~%" port)
  (funcall (intern "CREATE-SERVER" "SLYNK")
           :port port
           :dont-close dont-close))

;;; --------------------------------------------------------------------------
;;; 9. Entry point
;;; --------------------------------------------------------------------------

(defun main ()
  "Entry point for Codex MCP. Starts Slynk, then MCP stdio loop."
  (start-slynk :port 4008 :dont-close dont-close)
  (mcp-main-loop))
```

---

## 7. Codex & SLY Integration Summary

- **Codex** MCP config (`~/.codex/config.toml`):

  ```toml
  [mcp.lisp-repl]
  command = "sbcl"
  args = ["--load", "/ABS/PATH/mcp-lisp-server.lisp", "--eval", "(mcp-lisp:main)"]
  ```

- **Emacs/SLY**:
  - `M-x sly-connect`
  - Host: `127.0.0.1`
  - Port: `4008`

From there, you can:

- Let Codex:
  - Use `fs_read_text` / `fs_write_text` to edit Hemis/Lauri source files.
  - Call `hemis_reload_file` / `hemis_reload_system` / `hemis_reload_ui` to apply changes.
  - Use `lisp_eval` / `lisp_describe` for introspection and quick experiments.

- Use SLY:
  - For direct REPL hacking, debugging, stepping, inspection.

This file should be enough context to drop straight into a repo and also feed to Codex/ChatGPT as “project context” for future iterations.
