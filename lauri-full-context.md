# Lauri & Hemis – Full Context and Architecture Document

This document consolidates **all context** from our discussions about:

- **Lauri** — a Tauri-style desktop runtime powered by Common Lisp  
- **Hemis** — “a second brain for your codebase,” built on top of Lauri  
- Architecture, packaging, build flow, frontend/backend structure, and naming

It is intended as a self-contained reference you can store in your project repo.

---

# 1. Naming, Vision, and Roles

## Hemis
**Hemis** is the product:

> **Hemis: A second brain for your codebase.**

You described it as a Mac-first developer tool with:
- Deep code understanding  
- Intent-aware search  
- Code browsing  
- Notes tied to functions, modules, files  
- Backlinks  
- Visual UI  
- Fast incremental indexing  

You want a beautiful, modern UI — React-level polish — combined with a powerful Lisp engine for intelligent operations.

## Lauri
**Lauri** is the runtime / shell / app-hosting framework that powers Hemis.

It is conceptually:

> **Tauri, but powered by Common Lisp.**

The name works because:
- Short, clean, Finnish-sounding  
- Evokes “Lisp” + “Tauri” → **Lauri**  
- Sounds like a friendly assistant daemon  
- Easy branding for CL developers  

The idea: a reusable Lisp/WebView shell that future apps (including Hemis) can be built on.

---

# 2. What Tauri Does, and What Lauri Mirrors

Tauri provides:
1. Tiny native backend (Rust)
2. WebView frontend (WebKit/GTK/Edge)
3. IPC bridge (JS <-> Rust)
4. Packaging, signing, updating
5. Security model & runtime

**Lauri mirrors this:**

| Tauri Component | Lauri Equivalent |
|-----------------|------------------|
| Rust backend | Common Lisp backend |
| WebView UI | cl-webkit WebView window |
| Commands / IPC | JSON RPC or JS bridge |
| Bundling into `.app` | SBCL binary + .app structure |
| Runtime lifecycle | Lisp-controlled main loop |

The key difference is **interactive development**:  
Lisp + REPL + Slynk → you can modify the app while it runs.

---

# 3. Lauri Architecture (Full Sketch)

Lauri consists of four pieces:

---

## 3.1 Backend (Common Lisp)

Responsibilities:
- App startup  
- HTTP/WebSocket/bridge server  
- Commands such as:  
  - `index-project`  
  - `search`  
  - `open-file`  
  - `render-markdown`  
- Persistent storage  
- Code intelligence logic  
- Any AI interactions  

Core libraries:
- HTTP: `hunchentoot` or `woo`
- JSON: `jonathan` (recommended)
- WebView: `cl-webkit` (macOS/Linux)
- Path mgmt: `uiop`

---

## 3.2 Frontend (Web UI)

Modern UI stack:
- Vite
- React or Svelte
- Tailwind
- shadcn/ui (optional)
- Code rendering, Tree-sitter, etc.

Frontend assets live in:
```
static/
  index.html
  assets/
    main.js
    styles.css
```

UI responsibilities:
- Render code
- Show notes
- Trigger backend commands
- Use HMR during development (via Vite dev server)
- Production build → bundled static files

---

## 3.3 IPC Layer (Backend ↔ Frontend)

Two modes:

### **Mode A: HTTP JSON RPC (simple)**
Frontend calls:
```js
await fetch("/invoke", {
  method: "POST",
  body: JSON.stringify({ cmd: "ping", args: {...} }),
});
```

Backend defines `/invoke` route and dispatches to functions.

### **Mode B: Direct JS Bridge (advanced)**
WebKit allows evaluating JS from Lisp and receiving callbacks.

Example API:
```js
window.lauri.invoke("index-project", { path });
```

Backend:
- Implements a JS callback handler  
- Maps commands to Lisp functions  

This mode is cleaner long-term.

---

## 3.4 App Wrapper

Lisp `main` must:
1. Find the app resource directory
2. Start the backend server
3. Create the WebView window
4. Load the frontend bundle
5. Enter the GUI event loop
6. Stop server on exit

---

# 4. Detailed Backend Example Code

## 4.1 ASDF System

```lisp
(defsystem "lauri"
  :depends-on (:hunchentoot :jonathan :cl-webkit :uiop)
  :serial t
  :components
  ((:file "package")
   (:file "server")
   (:file "ui")
   (:file "main")))
```

## 4.2 JSON-RPC HTTP server

```lisp
(in-package :lauri)

(defparameter *port* 47832)
(defparameter *server* nil)
(defparameter *static-root* nil)

(defun start-server ()
  (setf *server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port *port*
                        :document-root *static-root*))))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)))

(hunchentoot:define-easy-handler (invoke-api :uri "/invoke") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (hunchentoot:raw-post-data :force-binary t))
         (json (jonathan:parse (babel:octets-to-string body :encoding :utf-8)))
         (command (gethash "cmd" json))
         (args    (gethash "args" json)))
    (jonathan:to-json (handle-command command args))))
```

## 4.3 Command dispatcher

```lisp
(defun handle-command (cmd args)
  (case (intern (string-upcase cmd) :keyword)
    (:PING (list :ok t :message "pong"))
    (:INDEX-PROJECT (index-project args))
    (t (list :ok nil :error (format nil "Unknown command ~A" cmd)))))
```

---

# 5. WebView Integration (macOS)

Concept:

```lisp
(defun start-ui ()
  (let* ((url (format nil "http://127.0.0.1:~D/index.html" *port*)))
    (cl-webkit:with-webview (view :title "Hemis" :width 1200 :height 800)
      (cl-webkit:load-uri view url)
      (cl-webkit:run view))))
```

---

# 6. Lauri Entrypoint

```lisp
(defun main ()
  (setf *static-root* (app-resources-static-path))
  (start-server)
  (unwind-protect
       (start-ui)
    (stop-server)))
```

---

# 7. Finding App Resources at Runtime

To bundle `static/` inside a macOS `.app`, Lisp needs to locate:

```
Hemis.app/Contents/Resources/static/
```

### Compute path from the running binary:

```lisp
(defun app-resources-path ()
  (let* ((exe (truename (nth 0 sb-ext:*posix-argv*)))
         (macos-dir (uiop:pathname-directory-pathname exe))
         (contents-dir (uiop:pathname-directory-pathname macos-dir)))
    (merge-pathnames "Resources/" contents-dir)))
```

### Static path:

```lisp
(defun app-resources-static-path ()
  (merge-pathnames "static/" (app-resources-path)))
```

---

# 8. macOS Packaging Flow (Complete)

## 8.1 Build binary

Create `build.lisp`:

```lisp
(load "quicklisp/setup.lisp")
(ql:quickload :lauri)
```

Build:

```bash
buildapp   --load build.lisp   --entry lauri:main   --output hemis
```

This produces a self-contained binary.

---

## 8.2 Create `.app` structure

```
mkdir -p Hemis.app/Contents/MacOS
mkdir -p Hemis.app/Contents/Resources
```

Copy files:

```
cp hemis Hemis.app/Contents/MacOS/
cp -r static Hemis.app/Contents/Resources/
cp AppIcon.icns Hemis.app/Contents/Resources/
```

---

## 8.3 Info.plist

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
 "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>Hemis</string>

  <key>CFBundleIdentifier</key>
  <string>com.yourcompany.hemis</string>

  <key>CFBundleExecutable</key>
  <string>hemis</string>

  <key>CFBundlePackageType</key>
  <string>APPL</string>

  <key>CFBundleIconFile</key>
  <string>AppIcon</string>

  <key>LSMinimumSystemVersion</key>
  <string>11.0</string>
</dict>
</plist>
```

---

## 8.4 Codesigning (optional but recommended)

```
codesign --deep --force --sign "Developer ID Application: YOUR NAME" Hemis.app
```

---

# 9. Hemis on Top of Lauri

Hemis is built on Lauri:

- Lauri handles:  
  - Window  
  - Runtime  
  - IPC  
  - Packaging  
  - Backend server  
- Hemis adds:  
  - Code analysis  
  - Tree-sitter integration  
  - Indexing  
  - Notes  
  - Backlinks  
  - Code browsing UI  

In other words:

> **Lauri is the engine; Hemis is the car.**

---

# 10. Next Steps You Can Do

- Create repo `lauri/`
- Add `lauri.asd`
- Add `src/{package,server,ui,main}.lisp`
- Add `frontend/` with Vite template
- Add `packaging/macos/Info.plist`
- Automate `lauri dev` and `lauri build` CLI tools

---

# End of Document
