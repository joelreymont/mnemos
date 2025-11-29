# 5. Tools

Tools are the bridge between the LLM and your actual capabilities.

## 5.1 Design Goals

- Keep tools **small and composable**.
- Expose **semantic operations**:
  - “Find usages of symbol X”.
  - “Get AST fragment for this region”.
  - “Search for code related to Y”.
- Keep them **stateless** where possible; state lives in the index.

## 5.2 Example Tool Set

### Code Understanding

- `find-symbol-definition`
- `find-usages`
- `list-children` (methods in a class, functions in a module)
- `get-ast-fragment`
- `resolve-import`

### Semantic Navigation

- `search-embedding`
- `find-related-notes`
- `cluster-functions`
- `suggest-refactor-targets`

### UI Integration

- `open-file`
- `highlight-region`
- `show-note-panel`
- `create-note`

## 5.3 Tool Implementation Shape (Lisp)

A tool is a function from JSON arguments to JSON results.

```lisp
(defun tool-find-usages (args)
  (let* ((symbol (gethash "symbol" args))
         (results (hemis.index:find-usages symbol)))
    `(("status" . "ok")
      ("results" . ,results))))
```

A dispatcher maps tool names to these functions:

```lisp
(defun dispatch-tool (name args)
  (ecase name
    ("find-usages" (tool-find-usages args))
    ("find-symbol-definition" (tool-find-symbol-definition args))
    ;; ...
    ))
```
