# 9. UI Bridge

The UI and Lisp backend talk over JSON-RPC.

## 9.1 Example Methods

- `mnemos.open-project`
- `mnemos.list-files`
- `mnemos.get-file`
- `mnemos.explain-region`
- `mnemos.find-usages`
- `mnemos.search`

## 9.2 Example Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "mnemos.explain-region",
  "params": {
    "file": "src/main.lisp",
    "start": {"line": 10, "column": 0},
    "end": {"line": 40, "column": 0}
  }
}
```

## 9.3 Example Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "explanation": "This function initialises the server and sets up routes...",
    "references": [
      {"file": "src/routes.lisp", "line": 12},
      {"file": "src/config.lisp", "line": 5}
    ]
  }
}
```
