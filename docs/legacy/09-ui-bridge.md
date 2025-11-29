# 9. UI Bridge

The UI and Lisp backend talk over JSON-RPC.

## 9.1 Example Methods

- `hemis.open-project`
- `hemis.list-files`
- `hemis.get-file`
- `hemis.explain-region`
- `hemis.find-usages`
- `hemis.search`

## 9.2 Example Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "hemis.explain-region",
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
