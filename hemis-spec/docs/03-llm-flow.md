# 3. LLM Flow

## 3.1 Input Construction

For each user action, Lisp:

1. Collects project context:
   - Current buffer and selection.
   - Surrounding lines for context.
   - Symbol definition and references from the index.
   - Related notes and backlinks.
2. Chooses a prompt template:
   - `explain-function`
   - `compare-two-implementations`
   - `trace-call-chain`
   - `summarise-subsystem`
3. Renders a `messages` array:
   - System: stable instruction for Hemis’ persona and boundaries.
   - Context: code snippets and summaries.
   - User: the user’s question or command.

## 3.2 Tool Schemas

Lisp provides the LLM with a list of tools, e.g.:

- `find-usages`
- `find-symbol-definition`
- `search-embedding`
- `open-file`
- `highlight-region`
- `get-ast-fragment`

Each tool includes a JSON schema so the LLM can format arguments.

## 3.3 Streaming and Tool Calls

The LLM responds with a stream of events:

- `token` — partial text to display.
- `tool_call` — request for a tool to be executed.
- `done` — completion of the response.

Lisp:
- For tokens: forwards them to the UI.
- For tool calls:
  - Executes the corresponding `hemis.tools` function.
  - Sends a follow-up “tool result” message back to the model.
- For completion:
  - Finalises the response and returns structured metadata (e.g., suggested notes, file locations).

## 3.4 Example Tool Call Exchange (Conceptual)

1. LLM:  
   `tool_call: { "name": "find-usages", "arguments": {"symbol": "parse-expression"} }`
2. Lisp:
   - Runs `hemis.index:find-usages`.
   - Returns results as `tool_result` message to LLM:
     - file paths
     - line/column ranges
3. LLM:
   - Incorporates the fetched usages into its explanation.
