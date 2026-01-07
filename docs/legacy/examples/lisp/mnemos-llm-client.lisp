(defpackage :mnemos.llm
  (:use :cl)
  (:import-from :jonathan :to-json :parse)
  (:export :call-llm))

(in-package :mnemos.llm)

(defun call-llm (messages &key tools (model "qwen-32b-q4") (temperature 0.1))
  "Send a chat completion request to the LLM runner.
MESSAGES is a list of plist objects with :role and :content."
  (let* ((payload `(("model" . ,model)
                    ("messages" . ,messages)
                    ("tools" . ,tools)
                    ("temperature" . ,temperature)))
         (json (to-json payload)))
    ;; Replace this with dexador/usocket call.
    (declare (ignore json))
    (error "call-llm: HTTP client not wired yet.")))
