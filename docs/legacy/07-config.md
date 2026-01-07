 # 7. Configuration

 ## 7.1 Global Configuration

 Example keys:

 - Engine type: `"ollama"`, `"llama-cpp"`, `"mlx"`.
 - Host/port for LLM runner.
 - Default model name.
 - Paths for logs and models.

 Stored in a file like:

 ```toml
 [engine]
 type = "llama-cpp"
 host = "127.0.0.1"
 port = 4010
 model_path = "/Users/you/Library/Application Support/Mnemos/models/qwen-32b-q4.gguf"

[paths]
 models_dir = "/Users/you/Library/Application Support/Mnemos/models"
 logs_dir = "/Users/you/Library/Application Support/Mnemos/logs"
 ```

 ## 7.2 Project Configuration

 Project-specific config:

 ```toml
 [project]
 root = "/path/to/project"
 name = "my-cool-project"

[index]
 include = ["src", "lib"]
 exclude = ["dist", "node_modules", ".git"]
 ```

 Used to tweak indexing and ignore noise.
