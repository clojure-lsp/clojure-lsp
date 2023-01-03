# What is clojure-lsp API?

clojure-lsp is commonly used in a text editor during code development, but since it knows and have all necessary features to handle clojure code, it's the ideal/reliable tool to manage your code __outside the editor__ as well via multiple ways.

It has its own API containing the main features that can be used as: 

- [API (JVM)](./api.md): Use from your REPL or any other library that wants to leverage clojure-lsp features programatically.

- [CLI](./cli.md): Use from your terminal as a tool to format, clean, check diagnostics from clojure-lsp executable directly.

- [CI](./ci.md): Need to check if your code is formatted/clean/doesn't contain any lint errors after push? Use it in your CI.

- [Lein plugin](./lein-plugin.md): Use all CLI features but without the need to install it on your machine, using directly as a leiningen plugin.

- [Babashka pod](./bb-pod.md): Use clojure-lsp as a babashka pod for your bb program.


## Settings

clojure-lsp will check for `.lsp/config.edn` in the project or home dir, but it's possible to force override the settings via the `:settings` option of the API or `--settings` option of the CLI.

For all available settings, check the [settings documentation](settings.md).
