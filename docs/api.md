# API (experimental)

clojure-lsp is commonly used in a text editor, but it has its own API featuring the same features that can be used as a library or via CLI. An example is calling the CLI in a CI for cleaning the project namespaces.

## Usage

### CLI

`clojure-lsp --help` should show all available commands and options, clojure-lsp will consider the same rules of configuration, checking `.lsp/config.edn`.

At the moment the features available are:

`clean-ns` - clean the ns form removing unused requires/refers/imports, sorting the form.

### Library

The namespace [`clojure-lsp.api`](https://github.com/clojure-lsp/clojure-lsp/tree/master/src/clojure_lsp/api.clj) should be used as the entrypoint for available features as API, feel free to open a issue for missing features.
