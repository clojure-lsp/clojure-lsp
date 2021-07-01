# API (experimental)

clojure-lsp is commonly used in a text editor, but it has its own API featuring the main features that can be used as a library or via CLI. An example is calling the CLI in a CI for cleaning the project namespaces.

## Usage

### CLI

`clojure-lsp --help` should show all available commands and options.

At the moment the features available are:

`clean-ns` - clean the ns form removing unused requires/refers/imports, sorting the form.
`rename` - rename a symbol and all references across the project, use --from and --to options.

### Library

The namespace [`clojure-lsp.api`](https://github.com/clojure-lsp/clojure-lsp/tree/master/src/clojure_lsp/api.clj) should be used as the entrypoint for available features as API, feel free to open a issue for missing features.

## Settings

clojure-lsp will check for `.lsp/config.edn` in the project or home dir, but it's possible to force override the settings via the `:settings` option of the API or `--settings` option of the CLI.
