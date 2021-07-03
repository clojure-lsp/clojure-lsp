# API

clojure-lsp is commonly used in a text editor, but it has its own API featuring the main features that can be used as a library or via CLI.
This is a new feature and it's experimental, so things can break at any moment, so please open an issue if find anything wrong.

## CLI

`clojure-lsp --help` should show all available commands and options.

## Library

The namespace [clojure-lsp.api](https://github.com/clojure-lsp/clojure-lsp/tree/master/src/clojure_lsp/api.clj) should be used as the entrypoint for the same available features of the CLI, feel free to open a issue for missing features.

## GitHub Action

You can use [setup-clojure-lsp](https://github.com/marketplace/actions/setup-clojure-lsp) GitHub action to install clojure-lsp in a CI.

## Usage

* `clean-ns`

Useful for cleaning the namespaces forms of your project, removing any unused required, import or refer, sorting and indenting correctly.

* `rename`

Rename a symbol and all its references across the project.

## Settings

clojure-lsp will check for `.lsp/config.edn` in the project or home dir, but it's possible to force override the settings via the `:settings` option of the API or `--settings` option of the CLI.

For all available settings, check the [settings section documentation](https://clojure-lsp.github.io/clojure-lsp/settings/).
