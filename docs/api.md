# API

clojure-lsp is commonly used in a text editor, but it has its own API featuring the main features that can be used as a library or via CLI.
This is a new feature and it's experimental, so things can break at any moment, so please open an issue if you find anything wrong.

## Usages

### CLI

`clojure-lsp --help` should show all available commands and options.

### Library

[![cljdoc](https://cljdoc.org/badge/com.github.clojure-lsp/clojure-lsp)](https://cljdoc.org/d/com.github.clojure-lsp/clojure-lsp/CURRENT/api/clojure-lsp.api)

The namespace [clojure-lsp.api](https://cljdoc.org/d/com.github.clojure-lsp/clojure-lsp/CURRENT/api/clojure-lsp.api) should be used as the entrypoint for all available features of the CLI and some specific for library only usage, feel free to open a issue for missing features.

Below is an example using the clean-ns feature on a `deps.edn` project:

```clojure
{:aliases 
  {:lint-ns {:replace-deps {com.github.clojure-lsp/clojure-lsp {:mvn/version "..."}}
             :exec-fn clojure-lsp.api/clean-ns!
             :exec-args {:dry? true}}}}
```

### CI

You can use [setup-clojure-lsp](https://github.com/marketplace/actions/setup-clojure-lsp) GitHub action to install clojure-lsp in a CI.

Remember to install your build tool, like `clojure` or `leiningen` as well in your CI according to your project to clojure-lsp be able to scan the classpath correctly.

### Leiningen

To run clojure-lsp from Leiningen easily, check [lein-clojure-lsp](https://github.com/clojure-lsp/lein-clojure-lsp) plugin.

### Babashka pod

It's possible to load clojure-lsp as a babashka pod giving access to the `clojure-lsp.api` namespace, Check babashka pod registry [example](https://github.com/babashka/pod-registry/blob/master/examples/clojure-lsp.clj).

## Settings

clojure-lsp will check for `.lsp/config.edn` in the project or home dir, but it's possible to force override the settings via the `:settings` option of the API or `--settings` option of the CLI.

For all available settings, check the [settings section documentation](https://clojure-lsp.io/settings/).
