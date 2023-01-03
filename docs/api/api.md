# API (JVM)

[![cljdoc](https://cljdoc.org/badge/com.github.clojure-lsp/clojure-lsp)](https://cljdoc.org/d/com.github.clojure-lsp/clojure-lsp/CURRENT/api/clojure-lsp.api)

The namespace [clojure-lsp.api](https://cljdoc.org/d/com.github.clojure-lsp/clojure-lsp/CURRENT/api/clojure-lsp.api) should be used as the entrypoint for all available features of the CLI and some specific for library only usage, feel free to open a issue for missing features.

Below is an example using the clean-ns feature on a `deps.edn` project:

```clojure
{:aliases 
  {:lint-ns {:replace-deps {com.github.clojure-lsp/clojure-lsp {:mvn/version "..."}}
             :exec-fn clojure-lsp.api/clean-ns!
             :exec-args {:dry? true}}}}
```



