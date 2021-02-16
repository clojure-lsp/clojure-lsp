# Settings 

`clojure-lsp` settings are picked up on server start and can be configured via 3 ways: 

- **Global configuration** 
- **project configuration**
- **LSP InitializationOptions**

## Project

`clojure-lsp` will look for project specific settings in a file called `.lsp/config.edn`. It will search from your project root folder up the directory structure so you can have multiple projects share the same settings.

Example:
```clojure
{:cljfmt {:indents {#re ".*" ns [[:inner 0] [:inner 1]]}}
 :auto-add-ns-to-new-files? false}
```

---
## Global

For global settings which should work for all the projects using `clojure-lsp`, you just need to add the same configs to `~/.lsp/config.edn`.

For an example of a global `config.edn`, check [here](https://github.com/ericdallo/dotfiles/blob/master/.lsp/config.edn).

---
## InitializationOptions

This is specific for an client, where it sends on startup, check [LSP spec for more information](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize).

This is an [example how Emacs `lsp-mode`](https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-clojure.el#L205) pass custom information.

## All settings

| name                            | description                                                                                                                                                                                                                                                                                                                                                                                                          | default           |
|---------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|
| `source-paths`                  | project-local directories to look for clj/cljc/cljs files                                                                                                                                                                                                                                                                                                                                                            | `#{"src" "test"}` |
| `ignore-classpath-directories`  | will not consider clojure files within the directories specified by your classpath. This is needed, for instance, if your build puts artifacts into `resources` or `target` that you want lsp to ignore.                                                                                                                                                                                                             | `false`           |
| `use-metadata-for-privacy?`     | Whether to use `^:private` metadata for refactorings instead of `defn-`                                                                                                                                                                                                                                                                                                                                              | `false`           |
| `keep-require-at-start?`        | Whether to keep first require/import at the first line instead of inserting a new line before it when using `clean-ns` refactoring.                                                                                                                                                                                                                                                                                  | `false`           |
| `semantic-tokens?`              | Whether to enable LSP semantic tokens server support for syntax highlighting.  (Experimental)                                                                                                                                                                                                                                                                                                                        | `false`           |
| `show-docs-arity-on-same-line?` | Whether to keep the arity on the same line of the function on hover, useful for Emacs users.                                                                                                                                                                                                                                                                                                                         | `false`           |
| `auto-add-ns-to-new-files?`     | Whether to automatically add the `ns` form in new blank files.                                                                                                                                                                                                                                                                                                                                                       | `true`            |
| `document-formatting?`          | if true or not present, document formatting is provided.                                                                                                                                                                                                                                                                                                                                                             | `true`            |
| `document-range-formatting?`    | if true or not present, document range formatting is provided.                                                                                                                                                                                                                                                                                                                                                       | `true`            |
| `dependency-scheme`             | How the dependencies should be linked, `jar` will make urls compatible with java's JarURLConnection. You can have the client make an lsp extension request of `clojure/dependencyContents` with the jar uri and the server will return the jar entry's contents. [Similar to java clients](https://github.com/redhat-developer/vscode-java/blob/a24945453092e1c39267eac9367c759a6c7b0497/src/extension.ts#L290-L298) | `zip`             |
| `cljfmt`                        | Used for formatting, json encoded configuration for [cljfmt](https://github.com/weavejester/cljfmt)                                                                                                                                                                                                                                                                                                                  | `{}`              |
| `project-specs`                 | A vector containing a map of key/value pairs, defining how `clojure-lsp` should find your project classpath                                                                                                                                                                                                                                                                                                          | Check the default [here](https://github.com/clojure-lsp/clojure-lsp/blob/master/src/clojure_lsp/crawler.clj#L53-L60)                  |
| `sqlite-db-path`                | Where to store the project's analysis cache, used to speed up next `clojure-lsp` startup. A path relative to project root or an absolute path. When not present, defaults to `.lsp/sqlite.db`

### Lint

`clojure-lsp` uses [clj-kondo](https://github.com/clj-kondo/clj-kondo) to lint the code and retrieve the analysis to
make most of features work, make sure you have it properly configured in your `.clj-kondo/config.edn` file, for more information about available configurations, 
check the [clj-kondo configuration section](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md)
