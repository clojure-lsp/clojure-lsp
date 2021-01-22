# Settings 

Custom `clojure-lsp` settings can be configured via 3 ways: **Global configuration**, **project configuration** or **LSP InitializationOptions**.

## Supported settings

`source-paths` value is a set of project-local directories to look for clj/cljc/cljs files. Default is `#{"src", "test"}`.

`ignore-classpath-directories` if true, will not consider clojure files within the directories specified by your classpath. This is needed, for instance, if your build puts artifacts into `resources` or `target` that you want lsp to ignore.

`use-metadata-for-privacy?` if true, will use `^:private` metadata for refactorings instead of `defn-`

`keep-require-at-start?` if true, will keep first require/import at the first line instead of inserting a new line before it when using `clean-ns` refactoring.

`semantic-tokens?` if true or not present, will enable LSP semantic tokens server support for syntax highlighting. 

`show-docs-arity-on-same-line?` if true, will keep the arity on the same line of the function on hover, useful for Emacs users.

`auto-add-ns-to-new-files?` if true or not present, will automatically add the `ns` form in new files. 

`document-formatting?` if true or not present, document formatting is provided.

`document-range-formatting?` if true or not present, document range formatting is provided.

`dependency-scheme` by default, dependencies are linked with vim's `zipfile://<zipfile>::<innerfile>` scheme, however you can use a scheme of `jar` to get urls compatible with java's JarURLConnection. You can have the client make an lsp extension request of `clojure/dependencyContents` with the jar uri and the server will return the jar entry's contents. [Similar to java clients](https://github.com/redhat-developer/vscode-java/blob/a24945453092e1c39267eac9367c759a6c7b0497/src/extension.ts#L290-L298)

`cljfmt` json encoded configuration for https://github.com/weavejester/cljfmt

```clojure
:cljfmt 
  {:indents 
    {"#.*" [["block", 0]]
     "ns" [["inner", 0], ["inner", 1]]
     "and" [["inner", 0]]
     "or" [["inner", 0]]
     "are" [["inner", 0]]}}
```

`project-specs` - value is a vector containing a map of key/value pairs, for example:
```clojure
"initializationOptions": {
    "project-specs": [{
        "project-path": "deps.edn",
        "classpath-cmd": ["clj", "-Spath"]}]
    }
```
Note: You may also consider configuring project specs via the (optional) `.lsp/config.edn` file, i.e.,
```clojure
{:project-specs [{:project-path "deps.edn"
                  :classpath-cmd ["clj" "-Spath"]}]}
```
Each project-spec will add to the list of dependencies for lsp to crawl:
  - `:project-path` is the required filename used by your build tool (project.clj, build.boot, deps.edn, package.json, etc)
  - `:classpath-cmd` is the required vector of commands to get your project's classpath string (e.g. `["clj", "-Spath"]`)
  - `:env` optionally add environment variables to the classpath-cmd (e.g. `{"BOOT_FILE" "x.boot"}`)
  
### Lint

`clojure-lsp` uses [clj-kondo](https://github.com/clj-kondo/clj-kondo) to lint the code and retrieve the analysis to
make most of features work, make sure you have it properly configured in your `.clj-kondo/config.edn` file, for more information about available configurations, 
check the [clj-kondo configuration section](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md)

---
## Project

`clojure-lsp` will also look for project specific settings in a file called `.lsp/config.edn`. It will search from your root folder up the directory structure so you can have multiple projects share the same settings.

```clojure
{:macro-defs {korma.core/defentity [:declaration :elements]}
 :cljfmt {:indents {#re ".*" ns [[:inner 0] [:inner 1]]}}
 :clj-kondo {:linters {:missing-docstring {:level :warning}}}}
```

---
## Global

For global settings which should work for all the projects using `clojure-lsp`, you just need to add the same configs to `~/.lsp/config.edn`.

For an example of `config.edn`, check [here](https://github.com/ericdallo/dotfiles/blob/master/.lsp/config.edn).

---
## InitializationOptions

This is specific for an client, where it sends on startup, check [LSP spec for more information](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize).

This is an [example how Emacs `lsp-mode`](https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-clojure.el#L205) pass custom information.
