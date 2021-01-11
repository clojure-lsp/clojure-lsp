# Settings 

Custom `clojure-lsp` settings can be configured via 3 ways: **Global configuration**, **project configuration** or **LSP InitializationOptions**.

## Supported settings

`source-paths` value is a set of project-local directories to look for clj/cljc/cljs files. Default is `#{"src", "test"}`.

`ignore-classpath-directories` if true, will not consider clojure files within the directories specified by your classpath. This is needed, for instance, if your build puts artifacts into `resources` or `target` that you want lsp to ignore.

`use-metadata-for-privacy?` if true, will use `^:private` metadata for refactorings instead of `defn-`

`keep-require-at-start?` if true, will keep first require at the first line instead of inserting a new line before it.

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

`clj-kondo` (Experimental) -  `clojure-lsp` uses [clj-kondo](https://github.com/borkdude/clj-kondo) to lint code, so you can use here any `clj-kondo` configuration or just have your config file by project at `.clj-kondo/config.edn`, for more information about `clj-kondo` available configurations, check [here](https://github.com/borkdude/clj-kondo/blob/master/doc/config.md).

```clojure
:clj-kondo {:linters {:missing-docstring {:level :warning}}}
```
 
~`linters`~ (Deprecated in favor of `clj-kondo`) - some initial support for disabling diagnostics currently only this one that will suppress the unused alias warning and stop the require from being cleaned by `clean-ns`:

```clojure
 :linters {:unused-namespace {:exclude [clojure.data]}
           :unused-namespace-declarations #{"test/" "foo/bar"}}
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

---
## macro-defs

`macro-defs` value is a map of fully-qualified macro names to a vector of definitions of those macros' forms.

### Element definitions

An element represents one or more forms within a macro-def vector. They can be defined in two ways:

* A simple keyword, e.g. `:declaration`

* A map that includes the element type and options,
  e.g. `{:element :declaration, :tags ["unused" "local"], :signature ["next"]}`

### Element types

Valid element definitions are:

  - `:declaration` This marks a symbol or keyword as a definition/declaration of
    a var in the current namespace.
    - In the simplest case, this element can be specified as the keyword
      `:declaration`.
    - You can customize the behavior of the declaration by making it a map.
      - e.g. `{:element :declaration, :tags ["unused" "local"], :signature ["next"]}`
      - `tags` are optional.
        - The `unused` tag supresses the "unused declaration" diagnostic, useful
          for `deftest` vars.
        - The `local` tag marks the var as private.
      - `signature` is optional. If the macro has `defn`-like bindings, this
        vector of movements should point to the parameter vector or the first
        var arg list form. Only `next` is supported right now.
    - e.g. `(my-defn- my-name "docstring" [& params] (count params))` =>
      `{my.ns/my-defn- [{"element": "declaration", "tags", ["local"],
      "signature": ["next" "next"]}]}`

  - `:bindings` This marks `let` and `for`-like bindings. `bound-elements` will have these bindings in their scope.
  - e.g. `(my-with-open [resource ()] ....)` => `{my.ns/my-with-open [:bindings :bound-elements]}`

  - `:function-params-and-bodies` This will parse function like forms that support optional var-args like `fn`.
    - e.g. `(myfn ([a] ...) ([b] ...)) (myfn [c] ...)` => `{my.ns/myfn [:function-params-and-bodies]}`

  - `:params` This marks a `defn` like parameter vector. `bound-elements` will have these parameters in their scope.
    - e.g. `(myfn [c] ...)` => `{my.ns/myfn [:params :bound-elements]}`

  - `:param` This marks a single `defn` like parameter. `bound-elements` will have these parameters in their scope.

  - `:elements` This will parse the rest of the elements in the macro form with the usual rules.
    - e.g. `(myif-let [answer (expr)] answer (log "no answer") "no answer")` =>
      `{my.ns/myif-let [:bindings :bound-element :elements]}`

  - `:element` This will parse a single element in the macro form with the usual
    rules.
    - In the simplest case, `:element` can be specified as the keyword
      `:element`. This will always parse a single element.
    - You can make the `:element` optional by making it a map that includes
      a predicate `pred` which will determine whether the current form is parsed
      as an `:element`, or if the `:element` should be skipped and the current
      form parsed as the next defined element.
    - For example, you can define an optional docstring element as `{:element
      :element, :pred :string}`, or an optional metadata map as `{:element
      :element, :pred :map}`.
    - `:element` can also describe repeated elements. For example,
      `{:element :element, :pred :string, :repeat true}` will parse 1 or more
      strings.
    - `:element` can also describe multiple elements of different types. This is
      useful, for example, if you have a macro like
      [`adzerk.env/def`](https://github.com/adzerk-oss/env#get) whose arguments
      are pairs of declarations and values:
      - `(adzerk.env/def FOO :required, BAR nil, BAZ "string")` =>
        `{adzerk.env/def [{:element [:declaration :element], :repeat true}]}`

  - `:bound-elements` This will parse the rest of the elements in the macro form with the usual rules but with any `bindings` or `params` in scope.

  - `:bound-element` This will parse a single element in the macro form with the usual rules but with any `bindings` or `params` in scope.

See https://github.com/snoe/clojure-lsp/blob/master/test/clojure_lsp/parser_test.clj for examples.

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

For global settings, you just need to add the same configs to `~/.lsp/config.edn`.

For an example of `config.edn`, check [here](https://github.com/ericdallo/dotfiles/blob/master/.lsp/config.edn).

---
## InitializationOptions

This is specific for an client, where it sends on startup, check [LSP spec for more information](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize).

This is an [example how Emacs `lsp-mode`](https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-clojure.el#L205) pass custom information.