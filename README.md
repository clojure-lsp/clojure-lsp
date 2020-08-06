![CI](https://github.com/snoe/clojure-lsp/workflows/CI/badge.svg?branch=master)

# clojure-lsp

A [Language Server](https://microsoft.github.io/language-server-protocol/) for Clojure. Taking a Cursive-like approach of statically analyzing code.

## What is this?

The goal of this project is to bring great editing tools for Clojure to all editors.
It aims to work alongside you to help you navigate, identify and fix errors, and perform refactorings.

You will get:

- **Autocomplete**
- **Jump to definition**
- **Find usages**
- **Renaming**
- **Code actions**
- **Errors**
- **Automatic ns management**
- **Refactorings**

## Installation

### Manually

- You need `java` on your $PATH.
- Grab the latest `clojure-lsp` from github [LATEST](https://github.com/snoe/clojure-lsp/releases/latest)
- Place it in your $PATH with a chmod 755
- Follow the documentation for your editor's language client. See [Clients](#clients) below.

### NixOS
`clojure-lsp` is available in the [nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/misc/clojure-lsp/default.nix):

```bash
nix-shell -p clojure-lsp
```

## Troubleshooting

See [troubleshooting.md](docs/troubleshooting.md).

## Capabilities

Bellow are all the currently supported LSP capabilities and their implementation status:

| capability                          | done | notes                                         |
| ----------                          | ---- | -----                                         |
| initialize                          | √    |                                               |
| initialized                         | √    |                                               |
| shutdown                            | √    |                                               |
| exit                                | √    |                                               |
| $/cancelRequest                     |      |                                               |
| $/progress                          |      |                                               |
| window/showMessage                  |      |                                               |
| window/showMessageRequest           |      |                                               |
| window/logMessage                   |      |                                               |
| window/workDoneProgress/create      |      |                                               |
| window/workDoneProgress/cancel      |      |                                               |
| telemetry/event                     |      |                                               |
| client/registerCapability           | √    |                                               |
| client/unregisterCapability         |      |                                               |
| workspace/workspaceFolders          |      |                                               |
| workspace/didChangeWorkspaceFolders |      |                                               |
| workspace/didChangeConfiguration    | √    | Currently only log                            |
| workspace/configuration             |      |                                               |
| workspace/didChangeWatchedFiles     | √    |                                               |
| workspace/symbol                    | √    |                                               |
| workspace/executeCommand            | √    | See [Extra capabilities](#extra-capabilities) |
| workspace/applyEdit                 | √    |                                               |
| textDocument/didOpen                | √    |                                               |
| textDocument/didChange              | √    |                                               |
| textDocument/willSave               |      |                                               |
| textDocument/willSaveWaitUntil      |      |                                               |
| textDocument/didSave                | √    | Do nothing currently                          |
| textDocument/didClose               | √    |                                               |
| textDocument/publishDiagnostics     | √    |                                               |
| textDocument/completion             | √    |                                               |
| completionItem/resolve              | √    |                                               |
| textDocument/hover                  | √    |                                               |
| textDocument/signatureHelp          | X    | Implemented hard coded                        |
| textDocument/declaration            |      |                                               |
| textDocument/definition             | √    | TODO: Find java classes definition            |
| textDocument/typeDefinition         |      |                                               |
| textDocument/implementation         |      |                                               |
| textDocument/references             | √    |                                               |
| textDocument/documentHighlight      | √    |                                               |
| textDocument/documentSymbol         | √    |                                               |
| textDocument/codeAction             | √    |                                               |
| textDocument/codeLens               | √    |                                               |
| codeLens/resolve                    | √    |                                               |
| textDocument/documentLink           |      |                                               |
| documentLink/resolve                |      |                                               |
| textDocument/documentColor          |      |                                               |
| textDocument/colorPresentation      |      |                                               |
| textDocument/formatting             | √    |                                               |
| textDocument/rangeFormatting        | √    |                                               |
| textDocument/onTypeFormatting       |      |                                               |
| textDocument/rename                 | √    |                                               |
| textDocument/prepareRename          |      |                                               |
| textDocument/foldingRange           |      |                                               |
| textDocument/selectionRange         |      |                                               |

## Extra capabilities

Besides LSP official capabilities, `clojure-lsp` has some extra features:

### Refactorings

It should be possible to introduce most of the refactorings [here](https://github.com/clojure-emacs/clj-refactor.el/tree/master/examples)
Calling `executeCommand` with the following commands and additional args will notify the client with `applyEdit`.
All commands expect the first three args to be `[document-uri, line, column]` (eg `["file:///home/snoe/file.clj", 13, 11]`)

| done | command             | args                                          | notes                                |
| ---- | ------------------- | ----                                          | -----                                |
| √    | add-missing-libspec |                                               |                                      |
| -    | clean-ns            |                                               | :require sort and remove unused only |
|      |                     |                                               |                                      |
| √    | cycle-coll          |                                               |                                      |
| √    | cycle-privacy       |                                               |                                      |
| √    | expand-let          |                                               |                                      |
| √    | extract-function    | `[document-uri, line, column, function-name]` |                                      |
| √    | inline-symbol       |                                               |                                      |
| √    | introduce-let       | `[document-uri, line, column, binding-name]`  |                                      |
| √    | move-to-let         | `[document-uri, line, column, binding-name]`  |                                      |
| √    | thread-first        |                                               |                                      |
| √    | thread-first-all    |                                               |                                      |
| √    | thread-last         |                                               |                                      |
| √    | thread-last-all     |                                               |                                      |
| √    | unwind-all          |                                               |                                      |
| √    | unwind-thread       |                                               |                                      |

See Vim client section for an example.

Other clients might provide a higher level interface to `workspace/executeCommand` you need to pass the path, line and column numbers.

## InitializationOptions

It is possible to pass some options to clojure-lsp through clients' `InitializationOptions`. Options are a map with keys:

`source-paths` value is a vector of project-local directories to look for clj/cljc/cljs files. Default is `["src","test"]`.

`ignore-classpath-directories` if true, will not consider clojure files within the directories specified by your classpath. This is needed, for instance, if your build puts artifacts into `resources` or `target` that you want lsp to ignore.

`use-metadata-for-privacy?` if true, will use `^:private` metadata for refactorings instead of `defn-`

`keep-require-at-start?` if true, will keep first require at the first line instead of inserting a new line before it.

`show-docs-arity-on-same-line?` if true, will keep the arity on the same line of the function on hover, useful for Emacs users.

`dependency-scheme` by default, dependencies are linked with vim's `zipfile://<zipfile>::<innerfile>` scheme, however you can use a scheme of `jar` to get urls compatible with java's JarURLConnection. You can have the client make an lsp extension request of `clojure/dependencyContents` with the jar uri and the server will return the jar entry's contents. [Similar to java clients](https://github.com/redhat-developer/vscode-java/blob/a24945453092e1c39267eac9367c759a6c7b0497/src/extension.ts#L290-L298)

`cljfmt` json encoded configuration for https://github.com/weavejester/cljfmt

```json
"cljfmt": {
  "indents": {
    "#.*": [["block", 0]],
    "ns": [["inner", 0], ["inner", 1]],
    "and": [["inner", 0]],
    "or": [["inner", 0]],
    "are": [["inner", 0]]
  }
},
```

`clj-kondo` (Experimental) -  `clojure-lsp` uses [clj-kondo](https://github.com/borkdude/clj-kondo) to lint code, so you can use here any `clj-kondo` configuration or just have your config file by project at `.clj-kondo/config.edn`, for more information about `clj-kondo` available configurations, check [here](https://github.com/borkdude/clj-kondo/blob/master/doc/config.md).

```clojure
"clj-kondo" {:linters {:missing-docstring {:level :warning}}}
```
 
~`linters`~ (Deprecated in favor of `clj-kondo`) - some initial support for disabling diagnostics currently only this one that will suppress the unused alias warning and stop the require from being cleaned by `clean-ns`:

```clojure
 "linters" {:unused-namespace {:exclude [clojure.data]}}
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
{"project-specs" [{:project-path "deps.edn"
                   :classpath-cmd ["clj" "-Spath"]}]}
   ```
Each project-spec will add to the list of dependencies for lsp to crawl:
  - `project-path` is the required filename used by your build tool (project.clj, build.boot, deps.edn, package.json, etc)
  - `classpath-cmd` is the required vector of commands to get your project's classpath string (e.g. `["clj", "-Spath"]`)
  - `env` optionally add environment variables to the classpath-cmd (e.g. `{"BOOT_FILE": "x.boot"}`)

### macro-defs

`macro-defs` value is a map of fully-qualified macro names to a vector of definitions of those macros' forms.

#### Element definitions

An element represents one or more forms within a macro-def vector. They can be defined in two ways:

* A simple keyword, e.g. `:declaration`

* A map that includes the element type and options,
  e.g. `{:element :declaration, :tags ["unused" "local"], :signature ["next"]}`

#### Element types

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

## Project Settings

LSP will also look for project specific settings in a file called '.lsp/config.edn'. It will search from your root folder up the directory structure so you can have multiple projects share the settings.

```clojure
{"macro-defs" {korma.core/defentity [:declaration :elements]}
 "cljfmt" {:indents {#re ".*" ns [[:inner 0] [:inner 1]]}}
 "clj-kondo" {:linters {:missing-docstring {:level :warning}}}}
 ```

## Clients

Clients are either editors with built in LSP support like Oni, or an appropriate plugin.
*Clients are responsible for launching the server, the server is a subprocess of your editor not a daemon.*

In general, make sure to configure the client to use stdio and a server launch command like `['/usr/local/bin/clojure-lsp']`.
If that fails, you may need to have your client launch inside a shell, so use someting like `['bash', '-c', '/usr/local/bin/clojure-lsp']`.
In windows you probably need to rename to `clojure-lsp.bat`.

### Vim
I prefer https://github.com/neoclide/coc.nvim but both http://github.com/autozimu/LanguageClient-neovim and https://github.com/prabirshrestha/vim-lsp work well.

See my [nvim/init.vim](https://github.com/snoe/dotfiles/blob/master/home/.vimrc) and [coc-settings.json](https://github.com/snoe/dotfiles/blob/master/home/.vim/coc-settings.json)

LanguageClient-neovim can be configure with:

Refactorings:
```vim

function! Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : "file://" . l:result
endfunction

nnoremap <silent> crcc :call LanguageClient#workspace_executeCommand('cycle-coll', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crth :call LanguageClient#workspace_executeCommand('thread-first', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtt :call LanguageClient#workspace_executeCommand('thread-last', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtf :call LanguageClient#workspace_executeCommand('thread-first-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtl :call LanguageClient#workspace_executeCommand('thread-last-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crml :call LanguageClient#workspace_executeCommand('move-to-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> cril :call LanguageClient#workspace_executeCommand('introduce-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> crel :call LanguageClient#workspace_executeCommand('expand-let', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> cram :call LanguageClient#workspace_executeCommand('add-missing-libspec', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
```

`InitializationOptions` can be sent by setting:
`let g:LanguageClient_settingsPath=".lsp/settings.json"`

Project-local `.lsp/settings.json` would have content like:
```clojure
{"initializationOptions": {
   "source-paths": ["shared-src", "src", "test", "dashboard/src"],
   "macro-defs": {project.macros/dofor: ["bindings", "bound-elements"]}}}
```

### Emacs
[lsp-mode](https://emacs-lsp.github.io/lsp-mode) has built in support for `clojure-lsp`. With `use-package`, add the following to your emacs config:

```elisp
(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp") ;; Optional: In case `clojure-lsp` is not in your PATH
        lsp-enable-indentation nil))
```

Optionally you can add `lsp-ui` for UI feedback and `company-mode` for completion:

```elisp
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)
```

In order to make the jumping into dependency jars work you have to have a `config.edn` in your `project-dir/.lsp` directory (or higher in the directory hierarchy, like `~/.lsp`) with the right `dependency-scheme` so the server returns an URI `emacs-lsp` can process:

```clojure
{"dependency-scheme" "jar"}
```

In `lsp-mode` `lsp-clojure-server-command` variable is available to override the command to start the `clojure-lsp` server, might be necessary to do this on a Windows environment.


### Oni
Seems to work reasonably well but couldn't get rename to work reliably https://github.com/onivim/oni

### Intellij / Cursive
https://github.com/gtache/intellij-lsp tested only briefly.

### VScode
Proof of concept in the client-vscode directory in this repo.

### Atom
I tried making a client but my hello world attempt didn't seem to work. If someone wants to take this on, I'd be willing to package it here too.

## TODO

### Others
- Better completion item kinds

## Building local

For building local, run `lein bin` to generate the binary inside `target` folder.
