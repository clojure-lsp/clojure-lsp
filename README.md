<img src="https://user-images.githubusercontent.com/7820865/103157675-3da6b700-4794-11eb-9771-d2da1dd9b7a7.png" width="180" align="right">

![CI](https://github.com/snoe/clojure-lsp/workflows/CI/badge.svg?branch=master)

# clojure-lsp

A [Language Server](https://microsoft.github.io/language-server-protocol/) for Clojure. Taking a Cursive-like approach of statically analyzing code.

## What is this?

The goal of this project is to bring great editing tools for Clojure to all editors.
It aims to work alongside you to help you navigate, identify and fix errors, and perform refactorings.

You will get:

- **Autocomplete**
- **Jump to definition**
- **Find references**
- **Renaming**
- **Code actions**
- **Errors**
- **Automatic ns management**
- **Refactorings**
- **Code lens**
- **Semantic tokens (syntax highlighting)**

## Installation

<details>
<summary><b>Manually</b></summary>

- You need `java` on your $PATH.
- Grab the latest `clojure-lsp` from github [LATEST](https://github.com/snoe/clojure-lsp/releases/latest)
- Place it in your $PATH with a chmod 755
- Follow the documentation for your editor's language client. See [Clients](#clients) below.

</details>

<details>
<summary><b>Nix</b></summary>

`clojure-lsp` is available in the [nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/misc/clojure-lsp/default.nix):

```bash
nix-shell -p clojure-lsp
```

</details>

## Settings 

Custom `clojure-lsp` settings can be configured via 3 ways: **Global configuration**, **project configuration** or **LSP InitializationOptions**.

<details>
  <summary><b>All supported settings</b></summary>

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

</details>

### macro-defs

`macro-defs` value is a map of fully-qualified macro names to a vector of definitions of those macros' forms.

<details>
  <summary>Custom macro defs</summary>

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

</details>

### Project

`clojure-lsp` will also look for project specific settings in a file called `.lsp/config.edn`. It will search from your root folder up the directory structure so you can have multiple projects share the same settings.

```clojure
{:macro-defs {korma.core/defentity [:declaration :elements]}
 :cljfmt {:indents {#re ".*" ns [[:inner 0] [:inner 1]]}}
 :clj-kondo {:linters {:missing-docstring {:level :warning}}}}
 ```

### Global

For global settings, you just need to add the same configs to `~/.lsp/config.edn`.

For an example of `config.edn`, check [here](https://github.com/ericdallo/dotfiles/blob/master/.lsp/config.edn).

### InitializationOptions

This is specific for an client, where it sends on startup, check [LSP spec for more information](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize).

This is an [example how Emacs `lsp-mode`](https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-clojure.el#L205) pass custom information.

## Clients

Clients are either editors with built in LSP support like Oni, or an appropriate plugin.
*Clients are responsible for launching the server, the server is a subprocess of your editor not a daemon.*

In general, make sure to configure the client to use stdio and a server launch command like `['/usr/local/bin/clojure-lsp']`.
If that fails, you may need to have your client launch inside a shell, so use someting like `['bash', '-c', '/usr/local/bin/clojure-lsp']`.
In windows you probably need to rename to `clojure-lsp.bat`.

<details>
  <summary><b>Vim</b></summary>

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

`initializationOptions` can be sent by setting:
`let g:LanguageClient_settingsPath=".lsp/settings.json"`

Project-local `.lsp/settings.json` would have content like:
```clojure
{"initializationOptions": {
   "source-paths": ["shared-src", "src", "test", "dashboard/src"],
   "macro-defs": {"project.macros/dofor": ["bindings", "bound-elements"]}}}
```
</details>

<details>
  <summary><b>Emacs</b></summary>

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
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))) ;; Optional: In case `clojure-lsp` is not in your PATH
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

In `lsp-mode`, `lsp-clojure-server-command` variable is available to override the command to start the `clojure-lsp` server, might be necessary to do this on a Windows environment.

For a detailed guide on how to configure Emacs with LSP, check [here](https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/)

For more `lsp-mode` clojure settings, check [here](https://emacs-lsp.github.io/lsp-mode/page/lsp-clojure/)

</details>

<details>
  <summary><b>Oni</b></summary>

Seems to work reasonably well but couldn't get rename to work reliably https://github.com/onivim/oni

</details>

<details>
  <summary><b>Intellij / Cursive</b></summary>

https://github.com/gtache/intellij-lsp tested only briefly.

</details>

<details>
  <summary><b>Visual Studio Code</b></summary>

* Proof of concept: in the client-vscode directory in this repo.
* [Calva](https://github.com/BetterThanTomorrow/calva) extension includes clojure-lsp support.
</details>

<details>
  <summary><b>Atom</b></summary>

I tried making a client but my hello world attempt didn't seem to work. If someone wants to take this on, I'd be willing to package it here too.
</details>

## Capabilities

Below are all the currently supported LSP capabilities and their implementation status:

<details>
<summary><b>Supported LSP capabilities</b></summary>

| capability                             | done | notes                                         |
| ----------                             | ---- | -----                                         |
| initialize                             | √    |                                               |
| initialized                            | √    |                                               |
| shutdown                               | √    |                                               |
| exit                                   | √    |                                               |
| $/cancelRequest                        |      |                                               |
| $/progress                             |      |                                               |
| window/showMessage                     | √    |                                               |
| window/showMessageRequest              |      |                                               |
| window/logMessage                      |      |                                               |
| window/workDoneProgress/create         |      |                                               |
| window/workDoneProgress/cancel         |      |                                               |
| telemetry/event                        |      |                                               |
| client/registerCapability              | √    |                                               |
| client/unregisterCapability            |      |                                               |
| workspace/workspaceFolders             |      |                                               |
| workspace/didChangeWorkspaceFolders    |      |                                               |
| workspace/didChangeConfiguration       | √    | Currently only log                            |
| workspace/configuration                |      |                                               |
| workspace/didChangeWatchedFiles        | √    |                                               |
| workspace/symbol                       | √    |                                               |
| workspace/executeCommand               | √    | See [Extra capabilities](#extra-capabilities) |
| workspace/applyEdit                    | √    | TextDocumentEdit and RenameFile only          |
| textDocument/didOpen                   | √    |                                               |
| textDocument/didChange                 | √    |                                               |
| textDocument/willSave                  |      |                                               |
| textDocument/willSaveWaitUntil         |      |                                               |
| textDocument/didSave                   | √    | Do nothing currently                          |
| textDocument/didClose                  | √    |                                               |
| textDocument/publishDiagnostics        | √    |                                               |
| textDocument/completion                | √    |                                               |
| completionItem/resolve                 | √    |                                               |
| textDocument/hover                     | √    |                                               |
| textDocument/signatureHelp             | X    | Implemented hard coded                        |
| textDocument/declaration               |      |                                               |
| textDocument/definition                | √    | TODO: Find java classes definition            |
| textDocument/typeDefinition            |      |                                               |
| textDocument/implementation            |      |                                               |
| textDocument/references                | √    |                                               |
| textDocument/documentHighlight         | √    |                                               |
| textDocument/documentSymbol            | √    |                                               |
| textDocument/codeAction                | √    |                                               |
| textDocument/codeLens                  | √    |                                               |
| codeLens/resolve                       | √    |                                               |
| textDocument/documentLink              |      |                                               |
| documentLink/resolve                   |      |                                               |
| textDocument/documentColor             |      |                                               |
| textDocument/colorPresentation         |      |                                               |
| textDocument/formatting                | √    |                                               |
| textDocument/rangeFormatting           | √    |                                               |
| textDocument/onTypeFormatting          |      |                                               |
| textDocument/rename                    | √    |                                               |
| textDocument/prepareRename             |      |                                               |
| textDocument/foldingRange              |      |                                               |
| textDocument/selectionRange            |      |                                               |
| textDocument/semanticTokens/full       | √    | Just `functions`, type' and `macros` ATM      |
| textDocument/semanticTokens/full/delta |      |                                               |
| textDocument/semanticTokens/range      | √    |                                               |
| workspace/semanticTokens/refresh       |      |                                               |
| textDocument/linkedEditingRange        |      |                                               |
| textDocument/prepareCallHierarchy      | √    |                                               |
| callHierarchy/incomingCalls            | √    |                                               |
| callHierarchy/outgoingCalls            |      |                                               |
| textDocument/moniker                   |      |                                               |
</details>

## Extra capabilities

Besides LSP official capabilities, `clojure-lsp` has some extra features:

### Refactorings

It should be possible to introduce most of the refactorings [here](https://github.com/clojure-emacs/clj-refactor.el/tree/master/examples)

<details>
  <summary><b>More details</b></summary>

Calling `executeCommand` with the following commands and additional args will notify the client with `applyEdit`.
All commands expect the first three args to be `[document-uri, line, column]` (eg `["file:///home/snoe/file.clj", 13, 11]`)

| done | command                 | args                                          | notes                           |
| ---- | -------------------     | ----                                          | -----                           |
| √    | add-import-to-namespace | `[document-uri, line, column, import-name]`   |                                 |
| √    | add-missing-libspec     |                                               |                                 |
| √    | clean-ns                |                                               | :require sort and remove unused |
| √    | cycle-coll              |                                               |                                 |
| √    | cycle-privacy           |                                               |                                 |
| √    | expand-let              |                                               |                                 |
| √    | extract-function        | `[document-uri, line, column, function-name]` |                                 |
| √    | inline-symbol           |                                               |                                 |
| √    | introduce-let           | `[document-uri, line, column, binding-name]`  |                                 |
| √    | move-to-let             | `[document-uri, line, column, binding-name]`  |                                 |
| √    | thread-first            |                                               |                                 |
| √    | thread-first-all        |                                               |                                 |
| √    | thread-last             |                                               |                                 |
| √    | thread-last-all         |                                               |                                 |
| √    | unwind-all              |                                               |                                 |
| √    | unwind-thread           |                                               |                                 |

See Vim client section for an example.

Emacs provides all those refactorings via [lsp-mode](https://emacs-lsp.github.io/lsp-mode/)  with the `lsp-clojure-` prefix.

Other clients might provide a higher level interface to `workspace/executeCommand` you need to pass the path, line and column numbers.

</details>

## [Troubleshooting](docs/troubleshooting.md)

## Building manually

For building manually, run `lein bin` to generate the binary inside `target` folder or `lein uberjar` for building the standalone jar.

## Development

For `clojure-lsp` development, there are 3 possible ways of finding a bug or implementing a new feature:
- Create a test for your bug/feature, then implement the code following the test.
- `clojure-lsp` starts a NREPL server, with that it's possible to change the code of a running instance and see the changes on your client in real time. To get the NREPL port, you can check the `/tmp/clojure-lsp.out` log, it will print the NREPL port on server startup or you can get it via `server-info` custom LSP command.
- Build `clojure-lsp` with your changes and test it manually in your client, this is the slowest option, but it makes sense final tests.

## Contribution

Contributions to `clojure-lsp` are very welcome! You can open an issue or a PR and we'd love to help.

## Support the project

`clojure-lsp` has more than 8.000 lines of code, to keep all of this working, we need to help the community on a lot of issues and implement new features. As a LSP server, this project is the base for Clojure clients like Emacs(lsp-mode), VSCode(Calva) and vim.

You can help us keep going and improving it by **[supporting the project](https://opencollective.com/clojure-lsp)**

<img src="https://opencollective.com/clojure-lsp/tiers/backer.svg">
