# clojure-lsp

A [Language Server](https://microsoft.github.io/language-server-protocol/) for Clojure. Taking a Cursive like approach of statically analyzing code.

## What is this?

The goal of this project is to bring great editing tools for Clojure to all editors. 
It aims to work alongside you to help you navigate, identify and fix errors, and perform refactorings. 

You will get:

- **Autocomplete**
- **Jump to definition**
- **Find usages**
- **Renaming**
- **Errors**
- **Automatic ns management**
- **Refactorings**

This is an early work in progress, contributions are very welcome.

## Installation

- Grab the latest `clojure-lsp` from github [LATEST](https://github.com/snoe/clojure-lsp/releases/latest)
- Place it in your $PATH with chmod 755
- Follow the documentation for your editor's language client. See [Clients](#clients) below.

## Capabilities

| capability | done | partial? | notes |
| ---------- | ---- | -------- | ----- |
| completionProvider | | √ | autocomplete as you type| TODO: add function signatures, docstrings, crawl classpath jars? |
| referencesProvider | | √ | TODO: keywords, crawl classpath jars, scoping for `if-let` and some others |
| renameProvider     | | √ | |
| definitionProvider | | √ | TODO: keywords, jar links |
| diagnostics        | | √ | very early - only shows unresolved symbols |
| hover              | | √ | very early - shows fn params but mostly for debugging at the moment | 

## Refactorings

It should be possible to introduce most of the refactorings here: https://github.com/clojure-emacs/clj-refactor.el/tree/master/examples
Calling executeCommand with the following commands and additional args will notify the client with `applyEdit`. 
All commands expect the first three args to be `[document-uri, line, column]` (eg `["file:///home/snoe/file.clj", 13, 11]`)

| done | command | args | notes |
| ---- | ------- | ---- | ----- |
| [x]  | cycle-coll | | |
| [x]  | thread-first | | |
| [x]  | thread-first-all | | |
| [x]  | thread-last | | |
| [x]  | thread-last-all | | |
| [ ]  | introduce-let | | |

Refactorings can be done with LanguageClient-neovim with:
```vim
function! s:Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : l:result
endfunction

function! ExecuteCommand(refactoring) abort
  call LanguageClient#workspace_executeCommand(a:refactoring,  ['file://' + s:Expand('%:p'), line('.') - 1, col('.') - 1])
endfunction
```

and then `:call ExecuteCommand('thread-first')` with the cursor in the appropriate place.
Other clients might provide a higher level interface to `workspace/executeCommand` you need to pass the path, line and column numbers.

## Clients

Clients are either editors with built in LSP support like Oni, or an appropriate plugin. 
*Clients are responsible for launching the server, the server is a subprocess of your editor not a daemon.*

In general, make sure to configure the client to use stdio and a server launch command like `['/usr/local/bin/clojure-lsp']`. 

### Vim 
Both http://github.com/autozimu/LanguageClient-neovim and https://github.com/prabirshrestha/vim-lsp work well. I think supporting completionItem `additionalTexts` is important for auto-imports and the former might be a bit closer (various PRs with omnicomplete)

### Oni
Seems to work reasonably well but couldn't get rename to work reliably https://github.com/onivim/oni

### Intellij / Cursive
https://github.com/gtache/intellij-lsp tested only briefly. 

### vscode
Proof of concept in the client-vscode directory in this repo.

### atom
I tried making a client but my hello world attempt didn't seem to work. If someone wants to take this on, I'd be willing to package it here too. 

### emacs
https://github.com/emacs-lsp looks promising but I haven't had a chance to try it (similar one client per server).

## TODO

### Persistence
- Project db file 
  - store file and last modified and file environments

### Diagnostics 
- crawl through parsed results and see if symbols exist where used
- unused imports, params, defs

### Others
- Better completion item kinds
- formatting (clj-format?)
- other lsp capabilities?
- Cursive style "resolve macros as" def/defn/let etc.. to expose more vars (dynamic based on client configuration messages)
- crawl classpath for exported vars 
- crawl project.clj / build.boot src and test paths
- keep separate cljs and clj environments
- handle cljc reader-macros
