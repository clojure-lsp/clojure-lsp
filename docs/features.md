# Features

Above you can find all available features that clojure-lsp provide with examples using [Emacs lsp-mode](https://emacs-lsp.github.io/lsp-mode/) client.

### Find a function/var definition

<img src="../images/features/find-definition.gif" width=340>

### Find all references of a function, var, keyword or namespace alias

<img src="../images/features/find-references.gif" width=340>

### Show all workspace/project symbols

<img src="../images/features/workspace-symbols.gif" width=580>

### Show all symbols on current file

<img src="../images/features/document-symbols.gif" width=580>

### Rename symbols

<img src="../images/features/rename.gif" width=340>

### Document highlight on hover showing symbol usages

<img src="../images/features/document-highlight.gif" width=340>

### Code actions

| Name                               | Example                                                              |
|------------------------------------|----------------------------------------------------------------------|
| Clean namespace require/imports    | <img src="../images/features/clean-ns.gif" width=360>                |
| Add missing require                | <img src="../images/features/add-missing-require.gif" width=360>     |
| Add known common require           | <img src="../images/features/add-common-require.gif" width=360>      |
| Add known common import            | <img src="../images/features/add-common-import.gif" width=360>       |
| Add suggested alias require        | <img src="../images/features/add-alias-suggestion.gif" width=360>    |
| Inline symbol                      | <img src="../images/features/inline-symbol.gif" width=360>           |
| Extract function                   | <img src="../images/features/extract-function.gif" width=360>        |
| Move to let                        | <img src="../images/features/move-to-let.gif" width=360>             |
| Change coll to map,vector,set,list | <img src="../images/features/change-coll.gif" width=360>             |
| Thread first/all last              | <img src="../images/features/thread-first-all.gif" width=360>        |
| Create private function            | <img src="../images/features/create-private-function.gif" width=360> |

### Code lenses showing symbol references

![](images/features/code-lens.png)

### Format a whole file or range

![](images/features/format-buffer.gif)

### Signature help

![](images/features/signature-help.gif)

### Semantic tokens

Experimental: apply color on client editor for each known token

### Call hierarchy (Incoming only)

Show all the call hierarchy of a function/variable as a lazy tree

![](images/features/call-hierarchy.png)

### Execute command

Commands that client can request. Some code actions use these commands as actions.

#### Refactoring

Commands that change/refactor the code, most of them are available via code actions.

##### Clean namespace *
##### Add import to namespace
##### Add missing namespace *
##### Cycle privacy of def/defn
##### Cycle collection (#{}, {}, [], ())
##### Change collection to {}, (), #{}, []
##### Extract Function *
##### Create private function *
##### Inline Symbol *
##### Expand let
##### Introduce let
##### Move expression to let
##### Thread first expression
##### Thread last expression
##### Thread first all *
##### Thread last all *
##### Unwind all
##### Unwind thread

_* Available via code actions too_

#### Dev

##### Server information

Return basic information about the server.

##### Cursor information

Return debug information about the element at point.

### Custom message to client during any server process

During some process, `clojure-lsp` send messages to client informing some proccess, warning or error.

## Diagnostics (linter)

Most linters come from [clj-kondo](https://github.com/clj-kondo/clj-kondo) that clojure-lsp uses under the hood to lint the code and retrieve the analysis to
make most of features work.

Below you can find the custom linters implemented on clojure-lsp:

### unused-public-var

![](images/features/unused-public-var.png)

For more information on how to configure it, check the [diagnostics settings section](https://clojure-lsp.github.io/clojure-lsp/settings/#diagnostics-linter).
