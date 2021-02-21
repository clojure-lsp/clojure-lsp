# Features

Above you can find all available features that clojure-lsp provide with examples using [Emacs lsp-mode](https://emacs-lsp.github.io/lsp-mode/) client.

### Find a function/var definition

![](images/features/find-definition.gif)

### Find all references of a function, var, keyword or namespace alias

![](images/features/find-references.gif)

### Show all workspace/project symbols

![](images/features/workspace-symbols.gif)

### Show all symbols on current file

![](images/features/document-symbols.gif)

### Rename symbols

![](images/features/rename.gif)

### Cursor hover showing symbol usages
### Code actions

| Name                               | Example                                                           |
|------------------------------------|-------------------------------------------------------------------|
| Clean namespace require/imports    | <img src="../images/features/clean-ns.gif" width=360>             |
| Inline symbol                      | <img src="../images/features/inline-symbol.gif" width=360>        |
| Extract function                   | <img src="../images/features/extract-function.gif" width=360>     |
| Add missing require                | <img src="../images/features/add-missing-require.gif" width=360>  |
| Add known common require           | <img src="../images/features/add-common-require.gif" width=360>   |
| Add known common import            | <img src="../images/features/add-common-import.gif" width=360>    |
| Add suggested alias require        | <img src="../images/features/add-alias-suggestion.gif" width=360> |
| Move to let                        | <img src="../images/features/move-to-let.gif" width=360>          |
| Change coll to map,vector,set,list | <img src="../images/features/change-coll.gif" width=360>          |

### Code lenses showing symbol references

![](images/features/code-lens.png)

### Format a whole file or range

![](images/features/format-buffer.gif)

### Signature help

![](images/features/signature-help.gif)

### Semantic tokens

Experimental: apply color on client editor for each known token

### Call hierarchy (Incoming only)

![](images/features/call-hierarchy.png)

### Execute command

Commands that client can request. Some code actions use these commands as actions.

#### Refactoring

Commands that change/refactor the code, most of them are available via code actions.

##### Add import to namespace
##### Add missing namespace (available via code actions too)
##### Extract Function (available via code actions too)
##### Inline Symbol (available via code actions too)
##### Clean namespace (available via code actions too)
##### Cycle privacy of def/defn
##### Cycle collection (#{}, {}, [], ())
##### Change collection to {}, (), #{}, []
##### Expand let
##### Introduce let
##### Move expression to let
##### Thread first expression
##### Thread last expression
##### Thread first all
##### Thread last all
##### Unwind all
##### Unwind thread

#### Dev

##### Server information

Return basic information about the server.

##### Cursor information

Return debug information about the element at point.

### Custom message to client during any server process

During some process, `clojure-lsp` send messages to client informing some proccess, warning or error.
