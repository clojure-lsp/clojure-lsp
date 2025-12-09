# Capabilities

## Implementation Status

Below are all the currently supported LSP capabilities and their implementation status:

| capability                             | done | notes                                                                         |
|----------------------------------------|------|-------------------------------------------------------------------------------|
| initialize                             | √    |                                                                               |
| initialized                            | √    |                                                                               |
| shutdown                               | √    |                                                                               |
| exit                                   | √    |                                                                               |
| $/cancelRequest                        | √    |                                                                               |
| $/progress                             | √    |                                                                               |
| window/showDocument                    | √    |                                                                               |
| window/showMessage                     | √    |                                                                               |
| window/showMessageRequest              | √    |                                                                               |
| window/logMessage                      |      |                                                                               |
| window/workDoneProgress/create         |      |                                                                               |
| window/workDoneProgress/cancel         |      |                                                                               |
| telemetry/event                        |      |                                                                               |
| client/registerCapability              | √    |                                                                               |
| client/unregisterCapability            |      |                                                                               |
| workspace/workspaceFolders             |      |                                                                               |
| workspace/didChangeWorkspaceFolders    |      |                                                                               |
| workspace/didChangeConfiguration       | √    | Currently does nothing but log                                                |
| workspace/configuration                |      | We use a more robust settings concept (.lsp/config.edn)                       |
| workspace/didChangeWatchedFiles        | √    |                                                                               |
| workspace/symbol                       | √    |                                                                               |
| workspace/executeCommand               | √    | See [Extra capabilities](#extra-capabilities)                                 |
| workspace/applyEdit                    | √    | TextDocumentEdit and RenameFile only                                          |
| workspace/willRenameFiles              | √    |                                                                               |
| workspace/didRenameFiles               | √    |                                                                               |
| workspace/willCreateFiles              |      |                                                                               |
| workspace/didCreateFiles               |      |                                                                               |
| workspace/willDeleteFiles              |      |                                                                               |
| workspace/didDeleteFiles               |      |                                                                               |
| textDocument/didOpen                   | √    |                                                                               |
| textDocument/didChange                 | √    |                                                                               |
| textDocument/willSave                  |      |                                                                               |
| textDocument/willSaveWaitUntil         |      |                                                                               |
| textDocument/didSave                   | √    | Do nothing currently                                                          |
| textDocument/didClose                  | √    |                                                                               |
| textDocument/publishDiagnostics        | √    |                                                                               |
| textDocument/completion                | √    |                                                                               |
| completionItem/resolve                 | √    |                                                                               |
| textDocument/hover                     | √    |                                                                               |
| textDocument/signatureHelp             | √    | Missing support for active parameter ATM                                      |
| textDocument/declaration               | √    | Show where the symbol was declared on the file, like alias/refer  definitions |
| textDocument/definition                | √    |                                                                               |
| textDocument/typeDefinition            |      |                                                                               |
| textDocument/implementation            | √    | Implementation of defprotocols/defmulti showing deftypes and defrecords       |
| textDocument/references                | √    |                                                                               |
| textDocument/documentHighlight         | √    |                                                                               |
| textDocument/documentSymbol            | √    |                                                                               |
| textDocument/codeAction                | √    |                                                                               |
| codeAction/resolve                     | √    |                                                                               |
| textDocument/codeLens                  | √    |                                                                               |
| codeLens/resolve                       | √    |                                                                               |
| textDocument/documentLink              |      |                                                                               |
| documentLink/resolve                   |      |                                                                               |
| textDocument/documentColor             |      |                                                                               |
| textDocument/colorPresentation         |      |                                                                               |
| textDocument/formatting                | √    |                                                                               |
| textDocument/rangeFormatting           | √    |                                                                               |
| textDocument/onTypeFormatting          |      |                                                                               |
| textDocument/rename                    | √    |                                                                               |
| textDocument/prepareRename             | √    |                                                                               |
| textDocument/foldingRange              | √    |                                                                               |
| textDocument/selectionRange            | √    |                                                                               |
| textDocument/semanticTokens/full       | √    |                                                                               |
| textDocument/semanticTokens/full/delta |      |                                                                               |
| textDocument/semanticTokens/range      | √    |                                                                               |
| workspace/semanticTokens/refresh       |      |                                                                               |
| workspace/codeLens/refresh             | √    |                                                                               |
| textDocument/linkedEditingRange        | √    |                                                                               |
| textDocument/prepareCallHierarchy      | √    |                                                                               |
| callHierarchy/incomingCalls            | √    |                                                                               |
| callHierarchy/outgoingCalls            | √    |                                                                               |
| textDocument/moniker                   |      |                                                                               |

---
## Extra capabilities

Besides LSP official capabilities, `clojure-lsp` has some extra features:

### Refactorings

It should be possible to introduce most of the refactorings [here](https://github.com/clojure-emacs/clj-refactor.el/tree/master/examples)

---
#### More details

Calling `executeCommand` with the following commands and additional args will notify the client with `applyEdit`.
All commands expect the first three args to be `[document-uri, line, character]` (eg `["file:///home/snoe/file.clj", 13, 11]`)

| done | command             | args                                                                      | notes |
|------|---------------------|---------------------------------------------------------------------------|-------|
| √    | add-missing-import  | `[document-uri, line, character, import-name]`                            |       |
| √    | add-missing-libspec |                                                                           |       |
| √    | clean-ns            |                                                                           |       |
| √    | cond->if-refactor   |                                                                           |       |
| √    | cycle-coll          |                                                                           |       |
| √    | cycle-privacy       |                                                                           |       |
| √    | expand-let          |                                                                           |       |
| √    | extract-function    | `[document-uri, line, character, function-name]`                          |       |
| √    | if->cond-refactor   |                                                                           |       |
| √    | inline-symbol       |                                                                           |       |
| √    | introduce-let       | `[document-uri, line, character, binding-name]`                           |       |
| √    | move-to-let         | `[document-uri, line, character, binding-name]`                           |       |
| √    | thread-first        |                                                                           |       |
| √    | thread-first-all    |                                                                           |       |
| √    | thread-last         |                                                                           |       |
| √    | thread-last-all     |                                                                           |       |
| √    | unwind-all          |                                                                           |       |
| √    | unwind-thread       |                                                                           |       |
| √    | resolve-macro-as    | `[document-uri, line, character, resolved-full-symbol kondo-config-path]` |       |
| √    | create-test         |                                                                           |       |

See Vim client section for an example.

Emacs provides all those refactorings via [lsp-mode](https://emacs-lsp.github.io/lsp-mode/)  with the `lsp-clojure-` prefix.

Other clients might provide a higher level interface to `workspace/executeCommand` you need to pass the path, line and character numbers.

### Custom methods

`clojure-lsp` has some custom LSP methods that doesn't follow the protocol but aggregate value as a clojure IDE:

#### Test tree

Notify client with test tree data to build a UI tree on client side.

Type: Server Notification

Capability: `experimental.testTree`

Method: `clojure/textDocument/testTree`

Response: `TestTreeParams`

  - `TestTreeParams`: `{:uri string, :tree TestTreeNode}`

  - `TestTreeNode`: `{:name string, name-range: Range, range: Range, :kind TestTreeKind, :children [TestTreeNode]}`

  - `TestTreeKind`: `:namespace 1 | :deftest 2 | :testing 3`
  
#### Project tree

Request the project tree showing project source-paths and external dependencies.
If params is null request the project root nodes, otherwise request the nodes of the passed node.
The project tree is lazy, so client should request the nodes of a node when user expand it manually.

Type: Client request with response

Capability: `experimental.projectTree`

Method: `clojure/workspace/projectTree/nodes`

Params: `ProjectTreeNodeLeaf` | `null`

Response: `ProjectTreeNodeBranch`

  - `ProjectTreeNode`: `ProjectTreeNodeBranch | ProjectTreeNodeLeaf`

  - `ProjectTreeNodeBranch`: `{:name string, :type ProjectTreeNodeType, :uri string?, :detail string?, :id string?, :nodes [ProjectTreeNode]}`
  
  - `ProjectTreeNodeLeaf`: `{:name string, :type ProjectTreeNodeType, :id string?, :uri string?, :range Range?, :detail string?, final boolean}`

  - `ProjectTreeNodeType`: `:project 1 | :source-path 2 | :library 3 | :jar 4 | :ns 5 | :class 6 | :function 7 | :variable 8 | :interface 9`

#### Server Info Raw

Use to retrieve from server the server configuration information.

Type: Client request with response

Capability: none 

Method: `clojure/serverInfo/raw`

Params: none 

Response: Any

#### Server Info Log

Use to log to client the server configuration information.

Type: Client request with response

Capability: none 

Method: `clojure/serverInfo/log`

Params: none 

Response: none

#### Cursor Info Log

Use to log to client the debugging information for the symbol at cursor.

Type: Client request with response

Capability: none 

Method: `clojure/cursorInfo/log`

Params: `[uri, line, character]` 

Response: Any

#### Clojuredocs raw

Use to retrieve any Clojuredocs entry. Will return `null` if no entry found.

Type: Client request with response

Capability: none 

Method: `clojure/clojuredocs/raw`

Params: `[symbol-name, symbol-ns]`

Response: Any
