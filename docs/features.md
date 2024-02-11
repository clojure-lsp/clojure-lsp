# Features

Below you can find all available features that clojure-lsp provides with examples using [Emacs lsp-mode](https://emacs-lsp.github.io/lsp-mode/) as the client.

- [Built in LSP features](#lsp-features)
- [clojure-lsp extra commands](#clojure-lsp-extra-commands)


## LSP features

### Find a function/var definition

![](images/features/find-definition.gif)

### Find defprotocol/defmulti implementations

![](images/features/find-implementation.gif)

### Find a function/var declaration in the ns

![](images/features/find-declaration.gif)

### Find all references of a function, var, keyword or namespace alias

![](images/features/find-references.gif)

### Show all workspace/project symbols

![](images/features/workspace-symbols.gif)

### Show all symbols on current file

![](images/features/document-symbols.gif)

### Rename symbols

![](images/features/rename.gif)

Also, it's possible to live rename symbols on the same buffer with `linkedEditingRange` feature.

### Document highlight on hover showing symbol usages

![](images/features/document-highlight.gif)

### Documentation and clojuredocs integration

![](images/features/hover-clojuredocs.png)

### Java support

To understand what is supported and how to configure it, check the [settings section](settings.md#java-support).

![](images/features/java-find-definition.gif)

### Completion

![](images/features/completion.gif)

#### Snippets

[Snippets](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax) are templates that make it easier to enter repeating code patterns, such as common functions/forms, they are available during completion. Tabstops are defined as `$number` with `$0` as last tabstop.

<details>
<summary><strong>Check all available snippets here</strong></summary>

<table>
<thead>
    <tr>
        <th>name</th>
        <th>description</th>
        <th>raw content</th>
    </tr>
</thead>
<tbody>
    <tr>
        <td><code>comment</code></td>
        <td>Insert comment block</td>
        <td><code>(comment\n  ${0:body}\n  )</code></td>
    </tr>
    <tr>
        <td><code>comment-heading</code></td>
        <td>Insert comment Header</td>
        <td><code>;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; ${1:Namespace summary title}\n;;\n;; ${2:Brief description}\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n$0</code></td>
    </tr>
    <tr>
        <td><code>comment-separator</code></td>
        <td>Insert comment separator</td>
        <td><code> ;; ${1:Namespace summary title}\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n$0</code></td>
    </tr>
    <tr>
        <td><code>rich-comment</code></td>
        <td>Insert rich comment</td>
        <td>
<code>
(comment
  $0
#_())
</code></td>
    </tr>
    <tr>
        <td><code>rich-comment-rdd</code></td>
        <td>Insert rich comment rdd block</td>
        <td>
<code>#_{:clj-kondo/ignore [:redefined-var]}\n(comment\n $0 #_())</code></td>
    </tr>
    <tr>
        <td><code>rich-comment-hotload</code></td>
        <td>Insert rich comment library hotload</td>
        <td>
<code>#_{:clj-kondo/ignore [:redefined-var]}\n(comment\n ;; Add-lib library for hot-loading\n (require '[clojure.tools.deps.alpha.repl :refer [add-libs]])\n (add-libs '{${1:domain/library-name} {:mvn/version \"${2:1.0.0}\"}$3})\n $0\n) </code></td>
    </tr>
    <tr>
        <td><code>condp</code></td>
        <td>Insert condp</td>
        <td><code>(condp ${1:pred} ${2:expr}\n $0)</code></td>
    </tr>
    <tr>
        <td><code>def</code></td>
        <td>Insert def</td>
        <td><code>(def ${1:name} $0)</code></td>
    </tr>
    <tr>
        <td><code>def-</code></td>
        <td>Insert def private</td>
        <td><code>(def ^:private ${1:name} $0)</code></td>
    </tr>
    <tr>
        <td><code>def-doc</code></td>
        <td>Insert def with docstring</td>
        <td><code>(def ${1:name}\n  \"${2:docstring}\"\n  $0)</code></td>
    </tr>
    <tr>
        <td><code>defmethod</code></td>
        <td>Insert defmethod</td>
        <td><code>(defmethod ${1:name} ${2:match}\n [${3:args}]\n $0)</code></td>
    </tr>
    <tr>
        <td><code>defmulti</code></td>
        <td>Insert defmulti</td>
        <td><code>(defmulti ${1:name} ${2:dispatch-fn})</code></td>
    </tr>
    <tr>
        <td><code>defn</code></td>
        <td>Insert public defn</td>
        <td><code>(defn ${1:name} [$2]\n  $0)</code></td>
    </tr>
    <tr>
        <td><code>defn-doc</code></td>
        <td>Insert public defn with docstring</td>
        <td><code>(defn ${1:name}\n  \"${2:docstring}\"\n   [${3:args}]\n  $0)</code></td>
    </tr>
    <tr>
        <td><code>defn-</code></td>
        <td>Insert private defn</td>
        <td><code>(defn%s ${1:name} [$2]\n  $0)</code></td>
    </tr>
    <tr>
        <td><code>defprotocol</code></td>
        <td>Insert defprotocol</td>
        <td><code>(defprotocol ${1:Name}\n $0)</code></td>
    </tr>
    <tr>
        <td><code>defrecord</code></td>
        <td>Insert defrecord</td>
        <td><code>(defrecord ${1:Name} [${2:fields}]\n ${3:Protocol}\n $0)</code></td>
    </tr>
    <tr>
        <td><code>deftype</code></td>
        <td>Insert deftype</td>
        <td><code>(deftype ${1:Name} [${2:fields}]\n ${3:Protocol}\n $0)</code></td>
    </tr>
    <tr>
        <td><code>fn</code></td>
        <td>Insert fn</td>
        <td><code>(fn [${1:arg-list}] $0)</code></td>
    </tr>
    <tr>
        <td><code>for</code></td>
        <td>Insert for</td>
        <td><code>(for [${1:item} ${2:coll}]\n  $0)</code></td>
    </tr>
    <tr>
        <td><code>if</code></td>
        <td>Insert if</td>
        <td><code>(if ${1:test-expr}\n ${2:then-expr}\n ${3:else-expr})</code></td>
    </tr>
    <tr>
        <td><code>kwargs</code></td>
        <td>Insert keyword args</td>
        <td><code>{:keys [${1:keys}] :or {${2:defaults}}}</code></td>
    </tr>
    <tr>
        <td><code>let</code></td>
        <td>Insert let</td>
        <td><code>(let [$0])</code></td>
    </tr>
    <tr>
        <td><code>letfn</code></td>
        <td>Insert letfn</td>
        <td><code>(letfn [(${1:name} [${2:args}]\n $0)])</code></td>
    </tr>
    <tr>
        <td><code>ns</code></td>
        <td>Insert ns</td>
        <td><code>(ns ${1:name}\n  $0:references})</code></td>
    </tr>
    <tr>
        <td><code>ns-doc</code></td>
        <td>Insert ns with docstring</td>
        <td><code>(ns ${1:name}\n  \"${2:docstring}\"\n  ${0:references})</code></td>
    </tr>
    <tr>
        <td><code>require</code></td>
        <td>Insert ns :require</td>
        <td><code>(:require [${1:namespace}])$0</code></td>
    </tr>
    <tr>
        <td><code>require-as</code></td>
        <td>Insert ns require with :as alias</td>
        <td><code>(:require [${1:namespace} :as ${2:alias}]$3)</code></td>
    </tr>
    <tr>
        <td><code>require-refer</code></td>
        <td>Insert ns :require with :refer</td>
        <td><code>(:require [${1:namespace} :refer [$2]]$3)</code></td>
    </tr>
    <tr>
        <td><code>require-rdd</code></td>
        <td>Insert require for rich comment experiments</td>
        <td><code>(require '[${1:namespace} :as ${2:alias}]$3)$0</code></td>
    </tr>
    <tr>
        <td><code>req-as</code></td>
        <td>Insert single require dep :as alias</td>
        <td><code>[${1:namespace} :as ${2:alias}]</code></td>
    </tr>
    <tr>
        <td><code>req-refer</code></td>
        <td>Insert single require dep with :refer</td>
        <td><code>[${1:namespace} :refer [$2]]</code></td>
    </tr>
    <tr>
        <td><code>import</code></td>
        <td>Insert import</td>
        <td><code>(:import [${1:package}])</code></td>
    </tr>
    <tr>
        <td><code>use</code></td>
        <td>Insert require refer preferred over use</td>
        <td><code>(:require [${1:namespace} :refer [$2]])</code></td>
    </tr>
    <tr>
        <td><code>deps-alias</code></td>
        <td>Insert alias with extra path & deps</td>
        <td>
<code>:${1:category/name}\n {:extra-paths [\"${2:path}\"]\n :extra-deps {${3:deps-maven or deps-git}}}$0</code></td>
    </tr>
    <tr>
        <td><code>deps-maven</code></td>
        <td>Insert maven dependency</td>
        <td><code>${1:domain/library-name} {:mvn/version \"${2:1.0.0}\"}$0</code></td>
    </tr>
    <tr>
        <td><code>deps-git</code></td>
        <td>Insert git dependency</td>
        <td>
<code>${1:domain/library-name}\n {:git/sha \"${2:git-sha-value}\"}$0 </code></td>
    </tr>
    <tr>
        <td><code>deps-git-tag</code></td>
        <td>Insert git tag dependency</td>
        <td>
<code>${1:domain/library-name}\n {:git/tag \"${2:git-tag-value}\"\n :git/sha \"${3:git-sha-value}\"}$0</code></td>
    </tr>
    <tr>
        <td><code>deps-git-url</code></td>
        <td>Insert git URL dependency</td>
        <td>
<code>${1:domain/library-name}\n {:git/url \"https://github.com/$1\"\n :git/sha \"${2:git-sha-value}\"}$0</code></td>
    </tr>
    <tr>
        <td><code>deps-local</code></td>
        <td>Insert local dependency</td>
        <td>
<code> ${1:domain/library-name} {:local/root \"${2:/path/to/project/root}\"}$0 </code></td>
    </tr>
    <tr>
        <td><code>deftest</code></td>
        <td>Insert deftest clojure.test</td>
        <td>
<code>(deftest ${1:name}-test\n  (testing \"${2:Context of the test assertions}\"\n  (is (= ${3:assertion-values}))$4)) $0</code></td>
    </tr>
    <tr>
        <td><code>testing</code></td>
        <td>Insert testing clojure.test</td>
        <td>
<code>(testing \"${1:Context of the test assertions}\"\n  $0)</code></td>
    </tr>
    <tr>
        <td><code>is</code></td>
        <td>Insert is clojure.test</td>
        <td><code>(is (= ${1:assertion-values}))</code></td>
    </tr>
</tbody>
</table>

</details>

##### Custom snippets

User can register additional custom snippets. For more information, check the [snippets config documentation](settings.md#snippets).

### Code actions

See [below](#clojure-lsp-extra-commands) for screenshots.

### Code lenses showing symbol references

![](images/features/code-lens.png)

### Format a whole file or range

![](images/features/format-buffer.gif)

### Signature help

![](images/features/signature-help.gif)

### Semantic tokens

The LSP server is the best to say what is the semantic value of a token on the editor, semantic tokens allows server return to client all tokens of a buffer and how client show apply highlight.

Note: server return the semantic token (e.g. `function`) and the client/editor apply the color that matches the user's theme.

![](images/features/semantic-tokens.png)

### Call hierarchy

Show the incoming or outgoing call hierarchy of a function/variable as a lazy tree

##### Incoming

Show functions that call the current one recursively

![](images/features/call-hierarchy-incoming.png)

##### Outgoing

Show functions that the current one call, recursively

![](images/features/call-hierarchy-outgoing.png)

### Test Tree

Show the tests tree hierarchy of a file

![](images/features/test-tree.png)

### Project tree

Show the project tree ns and external dependencies.

![](images/features/project-tree.png)

### Diagnostics (linter)

All linters besides the ones below come from [clj-kondo](https://github.com/clj-kondo/clj-kondo) that clojure-lsp calls under the hood to lint the code and retrieve the analysis to
make most of features work.

Every linter configuration should be done on clj-kondo side, so anything related to unresolved symbols or unknown macros are probably related to wrong clj-kondo for the project. For more information on how to configure clj-kondo check [here](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#unrecognized-macros).

Below you can find the custom linters implemented on clojure-lsp side that uses the `:custom-lint-fn` from clj-kondo:

#### clojure-lsp/unused-public-var

![](images/features/unused-public-var.png)

For more information on how to configure it, check the [diagnostics settings section](settings.md#diagnostics-linter).

#### clojure-lsp/uniform-aliasing

For more information on how to configure it, check the [diagnostics settings section](settings.md#diagnostics-linter).

#### clj-depend

Clojure-lsp has a optional integration with [clj-depend](https://github.com/clj-depend/clj-depend), making it easier to configure namespaces relationship.

For more information on how to configure it, check the [diagnostics settings section](settings.md#diagnostics-linter).

### Stub generation

It's possible to configure clojure-lsp to generate and analyze stubs for specific namespaces available on your project classpath, this is useful for closed source dependencies like `datomic.api`, with that clojure-lsp will be able to make most features work with those dependencies.
For more information check the [stubs settings section](settings.md#stub-generation).

### Execute command

Commands that client can request. Most code actions use these commands as actions.

## clojure-lsp extra commands

__Note: Most of these are available via code actions and clients or users may choose to use the code action menu to execute them instead of mapping.__

All functions in clojure-lsp have a two-letter mnemonic shortcut. E.g. `tf` for `thread-first-all`. We __strongly__ suggest that client authors and users use these shortcuts keys when choosing to map these commands to key chords or menus; this allows for users to maintain muscle memory and familiarity as they switch between clients and editors. Thank you to [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el#usage) for this system and other tools that adhere to it.

LSP clients will allow you to bind these commands to shortcuts or menu items using LSP's `workspace/executeCommand` and passing a map with `"command"` and `"arguments"` keys.

Arguments:
- `file-uri`: Absolute file uri. e.x. `file:///home/user/project/src/main.clj`
- `row`: line-number of the cursor, 0 based.
- `col`: col-number of the cursor, 0 based.
- `name`: Used when introducing a name, usually a string for a valid clojure symbol.
- `filename`: Filename path. e.x. `src/utils.clj`

| Shortcut | Command                      | Name                                      | Arguments                                        | Available via code action | Example                                                                                                                                                   |
|----------|------------------------------|-------------------------------------------|--------------------------------------------------|---------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| ab       | drag-param-backward          | Drag param backward                       | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Drag param forward and backward](images/features/drag-param.gif)](images/features/drag-param.gif)                                  |
| af       | drag-param-forward           | Drag param forward                        | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Drag param forward and backward](images/features/drag-param.gif)](images/features/drag-param.gif)                                  |
| ai       | add-missing-import           | Add import to namespace                   | `[file-uri,row,col[,name]]`                      | ✅                        | [![Screen recording of Add import to namespace](images/features/add-common-import.gif)](images/features/add-common-import.gif)                            |
| am       | add-missing-libspec          | Add missing require                       | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Add missing require](images/features/add-missing-require.gif)](images/features/add-missing-require.gif)                            |
| as       | add-require-suggestion       | Add require suggestion                    | `[file-uri,row,col,ns,alias,refer]`              | ✅                        | [![Screen recording of Add require suggestion](images/features/add-alias-suggestion.gif)](images/features/add-alias-suggestion.gif)                       |
| cc       | cycle-coll                   | Cycle collection `(#{}, {}, [], ())`      | `[file-uri,row,col]`                             | ✅                        |                                                                                                                                                           |
| ck       | cycle-keyword-auto-resolve   | Cycle keyword auto-resolve                | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Cycle keyword auto resolve](images/features/cycle-keyword-auto-resolve.gif)](images/features/cycle-keyword-auto-resolve.gif)       |
| cn       | clean-ns                     | Clean namespace                           | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Clean namespace](images/features/clean-ns.gif)](images/features/clean-ns.gif)                                                      |
| cp       | cycle-privacy                | Cycle privacy of def/defn                 | `[file-uri,row,col]`                             | ✅                        |                                                                                                                                                           |
| ct       | create-test                  | Create test                               | `[file-uri,row,col]`                             | ✅                        |                                                                                                                                                           |
| df       | demote-fn                    | Demote fn to #()                          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Promote fn to defn](images/features/demote-fn.gif)](images/features/demote-fn.gif)                                                 |
| db       | drag-backward                | Drag backward                             | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Drag forward and backward](images/features/drag.gif)](images/features/drag.gif)                                                    |
| df       | drag-forward                 | Drag forward                              | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Drag forward and backward](images/features/drag.gif)](images/features/drag.gif)                                                    |
| dk       | destructure-keys             | Destructure keys                          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Destructure keys](images/features/destructure-keys.gif)](images/features/destructure-keys.gif)                                     |
| ed       | extract-to-def               | Extract to def                            | `[file-uri,row,col,name]`                        | ✅                        | [![Screen recording of Extract to def](images/features/extract-to-def.gif)](images/features/extract-to-def.gif)                                           |
| ef       | extract-function             | Extract function                          | `[file-uri,row,col,name]`                        | ✅                        | [![Screen recording of Extract function](images/features/extract-function.gif)](images/features/extract-function.gif)                                     |
| el       | expand-let                   | Expand let                                | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| fe       | create-function              | Create function from example              | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Create function from example](images/features/create-private-function.gif)](images/features/create-private-function.gif)           |
| ga       | get-in-all                   | Move all expressions to get/get-in        | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Threading get](images/features/thread-get.gif)](images/features/thread-get.gif)                                                    |
| gl       | get-in-less                  | Remove one element from get/get-in        | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Threading get](images/features/thread-get.gif)](images/features/thread-get.gif)                                                    |
| gm       | get-in-more                  | Move another expression to get/get-in     | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Threading get](images/features/thread-get.gif)](images/features/thread-get.gif)                                                    |
| gn       | get-in-none                  | Unwind whole get/get-in                   | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Threading get](images/features/thread-get.gif)](images/features/thread-get.gif)                                                    |
| il       | introduce-let                | Introduce let                             | `[file-uri,row,col,name]`                        |                           |                                                                                                                                                           |
| is       | inline-symbol                | Inline Symbol                             | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Inline symbol](images/features/inline-symbol.gif)](images/features/inline-symbol.gif)                                              |
| ma       | resolve-macro-as             | Resolve macro as                          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Resolve macro as](images/features/resolve-macro-as.gif)](images/features/resolve-macro-as.gif)                                     |
| mf       | move-form                    | Move form                                 | `[file-uri,row,col,filename]`                    | ✅                        |                                                                                                                                                           |
| ml       | move-to-let                  | Move expression to let                    | `[file-uri,row,col,name]`                        | ✅                        | [![Screen recording of Move expression to let](images/features/move-to-let.gif)](images/features/move-to-let.gif)                                         |
| pf       | promote-fn                   | Promote #() to fn, or fn to defn          | `[file-uri,row,col,fn-name]`                     | ✅                        | [![Screen recording of Promote fn to defn](images/features/promote-fn.gif)](images/features/promote-fn.gif)                                               |
| rr       | replace-refer-all-with-refer | Replace ':refer :all' with ':refer [...]' | `[file-uri,row,col,refers]`                      | ✅                        | [![Screen recording of Replace refer all with refer](images/features/replace-refer-all-with-refer.gif)](images/features/replace-refer-all-with-refer.gif) |
| ra       | replace-refer-all-with-alias | Replace ':refer :all' with alias          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Replace refer all with alias](images/features/replace-refer-all-with-alias.gif)](images/features/replace-refer-all-with-alias.gif) |
| rk       | restructure-keys             | Restructure keys                          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Restructure keys](images/features/restructure-keys.gif)](images/features/restructure-keys.gif)                                     |
| sc       | change-coll                  | Switch collection to `{}, (), #{}, []`    | `[file-uri,row,col,"map"/"list"/"set"/"vector"]` | ✅                        | [![Screen recording of Switch collection](images/features/change-coll.gif)](images/features/change-coll.gif)                                              |
| sl       | sort-clauses                 | Sort map/vector/list/set/clauses          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Sort clauses](images/features/sort-clauses.gif)](images/features/sort-clauses.gif)                                                 |
| tf       | thread-first-all             | Thread first all                          | `[file-uri,row,col]`                             | ✅                        | [![Screen recording of Thread first all](images/features/thread-first-all.gif)](images/features/thread-first-all.gif)                                     |
| th       | thread-first                 | Thread first expression                   | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| tl       | thread-last-all              | Thread last all                           | `[file-uri,row,col]`                             | ✅                        |                                                                                                                                                           |
| tt       | thread-last                  | Thread last expression                    | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| ua       | unwind-all                   | Unwind whole thread                       | `[file-uri,row,col]`                             | ✅                        |                                                                                                                                                           |
| uw       | unwind-thread                | Unwind thread once                        | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| fs       | forward-slurp                | Paredit: forward slurp                    | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| fb       | forward-barf                 | Paredit: forward barf                     | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| bs       | backward-slurp               | Paredit: backward slurp                   | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| bb       | backward-barf                | Paredit: backward barf                    | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| rs       | raise-sexp                   | Paredit: Raise sexp                       | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |
| ks       | kill-sexp                    | Paredit: Kill sexp                        | `[file-uri,row,col]`                             |                           |                                                                                                                                                           |


## Dev

##### Server information

Return basic information about the server.

##### Cursor information

Return debug information about the element at point.
