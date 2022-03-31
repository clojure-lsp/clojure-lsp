# Features

Below you can find all available features that clojure-lsp provides with examples using [Emacs lsp-mode](https://emacs-lsp.github.io/lsp-mode/) as the client.

- [Built in LSP features](#lsp-features)
- [clojure-lsp extra commands](#clojure-lsp-extra-commands)


## LSP features

### Find a function/var definition

<img src="../images/features/find-definition.gif" width=340>

### Find defprotocol/defmulti implementations

<img src="../images/features/find-implementation.gif" width=340>

### Find a function/var declaration in the ns

<img src="../images/features/find-declaration.gif" width=340>

### Find all references of a function, var, keyword or namespace alias

<img src="../images/features/find-references.gif" width=340>

### Show all workspace/project symbols

<img src="../images/features/workspace-symbols.gif" width=580>

### Show all symbols on current file

<img src="../images/features/document-symbols.gif" width=580>

### Rename symbols

<img src="../images/features/rename.gif" width=340>

Also, it's possible to live rename symbols on the same buffer with `linkedEditingRange` feature.

### Document highlight on hover showing symbol usages

<img src="../images/features/document-highlight.gif" width=340>

### Documentation and clojuredocs integration

<img src="../images/features/hover-clojuredocs.png">

### Java support

To understand what is supported and how to configure it, check the [settings section](../settings/#java-support).

<img src="../images/features/java-find-definition.gif" width=580>

### Completion

<img src="../images/features/completion.gif" width=340>

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

User can register additional custom snippets, for more information on how to configure it, check the [snippets settings section](https://clojure-lsp.io/settings/#snippets).

### Code actions

See [below](#clojuser-lsp-extra-commands) for screenshots.

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

### Diagnostics (linter)

All linters besides the ones below come from [clj-kondo](https://github.com/clj-kondo/clj-kondo) that clojure-lsp calls under the hood to lint the code and retrieve the analysis to
make most of features work.

Every linter configuration should be done on clj-kondo side, so anything related to unresolved symbols or unknown macros are probably related to wrong clj-kondo for the project. For more information on how to configure clj-kondo check [here](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#unrecognized-macros).

Below you can find the custom linters implemented on clojure-lsp side that uses the `:custom-lint-fn` from clj-kondo:

#### clojure-lsp/unused-public-var

![](images/features/unused-public-var.png)

For more information on how to configure it, check the [diagnostics settings section](https://clojure-lsp.io/settings/#diagnostics-linter).

### Stub generation

It's possible to configure clojure-lsp to generate and analyze stubs for specific namespaces available on your project classpath, this is useful for closed source dependencies like `datomic.api`, with that clojure-lsp will be able to make most features work with those dependencies.
For more information check the [stubs settings section](../settings.md#stub-generation).

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

| Shortcut | Command                 | Name                                   | Arguments                                        | Available via code action | Example                                                              |
|----------|-------------------------|----------------------------------------|--------------------------------------------------|---------------------------|----------------------------------------------------------------------|
| ai       | add-import-to-namespace | Add import to namespace                | `[file-uri,row,col[,name]]`                      | ✅                        | <img src="../images/features/add-common-import.gif" width=360>       |
| am       | add-missing-libspec     | Add missing require                    | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/add-missing-require.gif" width=360>     |
| as       | add-require-suggestion  | Add require suggestion                 | `[file-uri,row,col,ns,alias,refer]`              | ✅                        | <img src="../images/features/add-alias-suggestion.gif" width=360>    |
| cc       | cycle-coll              | Cycle collection `(#{}, {}, [], ())`   | `[file-uri,row,col]`                             | ✅                        |                                                                      |
| cf       | cycle-fn-literal        | Cycle fn literal `(fn []), #()`        | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/cycle-fn-literal.gif" width=480>        |
| cn       | clean-ns                | Clean namespace                        | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/clean-ns.gif" width=360>                |
| cp       | cycle-privacy           | Cycle privacy of def/defn              | `[file-uri,row,col]`                             | ✅                        |                                                                      |
| ct       | create-test             | Create test                            | `[file-uri,row,col]`                             | ✅                        |                                                                      |
| ef       | extract-function        | Extract Function                       | `[file-uri,row,col,name]`                        | ✅                        | <img src="../images/features/extract-function.gif" width=360>        |
| el       | expand-let              | Expand let                             | `[file-uri,row,col]`                             |                           |                                                                      |
| fe       | create-function         | Create function from example           | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/create-private-function.gif" width=360> |
| il       | introduce-let           | Introduce let                          | `[file-uri,row,col,name]`                        |                           |                                                                      |
| is       | inline-symbol           | Inline Symbol                          | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/inline-symbol.gif" width=360>           |
| ma       | resolve-macro-as        | Resolve macro as                       | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/resolve-macro-as.gif" width=720>        |
| md       | move-coll-entry-down    | Move clause down                       | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/move-coll-entry.gif" width=720>         |
| mf       | move-form               | Move form                              | `[file-uri,row,col,filename]`                    | ✅                        |                                                                      |
| ml       | move-to-let             | Move expression to let                 | `[file-uri,row,col,name]`                        | ✅                        | <img src="../images/features/move-to-let.gif" width=720>             |
| mu       | move-coll-entry-up      | Move clause up                         | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/move-coll-entry.gif" width=720>         |
| sc       | change-collection       | Switch collection to `{}, (), #{}, []` | `[file-uri,row,col,"map"/"list"/"set"/"vector"]` | ✅                        | <img src="../images/features/change-coll.gif" width=720>             |
| sm       | sort-map                | Sort map                               | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/sort-map.gif" width=720>                |
| tf       | thread-first-all        | Thread first all                       | `[file-uri,row,col]`                             | ✅                        | <img src="../images/features/thread-first-all.gif" width=360>        |
| th       | thread-first            | Thread first expression                | `[file-uri,row,col]`                             |                           |                                                                      |
| tl       | thread-last-all         | Thread last all                        | `[file-uri,row,col]`                             | ✅                        |                                                                      |
| tt       | thread-last             | Thread last expression                 | `[file-uri,row,col]`                             |                           |                                                                      |
| ua       | unwind-all              | Unwind all                             | `[file-uri,row,col]`                             | ✅                        |                                                                      |
| uw       | unwind-thread           | Unwind thread                          | `[file-uri,row,col]`                             |                           |                                                                      |


## Dev

##### Server information

Return basic information about the server.

##### Cursor information

Return debug information about the element at point.
