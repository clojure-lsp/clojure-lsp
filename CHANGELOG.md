# Changelog

## Unreleased

## 2023.05.04-19.38.01

- General
  - Bump clj-kondo to `2023.04.15-20230503.152749-8`

- Editor
  - Fix edn tree to consider symbols. #1556
  - Fix edn tree to consider vector root nodes. #1557
  - Fix edn tree to handle invalid edn code and not throw exceptions while typing code.
  
- API/CLI
  - Avoid enabling unecessary analysis features for API/CLI lint, improving memory usage.

## 2023.04.19-12.43.29

- General
  - Fix reflection issue in JDK19 with `Thread/sleep` #1520
  - Bump clj-kondo to `2023.04.15-20230418.173453-3`, fixing analysis inconsistencies with `schema.core`
  - Ignore vars defined wrongly via config. #1510
  - Add support for `:output {:langs true}` in clj-kondo config to show `.cljc` language contexts

- Editor
  - Fix classpath issue message to properly ignore or retry after user input. #1500
  - Decreate debounce time for calculating changes and diagnostics, improving UX in cost of performance, it doesn't seem to highly affect performance though.
  - Add new setting to change diagnostics range type. #1530
  - Spec compliance: avoid registering client capabilities if dynamic registration is not set. #1535
  - Improve java support: using new `java-member-definitions` analysis, first feature is the allow completion of static members (fields and methods) for java classes.
  - Show edn tree when in edn files via `documentSymbol` feature.
  - Improve java db cache consistency + analysis performance.

## 2023.02.27-13.12.12

- Editor
  - Fix exception on hover feature on graal images.

## 2023.02.26-23.46.05

- General
  - Add new setting `:paths-ignore-regex` to allow ignore analyzing/linting specific paths, useful if you have folders or files that are not relevant to be linted and are huge which may affect performance.
  - Bump clj-kondo. Fixes missing lint class constructors, making clojure-lsp show add import code action always.
  - Fix wrong ns rename for cljc files. #1508

- Editor
  - Fix cache issue: wrong external dependencies analysis when running clojure-lsp in editor after running on CLI, affecting navigation. #1473
  - Bump lsp4clj fixing progress notifications during initialization for Calva.
  - Allow go to definition of namespace even when the var is not known. Ex: `clojure.string/foo` will go to the definition of `clojure.string`. This is useful for cases where the var was not created yet but user wants to go to the ns to check the available functions or check the correct name of the function.
  - Avoid basing results on old analysis.
  - Add new setting `:completion :analysis-type` to choose between `:fast-but-stale`(default) or `:slow-but-accurate`, this should define whether completion should wait for changes that may still happening, this by default reverts the behavior introduced after #1425. #1487
  - Fix `textDocument/hover` issue when doc metadata isn't a string literal.
  - Follow references to other vars in doc metadata for use in `textDocument/hover`.
  - Support `:arglists` meta when finding docs of functions via hover feature. #1504
  - Enhance hover to search clojuredocs on CLJS. #1506

## 2023.01.26-11.08.16

- General
  - Bump clj-kondo to `2023.01.20`
  - Avoid false-positives of unused-public-var for functions used in :gen-class that starts with `-` as convention. #1443
  - New jar available on releases: `clojure-lsp-server.jar`, an Uberjar not aot-ed containing both clojure-lsp core + server code, improving startup time for JVM clients, for example for `lein-clojure-lsp`. Any lib using `clojure-lsp-standalone.jar` should consider switch to this new jar.
  - Change `:source-paths-ignore-regex` default value to `["target.*"]`, removing resource folders. #1452
  - Bump cljfmt to 0.9.2
  - Bump lsp4clj to 1.7.0
  - Support `:style/indent` metadata for indentation with cljfmt #1420

- Editor
  - Fix add missing import code action when there are multiple options. #1422
  - Only show completion items if no changed code is being processed, avoiding wrong completion items. #1425
  - Improve semantic tokens for defprotocol, definterface coloring as `interface` tokens.
  - Include reffered vars in completion. #1440
  - Allow rename alias of function calls, renaming properly both function usages and alias definition.
  - Add support for keyword analysis in edn files, allowing find-references, document highlight and other lsp features to keywords.
  - find-definition and references for quoted symbols like `'clojure.core/inc` and symbols mentioned in `.edn` files. #1446
  - Show error messages when can't apply edits after commands. #1448

- API/CLI
  - Make diagnostics command print as json or edn. #1419
  - Make possible starting a chan server with clojure-lsp components, useful to other clients extend.
  - Consider edn files when linting project, allowing to format edn files. #1452
  - Report diagnostics lines and columns as 1 based, not 0 like LSP, following clj-kondo standard. #1459

## 2022.12.09-15.51.10

- General
  - Fix rename issue with VS-Code/Calva on MS-Windows. #1388
  - Only publish progress on initialize if client provided a `workDoneProgress`. #1363
  - Bump clj-kondo to `2022.12.08`.
  - Avoid wrong clj-kondo configs in case clojure-lsp process is spawned from a different directory than project-root.
  - Fall back on CLJS var when finding definition from Clojure and nothing was found. #1403

- Editor
  - Show better icons for multimethods, var-arg fns, protocols, records, interfaces and types on `workspace/symbol` and `textDocument/documentSymbol`.
  - Inlining the last binding of a let removes the let. #210
  - Allow import java classes via code-actions `Add import 'x.y.Z'`.
  - Make `add-import` command/code-action smarter, checking if there is already a package import to just include the classname instead of full import.
  - Add new refactoring `cycle-keyword-auto-resolve`, e.g. `::bar` -> `:foo/bar` and vice-versa. #1128
  - Supporting find definition/references/implementations of `definterface`.
  - Avoid watching ignored source-paths, causing find-definition/references and other features go to wrong files.

- API/CLI
  - Allow specify analysis type for `dump` command, default to analyze project only analysis. #1383
  - Add `:result` to the dump command. #1390
  - Important regression fix where clojure-lsp may do inconsistent lint when classpath scan fail without showing to user the error.

## 2022.11.03-00.14.57

- General
  - Bump clj-kondo to `2022.11.02` avoiding breaking changes on custom hooks.
  - Bump graalvm to 22.3.0. #1345

- Editor
  - Add refactorings to change param order of `defn`/`defmacro`, also changing call sites. #1131
  - Avoid shadowing existing locals when restructuring keys. #1229
  - Let editors control whether the server's log includes traces of the messages they are exchanging. https://github.com/clojure-lsp/lsp4clj/issues/27
  - Bump promesa to `9.0.470`
  - Bump lsp4clj to `1.5.0`
  - For users with fewer cores, avoid unnecessary waits for file analysis.
  - Reduce CPU usage by aborting requests that the client won't use.
  - Fix to mark some code actions as preferred, so editors can emphasize them. https://github.com/clojure-lsp/lsp4clj/issues/32
  - Adds java classes completions, since clj-kondo now knows all java classes defined on external dependencies. #1329
  - Fix completion to consider required namespaces without alias or refers. #1352

## 2022.10.05-16.39.51

- General
  - Improve clj-depend merge config to overwrite `source-paths` only if it is nil or empty.  #1264
  - Fix stubs generation issue on MS-Windows, coming out of enabling all integrations tests on windows. #1211
  - Improve MS-Windows support by fixing various path, URI and line ending issues coming out of repairing the unit tests suite on windows. #1211
  - End dep-graph-queries experiment; clojure-lsp now uses the dep-graph to optimize queries whenever possible
  - Bump clj-kondo to `2022.10.05`. #1226 
  - Fix issue with changes being reporting with spurious and incorrect line endings on MS-Windows text files. #1211
  - Index internal data by URI instead of filename, to minimize conversion between these formats when running queries. #1207
  - Add support to enable trace logs on server via `--trace` flag. (For latest Emacs's lsp-mode this can be enabled easyly via `lsp-clojure-trace-enable` variable)
  - Bump graalvm version for MS-Windows to 22.2.0, in sync with the other archs. #1211

- Editor
  - Fix to avoid error when checking code actions from an #_x uneval node. #1227
  - Add support to decompile jar as a java project when finding the definition of a java class, allowing to have LSP features on that java project. #1187
  - Add refactorings similar to `Sort map`: `Sort vector`, `Sort list`, `Sort set`, and `Sort clauses` for functions like `assoc`. #1155
  - Support java class decompilation for zipfile scheme (vim users).
  - Avoid keeping diagnostics of external closed files for Calva. https://github.com/BetterThanTomorrow/calva/issues/1864
  - Lint opened files after a clojure-lsp or clj-kondo config file is saved on disk, avoiding users to re-edit files. #1247
  - Allow find definition of java class usages where definition comes from clojure, like defrecords.
  - Fix: wait for rename to apply before allowing another rename, to ensure suggested name is correct. #1270
  - Process requests in parallel, to prevent typing lag and other performance problems introduced during migration away from lsp4j. #1240
  - Fix: Avoid wrong ns require after `Create ns and require` code-action/command.
  - Fix: Avoid errors when a file starts with a comment. #1252
  - Bump promesa to `9.0.462` and use it for parallel request processing.

- API/CLI
  - Fix missing diagnostics when `--project-root` is different than current directory. #1245
  - Add new `dump` feature, returning analysis, project information, dependency-graph and others. Check `clojure-lsp.api/dump` for more information. #744
  - Improve CLI output avoiding logs from clj-kondo which are already available via verbose log.

## 2022.09.01-15.27.31

- General
  - Remove dependency on lsp4j. https://github.com/clojure-lsp/lsp4clj/issues/8
  - Fix extra space on :import when sorting classes. #1152
  - Bump Graalvm from 22.0.0.2 to 22.2.0 improving binary performance/size. #1154
  - Bump clj-kondo to `2022.08.03`.
  - General release of dep-graph-queries, improving query performance. See prior release for feature details.
  - Generate nightly builds for macos aarch64 (M1/M2) every push on master.
  - Bump lsp4clj to `1.2.1`.
  - Bump cljfmt to `0.9.0`.
  - Fix issue with classpath clojure build tools invocation on MS-Windows. #1132
  - Bump babashka/fs to `0.1.11`.

- Editor
  - Improve completion sorting, showing locals before functions and other completion items. #1158
  - Fix hover to show current var definition docs instead of `def`/`defn`. #1157
  - Improve hover for requires to show ns docstring. #1171
  - Fix destructuring of things that have been destructured before. #1156
  - Some completions require that a new alias be added to the ns form. Fix this feature for Calva users, and improve performance for all users. #1068
  - Fix resolve-macro-as code action corner case. #1084
  - Ensure line numbers provided by document-symbol correspond to the latest version of the file. #1178
  - Avoid exceptions when clients use URIs that don't exist on disk.
  - Fix documentation on completion items. #1181
  - Fix rename of defrecords. #1165
  - Fix to send all diagnostics to client at startup, even in very large projects. #1153
  - Fix to preserve kebab-casing in server-info-raw. #1195
  - Add refactoring `Restructure keys`, the inverse of `Destructure keys`. #1170
  - Add refactorings to convert between `(:x m)` and `(get m :x)` or `(:y (:x m))` and `(get-in m [:x :y])`. #1172
  - Add support to imported java class on completion. #1193
  - Add new question to skip or retry classpath scan during startup if failed. 
  - Improve performance of processing of changed files outside editor calling clj-kondo in batch. #1205
  - When renaming a keyword that is also a destructured key, rename its local usages too. #1192

## 2022.07.24-18.25.43

- General
  - Bump `cljfmt` from 0.8.0 -> 0.8.2

- Editor
  - Add clojure.core/fn to resolve-macro-as options. [#1094](https://github.com/clojure-lsp/clojure-lsp/issues/1094)
  - Restore #995 Improve element selected on `textDocument/hover` (previously reverted) with a fix that keeps it working for Calva even after a syntax error is introduced.
  - Improve call hierarchy performance by parsing less frequently. #1092
  - Improve system wide performance by keeping a graph of dependencies between namespaces. #990 #1053
    Enable setting `:experimental {:dep-graph-queries true}` to beta test this feature.
  - Improve performance by adding second level of analysis indexing.
  - Improve performance of things that need keyword definitions, like completion and custom lint.
  - Fix hover showing previous function elements on some cases. #1098
  - Fix: find definition will find registration of unnamespaced keyword.
  - Fix to update unused-public-var lint on registered keywords as usages change in other files. #1018
  - Fix to navigate to var defined by declare, when there aren't any later defs. #1107
  - Fix to always go to the definition of the correct var imported by potemkin. #1020
  - Fix to correctly rename namespaces. #1121
  - Fix to correctly :keep require and import indent spacing. #1141
  - Improve accuracy of progress reporting during uncached startup. #1134
  - Add refactoring `Destructure keys` to destructure keywords. #905
  - Add refactoring `Extract to def` to create a `def` from the thing under the cursor. #1136
  - Include defmethods in document and workspace symbols. #1016

## 2022.06.29-19.32.13

- Editor
  - Reverted #995 Improve element selected on `textDocument/hover`, as it caused clojure-lsp to stop working for Calva users after a syntax error was introduced. #1080

## 2022.06.22-14.09.50

- General
  - clojure-lsp has a flake.nix now, being able to be built using clj-nix. #999
  - Remove `use-source-paths-from-classpath` setting, its value was already true and disabling it could cause false-positives.
  - Add `compute-external-file-changes` setting as true by default, when enabled it will consider file changes outside editor like git branch changes and update analysis, avoiding the need to restart server when a file is changed outside editor, this will only work if client file-watchers is enabled. #1002
  - Bump lsp4clj to 0.4.1.
  - Remove deprecated disabled setting `:linters :clj-kondo :async-custom-lint?`. #1017
  - Fix references and code lens of defrecord/deftype for cljs files. #1055
  - Fix clean-ns to move reader conditionals to before normal requires. #1057
  - Add new optional linter: [clj-depend](https://github.com/clj-depend/clj-depend) integration. #957
  - Add new setting `:source-paths-ignore-regex` to filter source-paths that are auto generated for example for cljs projects, the default value should be enought for most cases (`["resources.*" "target.*"]`), replacing old `ignore-classpath-directories`.
  - Bump clj-kondo to 2022.06.22.

- Editor
  - Add support to rename namespace of namespaced keywords like re-frame events/subs. #978
  - Improve performance of find-declaration feature. #1021
  - Fix to avoid suggesting an alias from a clj file to a cljs file. #1024
  - Find references of namespace usages now find all namespace usages on project, not only the definition. #1022
  - Improve element selected on `textDocument/hover`, showing the function being called instead of the closest element found backwards. #995
  - Fix drag from quoted symbols and other special nodes #969
  - Drag requests two smaller edits, instead of one large edit, potentially avoiding flicker. #1043
  - Drag is disabled between clauses, to avoid arbitrarily choosing one to move. #1030
  - Cursor doesn't move within dragged clause. #1029
  - Improve performance of drag forward.
  - Avoid invalid cached analysis and document text after a rename. #1049
  - Improve lint performance by only linting references files when usage is added or removed. #1019
  - Extracted functions are private. #1039
  - Fix errors when Emacs lock files are linted. #1054

## 2022.05.31-17.35.50

- General
  - Bump clj-kondo to 2022.05.31
  - Consider `.clj_kondo` files as clj files.

## 2022.05.23-13.18.11

- General
  - Allow specify how many classes clojure-lsp should check before moving sorted package imported classes to next line. #966
  - Allow `:linters` to be configured by LSP clients passing string keys/values. #977
  - Support clj-kondo `:config-in-ns` and `:ns-groups` for `clojure-lsp/unused-public-var` linter. #981
  - Improve clean-ns correct positioning of multiple refers.

- Editor
  - Fix powershell.exe command execution error on Windows
  - Fix drag in `are` when `clojure.test` is aliased. #967
  - Reduce time to calculate semantic tokens, reducing CPU usage in large files. #970
  - Fix JDK global cache load when jdk source were already analyzed on previous sessions, fixing finding definition of jdk classes not working sometimes.
  - Fix command move-form not working.
  - Fix find definition of macros required by :require-macros on cljs files. #980
  - Add support to completion keywords found on function definition `:keys` destructuring, improving completion on api calls.
  - Avoid duplicate parens when inserting snippets during completion. #982

- CLI/API
  - Make all cli features open files faster, improving speed of all CLI/API features. Fixes #985

## 2022.05.03-12.35.40

- General
  - Bump clj-kondo to `2022.04.26-20220429.192438-2`.
  - Decrease uncached startup time by 60-70%, by instructing clj-kondo to skip var definition bodies when analyzing deps. [#1674](https://github.com/clj-kondo/clj-kondo/pull/1674)
  - Improve speed of alias/ns completions.
  - Change alias/ns completions to return a label that matches the input.
  - Support sorting classes inside package imports during `clean-ns`. #932
  - Avoid saving duplicate source-paths, not calling clj-kondo to analyze same filenames multiple times, improving startup speed.
  - Improve progress report both on editor and CLI using new clj-kondo callback feature.

- Editor
  - Support `workspace/willRenameFiles`, renaming namespaces and all its references when a file is renamed.
  - Don't save cache when classpath lookup failed.
  - Wait for editor to apply edits before requesting cursor re-positioning. Fixes cursor positioning after drag in Calva.
  - drag: Request edit only of changed clauses, not entire parent, reducing flicker.

- CLI/API
  - Bump lsp4clj to `0.3.0`.

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2022.04.18-00.59.32

- General
  - Improve settings documentation.
  - Fix completion performance regression from previous release.
  - Consider `.bb` and `.cljd` files as clj files. #906
  - Bump to clojure 1.11.0
  - Improve analysis query performance as a whole for lots of features. #916
  - Bump clj-kondo to `2022.04.09-20220414.123207-3` fixing semantic tokens for `:require`, `:refer` and `:as`. [#1609](https://github.com/clj-kondo/clj-kondo/issues/1609)
  - Move `lsp4clj` to outside clojure-lsp to its own repo/jar release.
  - Fix local files outside source-paths not being linted even if opened/changed.
  - Consider filename + lastModified as checksum for external files, avoiding analyzing it if analysis is already present.
  - graalvm: Compile static linux with --musl for better compatibility. #868
  - java: Use `XDG_CACHE_HOME` or `.cache/clojure-lsp` instead of config files for JDK cache.
  - java: Cache JDK analysis globally avoiding high CPU usages after startup.
  - java: Add `:java :home-path` setting for easier way to specify java location for JDK source analysis. #907

- Editor
  - Introduce ALPHA move-form command. #566
  - Rename "Move coll entry up/down" to "Drag forward/backward", matching Calva/Paredit terminology
  - drag: clauses move intuitively in `clojure.test/are`
  - drag: top-level forms can be dragged #891
  - Improve completion performance for most cases, reducing time to compute clj/cljs core symbols.
  - completion: suggest functions defined in Clojure 1.10 and 1.11
  - completion: Fix to no require extra ns when alias is already required. #920
  - promote-fn: *new feature* Promote a fn to a top-level defn. #783 @mainej
    - promote-fn can also promote a literal #() to a fn
  - demote-fn: Demote a fn to a literal #()
  - *breaking* remove cycle-fn-literal, since the same refactorings can be performed with the more clearly named promote-fn and demote-fn
  - drag: Fix to drag element-wise in destructured keys, not pair-wise. #927
  - test-tree: reduce CPU usage, especially during startup

- CLI
  - Reduce CPU and wall-clock time in cli commands clean-ns and diagnostics

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2022.03.31-20.00.20

- Fix URI resolver on java JDK logic.
- Fix zipfile scheme when finding external deps.

## 2022.03.31-14.21.14

- Add java class find-definition support, decompiling .class files when available. #762
- Add JDK source discoverability feature, searching for installed JDK for later analyze with clj-kondo and support java classes interop.
- Add `:java :download-jdk-source?` setting to download JDK source after startup if not cached before globally or found locally. Disabled by default.
- Avoid high CPU usage and freezes by more efficiently finding referenced files to notify on file change. #844 @mainej
- Bump clj-kondo to `2022.03.10-20220331.135739-32` improving java analysis and fixing a critical regression for re-frame. #888

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2022.03.26-18.47.08

- Fix unused-public-var not considering excluding comments, changing `async-custom-lint?` setting to false.

## 2022.03.25-12.02.59

- General
  - Fix cljfmt settings merge during refresh/classpath configs merge to avoid multiple config vectors on same symbol.
  - Fix install script for aarch64. #794
  - Fix handling cljfmt config files that end in `.clj` https://github.com/weavejester/cljfmt/issues/190
  - parser: more efficiently seek to cursor position, improving performance especially in large files. #793 @mainej
  - Fix clean-ns not sorting properly node requires for cljs. #815
  - Fix move-to-let to ensure locals don't move out of scope. #830
  - Improve logic around require suggestions. #837
  - Enhance move-to-let to introduce and expand let if an existing one doesn't exist. #829
  - Bump `org.clojure/clojure` to `1.11.0`.
  - Fix move-coll-entry to maintain cursor position instead of a range. #862
  - Clean ns automatically after adding missing require/imports, enabled by default under new `:clean :automatically-after-ns-refactor` flag. #558

- Editor
  - extract-function: Fix wrong args when extracting from multi-arity fn. #683
  - extract-function: Fix wrong args when extracting after a local usage. #812 @mainej
  - move-coll-entry: clauses move intuitively in `assoc`, `case`, `cond`, and similar functions. #780 @mainej
  - move-coll-entry: fix NPE when when invoked from top-level #803 @mainej
  - Generate stubs async after startup, improving startup time. #788
  - Improve and add lots of new snippets following practicalli config. #797
  - Improve how watched new files are analyzed avoiding infinite loops and performance issues. #796
  - Avoid infinite loops when several files are changed simultaneously. #796 @mainej
  - Fix "incoming call hierarchy" not considering usages inside defmethods. #808
  - range-formatting: more efficiently locate extent of range and reduce number of calls to cljfmt, improving performance especially when formatting large ranges. #795 @mainej
  - cycle-fn-literal: *new feature* convert between function-literal syntaxes `(fn [] ...)` <-> `#(...)`. #774
  - Add find-implementation feature to `defmulti` and `defmethod`. #751
  - Make find-implementation of `defprotocol` names find its implementations and find-implementation on `deftype`/`defrecord` methods find other implementations.
  - Add new code action `Introduce let` for existing command. #825
  - Make find-implementations consider `reify`. #827
  - Fix namespace on file creation when nested source-paths are available. #832
  - unused-public-var: fix to show warnings on vars defined with declare. #840
  - unused-public-var: large performance improvements, especially for large projects. #861 @mainej

- API/CLI
  - Extract lsp4clj as a seperate library. #807 @Cyrik Supported by [Scarlet](https://www.scarletcomply.com)
  - Fix inconsistency with clean-ns/format not copying kondo configs.

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2022.02.23-12.12.12

- General
  - Use `:source-aliases` setting during default deps.edn and lein project-spec aliases, avoiding the need to configure a whole project-spec just because of a additional alias.
  - Exclude from unused-public-var linter vars with metadata `^:export`. #753
  - Fix clean-ns multiple refers sort when there is a alias before the refers.
  - Bump clj-kondo from `2022.01.15` to `2022.02.09` adding support for implementation analysis and more improvements.
  - Medium performance improvement during startup when unused-public-var linter is disabled.
  - Medium performance improvement during startup on unused-public-var calculation parallelizing calculations.
  - Small performance improvement on code actions calcullation.
  - Add `:use-source-paths-from-classpath` setting defaulting to true, which makes clojure-lsp do not manually discovery source-paths but get from classpath excluding jar files and paths outside project-root. #752 #551
  - Improve completion performance when all clojure.core or cljs.core symbols are valid completions. #764, #771 @mainej
  - Fix scenarios where the lint findings in individual files differed from what you'd expect based on the .clj-kondo/config.edn settings.
  - Add `:exclude-regex` and `:exclude-when-definted-by-regex` to `:clojure-lsp/unused-public-var` linter.
  - Bump `org.clojure/clojure` to `1.11.0-rc1`.

- Editor
  - Fix exception during code actions calculation when in a invalid code of a map with not even key-pairs.
  - Don't return diagnostics for external files like files on jar dependencies, avoiding noise on lint when opening dependencies.
  - Support finding implementations of defprotocol and references of defrecord/deftype, implementing LSP method `textDocument/implementation`. #656
  - Make the actions and commands aware of when they were invoked from comments or whitespace. This will allow individual refactorings to be more deliberate about how they handle comments and whitespace. #716 @mainej
  - Correctly position the cursor after calling move-coll-entry-down on an entry with leading comments. #758 @mainej
  - Don't return completions when invoked from a comment, avoiding performance problems. #756
  - Fix small anomalies in parameter names of extracted private functions. #759 @mainej
  - Add semantic tokens for protocol implementations methods like defrecord and deftype.
  - Small performance improvevement on code lens calculation using transducers.

- API/CLI
  - Small performance improvement to `format`, `clean-ns`, `diagnostics`, and `rename` via parallelizing parts of the logic.
  - Fix edn->json parser of `serverInfo/raw` for Calva use cljfmt configuration. #763
  - Add `:cljfmt-raw` config to `serverInfo/raw` for Calva. #768
  - Add support for passing specific `--filenames` for most actions. #775

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2022.02.01-20.02.32

- General
  - Fix classpath hash regression introduced by last release

## 2022.02.01-16.53.14

- General
  - Migrate project from depstar -> tools.build. #711
  - Improve classpath hash to consider invaliding cache when local root deps was changed.
  - Replace datalevin cache db with transit. #703
  - Bump Graalvm from 21.3.0 to 22.0.0.2 improving binary performance/size
  - Update `XDG_CONFIG_HOME` to clojure-lsp directory in doc/settings.md

- Editor
  - Support going to namespace definition on an alias. #706
  - Add LSP `textDocument/declaration`, for now adding the making possible navigate to alias and namespaces declared on the namespace. #680
  - Fix removal of file analysis on didClose for external files like jars. #717
  - Fix cursor not moving when using code actions that move the cursor automatically.
  - Fix additional-snippets to work on top-level forms as well.
  - Fix resolve completion item not returning insertText. #719 @Cyrik
  - Remove support for LSP `codeAction/resolve`. It added complexity and was not used in a way that improved performance of the action menu. #722, #725, #726
  - Fix create public function refactoring not working when function is not a function call.

## 2022.01.22-01.31.09

- General
  - Fix freezing in some cases during clojure-lsp startup linting. #708

## 2022.01.20-14.12.43

- General
  - Bump clj-kondo to `2022.01.15`.
  - Add `:classpath` to `serverInfo` command for downstream usages.
  - Fix issues in `extract-function` that arise when operating over `.cljc` files.
  - Add setting `:copy-kondo-configs?` to allow disable auto clj-kondo config copy, enabled by default. #694
  - Lint unused public keywords, like unused re-frame events/subs. #691
  - Fix rename of namespaced keywords -> aliased keywords. #665
  - Huge refactor on clojure-lsp codebase, spliting code into submodules, this made possible remove a lot of deps from API jar and help separate clojure-lsp features in the future. #658
  - Send testTree notification for all project test files after clojure-lsp start. #697

- Editor
  - Improve 'create function' refactor code action handling multiple cases. #682
  - Fix 'resolve macro as ...' code action not working.
  - Fix `showDocumentRequest` issues when triggered via some refactor code action.
  - Add new code actions + commands `Move coll entry down` and `Move coll entry up` to move entries within collections. #684, #701

- API/CLI
  - Make `format`, `clean-ns` and `rename` features not need to scan whole classpath, analyzing only project code improving performance a lot.

## 2022.01.03-19.46.10

- Catch clj-kondo prints to stdout and log to clojure-lsp log file avoiding crashes on some clients like vscode/Calva.

## 2022.01.03-15.41.19

- General
  - Fix some analysis conflicts regarding `custom-async-lint?` feature introduced on latest release causing outdated analysis and some deadlocks.

- Editor
  - Fix inline symbol code action regression from previous releases. #678
  - Fix expand let refactor duplicating variables in some cases. #676
  - Add completion support to potemkin usages of a namespace.

- API/CLI
  - Wait for db cache upsert before end proccess, avoiding the need to re-lint whole classpath on next api/cli runs.
  - Fix the need to use `:raw? true` on babashka pod usage.

## 2021.12.20-00.36.56

- General
  - Bump clj-kondo to `2021.12.19`, supporting auto-load configs, improving potemkin support, adding more linters and more.
  - Merge `:cljfmt` settings with `:cljfmt-config-path` if file path exists.
  - Avoid high CPU and lockup when clj-kondo throws exceptions. #671
  - Allow absolute paths in deps.edn :local/root #672
  - Fix clojure-lsp not loading for some mono-repo cases, improving local/root support for polylith projects. #673
  - Avoid infinite loop because of cyclic dependencies on deps source-path discovery.
  - Add babashka pod. #555

- Editor
  - Change call hierarchy to return selection range of usage, not function definition.
  - Return `edits` in `codeAction/resolve` responses rather than `commands`. #655
  - Improve `:linters :clj-kondo :async-custom-lint?` to avoid infinite loops and default to `true`.
  - Add new custom LSP feature __Test Tree__, which shows all test hierarchy of a file. #653
  - Improve function name finding to consider other function definition types for some features. #666
  - Make `textDocument/hover` return the correct range from LSP spec, the element range instead of the element scope range.

- API/CLI
  - Exit process if any error during classpath lookup. Opt-out via `:api :exit-on-errors?` flag.

## 2021.12.01-12.28.16

- General
  - Add support for LSP method `textDocument/prepareRename` which it's the proper way to check if the rename will work correctly. #642
  - Expose new custom method `clojure/cursorInfo/raw` for custom hack on current cursor information code. #645
  - Support stub generation using `clj-easy/stub`, adding analysis and linting support for closed sources codes like Datomic. Check `:stubs` settings for more details. #637
  - Handle config deep merge differently for collections, concating instead of overwriting.
  - Fix unnecessary exception thrown on graal images during startup.
  - Support `deps.edn` `:local/root` source-paths discovery, improving support for monorepo projects like `polylith`. #652
  - New setting value for `:clean :sort :require`: `:lexicographic`. #654
  - Bump clj-kondo to `2021.10.20-20211126.151305-16`.

- Editor
  - Support completion on aliased keywords. #649
  - Add new `Sort map keys` refactoring code action. #651
  - Add new `Create function` code action, allowing to create a function on a existing namespace or creating a new namespace + the function. #646
  - Improve `Extract function` refactoring to consider comments above current function.
  - Experimental: new `:linters :clj-kondo :async-custom-lint?` setting, when true, scan unused-public-vars async improving lint/analysis UI feedback for huge buffers (> ~1000 lines). Default `false`.

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2021.11.16-16.52.14

- General
  - Improve rename feature to not heavily rely on valid source-paths for most cases.
  - Fix setTrace exception logs for graalvm native images.
  - Huge improvements on namespaces renames and namespaces references find. #573
  - Fix/Remove warnings during datalevin access.
  - Improve freezing for some MacOS cases. #631
  - Bump clj-kondo to `2021.10.20-20211116.110002-7` improving code parsing and other fixes.

- Editor
  - Fix "Add require" code actions adding multiple requires instead of the selected.
  - Improve "Add require" wording, making it easier to understand what each different action will do.
  - Smart check all available refers to require, adding refer options to `Add require` code actions. #627
  - Big improvements on keyword completions. #630
  - Add setting `keep-parens-when-threading?` to keep parens for single arity functions when threading. #636
  - Avoid adding duplicate requires when adding a new require via code action. #640
  - Improve common known snippets to replace completion items, improving completion UX. #638

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2021.11.02-15.24.47

- General
  - Bump Graalvm from 21.2.0 to 21.3.0 improving binary performance/size
  - Fix wrong parse of code when code contains namespaced maps like `#:foo{:bar 1}`. This issue was affecting a lot of features for example code actions.
  - Bump datalevin from 0.5.26 to 0.5.27.
  - Improve semantic tokens for dynamic vars, function definitions, namespaced and aliased keywords.
  - Fix bug where `:source-paths` settings could be hot-reloaded with wrong-value.

- Editor
  - Deprecates setting `:show-docs-arity-on-same-line?` in favor of `:hover` `:arity-on-same-line?`.
  - Add support to new LSP `LinkedEditingRange` feature. #341
  - Improve suggested `Add require ...` code actions, this should make clojure-lsp smarter when user wants to add a missing require. #614
  - Change `:notify-references-on-file-change` default from `false` to `true`, we had some performance improvements and I've been testing this for some time now and didn't see any new issues with that. This should improve a lot the UX when user change any code that is references on other files, updating the diagnostics for those files as well.
  - Improve rename feature UX to output errors when it's not possible rename.
  - Add support for `window/showDocument` LSP method, used on `create-test` command/code action after creating the test to show the test file.
  - Add new `Unwind thread once` and `Unwind whole thread` code actions to undo a thread call.
  - Improve code actions performance requesting async all actions.
  - Add new LSP custom method `clojure/clojuredocs/raw` which takes a symbol and a namespace (both strings) and returns any Clojuredocs entry found, otherwise `null`.
  - Fix missing keywords rename/references for destructured keywords.

- CLI
  - Show error/warning message when a classpath scan fail during analysis. Fixes #626
  - Add coloring to `diagnostics` output matching diagnostic severity.

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2021.10.20-16.49.47

- Editor
  - Hot fix clojuredocs on graalvm native image.
  - Bump clj-kondo to `2021.10.20-20211020.123254-3` to fix a specific issue with cljs.
  - Implement support to return to client LSP Errors, making user UX better since clojure-lsp can return specific errors for specific exceptions.

## 2021.10.20-13.04.11

- General
  - Improve intialization feedback report messages.
  - Consider `dev` and `test` alias for deps.edn projects as project-specs during classpath lookup. #586
  - Avoid scanning source-paths twice, as it was being considered as part of external classpath as well.
  - Change cache db from sqlite to datalevin for faster startup + better graalvm compatibility.
  - Make the cache analysis save async to make startup faster.
  - Support Auto refresh settings memoizing with a ttl of 1 second avoiding the need of restarting server when changing configs. #502
  - Bump clj-kondo adding new `gen-class` linter and other fixes/improvements. Fixes #589
  - Remove unused duplicate require if any. #527
  - Fix crash on clean-ns when ns contains comments.
  - Improve project analysis filter to check source-paths. #597

- Editor
  - Add reference code lens for ns forms. #578
  - Fix expand-let bug that occurs when a list form precedes let. #590
  - Add new command to create test for function at point. #582
  - Add new code action to create test for current function/var
  - Add `private` to documentSymbol to make clear that a var or function is  `private`.
  - Add new code action `Suppress xxx diagnostic`, adding clj-kondo comment code to ignore the diagnostic. #591
  - Add more semantic tokens: aliases for macros, variable and function definitions.
  - Add [clojuredocs](https://clojuredocs.org/) information during symbol hover. #571

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2021.09.30-15.28.01

- Editor
  - Hot fix hover content wrong type hinting.

## 2021.09.30-12.28.16

- General
  - Use lower-case for refer/import/require sorting. #560 #561
  - Avoid removing comments when sorting/cleaning namespace. #559
  - Break lines when sorting refers along with then new `:clean :sort :refer :max-line-length` setting with a default of `80`. #562
  - Deprecate `lens-segregate-test-references` in favor to `:code-lens :segregate-test-references`
  - Check for a default `.cljfmt.edn` config file for cljfmt config settings if no `:cljfmt-config-path` was provided. #563
  - Bump clj-kondo to `2021.09.25` fixing false-positives with potemkin import-var analysis.
  - Re-scan whole project if any clj-kondo config changed for better consistence. #331
  - Fix clojure-lsp not initializing when empty `project.clj`. #579
  - Support finding config in classpath via new setting`:classpath-config-paths ["my-org/my-lib"]`. #580

- Editor
  - Fix `resolve macro as` code action after regression introduced recently.
  - Fix `unused-public-var` not being suppressed during project startup. #554
  - Improve `hover` feature to return elements when inside a function call. #569
  - Fix `create-private-function` command and code action to consider when new function is inside thread macros.
  - Support `$/progress` LSP feature, sending notifications for client when server is starting, improving the feedback for the user.
  - Improve semantic tokens support for java classes and methods.

- API/CLI
  - Support renaming namespaces as well with `rename` feature.
  - Use relative paths instead of absolute paths on diff messages.
  - Add `analyze-project!` to analyze whole project only and cache analysis, useful for REPL usage for example.
  - Follow same exit status from clj-kondo for `diagnostics` feature. #572
  - Improve start project feedback reporting the percentage and specific message.

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2021.09.13-22.25.35

- Editor
  - Rollback change on `didChangeWatchedFiles` for `:change` events, avoiding outdated changes overwriting newer changes.

## 2021.09.13-19.32.00

- General
  - Create .clj-kondo folder if not exists in project root. #528
  - Fix exception when `:clojure-lsp/unused-public-var` linter is `:off`. #546
  - Bump clj-kondo to `2021.08.07-20210903.210340-28` to fix a false-positive with potemkin. https://github.com/clj-kondo/clj-kondo/issues/1370.
  - Bump clj-kondo to `2021.08.07-20210909.131804-29` fixing issues with built-in clj-kondo cache not present on graalvm binaries. #548
  - Exclude `cljs.test/deftest` from unused public vars linter.
  - Migrate default db file from `.lsp/sqlite.db` to `.lsp/.cache/sqlite.db`, this is necessary as in the future we will replace sqlite with other db implementation, for users they just need to consider/gitignore the `.lsp/.cache` folder, this way any next change on db implementations or any other cache will not affect user.
  - Auto migrate existing `.lsp/sqlite.db` to new `.lsp/.cache/sqlite.db` to avoid unnecessary project re-scan.
  - Deprecates `:sqlite-db-path` in place of `:cache-path`.

- Editor
  - Fix `didChangeWatchedFiles` to correctly create the file on server, properly change file content and re-scan with clj-kondo, or remove file analysis. This should improve LSP analysis reliability when changing files outside the editor. #536
  - Improve completion only showing valid local vars for current cursor.
  - Improve completion sorting adding priority to each item, showing most used symbols like variables and functions first before other completion items.

- API/CLI
  - New `diagnostics` command, which return all diagnostics found by clojure-lsp (using clj-kondo). Check the API section for more details.

This release was supported by [Clojurists Together](https://www.clojuriststogether.org/)

## 2021.09.04-17.11.44

- Hotfix java classes not present on jar, required for clojure-lsp downstreams.

## 2021.09.03-00.42.46

- General
  - Improve logging during startup for better troubleshooting.
  - Refactor allowing calls to `clojure-lsp.main/run!` for manually passing args, useful for `lein-clojure-lsp` for example.
  - Internal: Move graalvm configuration to sqlite-jdbc.
  - Recognize `deftest` as function definition form for refactoring features like `extract-function`.
  - Bump Graalvm from 21.1.0 to 21.2.0

- API/CLI
  - Use clj-kondo custom lint for API as well, required for correct diagnostics API feature.

- Editor
  - Fix regression, custom `source-paths` from initializationOptions were not being parsed correctly. #537

- Documentation
  - New domain for documentation and webpage https://clojure-lsp.io :rocket:

## 2021.08.24-14.41.56

- General
  - Fix classpath scan when classpath has other things like new lines or warning message besides the classpath. Fixes #523
  - Improve `clean-ns` to remove empty reader conditionals(`#?(:clj)` or `#?@(:clj [])` on ns form) after cleaning requires/imports.
  - Fix `clean-ns` false-positives removals to cljc files when the alias/refer/import is being used inside a reader conditional.
  - Add new setting `:linters :clj-kondo :ns-exclude-regex` which allows exclude diagnostics/findings for namespaces matching that regex.
  - Fix merge of configs resolved for projects with multiple configurations in parent folders and subprojects.

- Docs
  - Improved the settings docs with a new link to a file with all available clojure-lsp settings.

## 2021.08.16-19.02.30

- Fix `clojure-lsp --version`

## 2021.08.16-14.47.54

- General
  - Fix wrong parse of source-paths for bb.edn when :paths contains symbols not only strings. #507
  - Bump clj-kondo to fix a issue with clojure-lsp running in a lein process.

- Editor
  - Fix find-definition in jars for cljc files where the var is available on both cljs and clj files. #509
  - Add clojure.core.async common vars to common-refers to be required via code action.
  - Remove diagnostics when files are deleted, properly cleaning server. #513
  - Don't add ns form to blank edn files. #515
  - Fix initializationOptions parsing for some clients. #516
  - Fix refactor 'add missing refer' when there is already that namespace with a alias but no refers.
  - Improve `:notify-references-on-file-change` performance and use-cases, still disabled by default for some time.

- API
  - Rollback printing only via CLI to work with API as well. (can be disabled via :raw? option)
  - Support for release of lein-clojure-lsp

## 2021.08.05-19.36.09

- Bump clj-kondo to fix window path issues with analysis.
- Fix issue with references code lens for vim.

## 2021.08.05-18.25.54

- Fix async project lint after startup for huge projects. #506
- Fix `:lint-project-files-after-startup?` to be considered before clojure-lsp lint unusued public vars.
- Fix excluded symbols for code lens, making clojure-lsp check clj-kondo config as well for the `:clojure-lsp/unused-public-var` excludes

## 2021.08.03-13.33.03

- General
  - Parse correctly unescaped URIs sent from clients like vim avoiding errors on some features.
  - Bump clj-kondo fixing analysis position issue with `declare`, making rename and other features work.
  - Don't use PowerShell profiles on Windows when analyzing classpath. Fixes https://github.com/BetterThanTomorrow/calva/issues/1050
  - Support babashka classpath and source-paths discovery via bb.edn file. (needs babashka >= 0.5.1)

- Editor
  - Add `:hover :hide-file-location?` settings option to disable displaying the source path on hover.
  - Use new clj-kondo `:custom-lint-fn` for the `:unused-public-var`, this should improve performance and give the ability to suppress unused vars via code with `#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}` or `#_:clj-kondo/ignore`

## 2021.07.28-14.24.06

- General
  - Minor fix on the analysis queries comparison.
  - Improve GraalVM configuration to use direct/less configurations.
  - Bump clj-kondo 2021.06.18 -> 2021.07.28 which adds support for macroexpanding.

- Editor
  - Fix outgoing/incoming call hierarchy when vars are outside project/external jars.
  - Fix completion of aliases without var names typed yet, for example: `string/`

- API/CLI
  - Fix filename in diffs with dry option.
  - Add coloring to diffs following git diff.
  - Add `ns-exclude-regex` for commands that check whole project, allowing to exclude certain namespaces via regex.

## 2021.07.19-14.46.52

- General
  - `clean-ns` now sorts ns children forms according to the ClojureStyleGuide, at the moment moving require form before import form, enabled by default under flag `:clean :sort :ns` setting.
  - Improve startup error handler and logging during project analysis. Related to #484
  - Performance improvements using transducers on analysis queries.
  - Fixed auto-resolve source-paths for lein projects that get source-paths dynamically, clojure-lsp will use the default source-paths for these cases

- Editor
  - Improve hover output, both markdown and plain text forms.
  - Allow calling thread-first/last and thread-first-all/last-all from within the list.
  - Improve resolve-macro-as messages and default excluded symbols.

- API/CLI
  - Fix empty `XDG_CONFIG_HOME` not defaulting to `$HOME/.config` #474
  - Improve verbose logging setting.

## 2021.07.12-12.30.59

- Editor
  - Make semantic-tokens return no token for unknown symbols which has `:clj-kondo/unknown-namespace` on its analysis.
  - Fix file uri location when hovering a symbol.
  - Add reference code lens to keyword definitions, e.g. `re-frame.core/reg-sub`.
  - Add `semantic-tokens` debug information to `cursor-info` response.

- API/CLI
  - Fix when ns form does not match filename. #466
  - Fix errors with project-root on graalvm binary
  - Improve API usage avoiding exceptions and returning just data instead.
  - Improve analysis cache to multiple API calls.
  - Add new `--raw` option allowing to display only raw data. Useful to integrate with other tools like reviewdog.

## 2021.07.05-20.31.12

- API/CLI
  - Add `--verbose` option for debugging purposes.
  - Fix usage as library parsing `:exec-args` correctly

## 2021.07.05-15.12.14

- General
  - Add `:clean :sort` settings option to disable sorting during `clean-ns`.
  - Add `:keep` value to `:ns-indent-blocks-indentation` setting to don't change indentation during `clean-ns`.
  - Deprecate `install-latest-clojure-lsp.sh` in place of `install` new script.
  - Improve source paths discoverability for `leiningen` projects following the same rules as `deps.edn` projects. For more information, check the [settings section](https://clojure-lsp.github.io/clojure-lsp/settings/#source-paths-discovery).

- API/CLI
  - Add `--dry` option to commands, useful to print only diffs instead of making changes.
  - Check the new [setup-clojure-lsp](https://github.com/marketplace/actions/setup-clojure-lsp) Github Action to run clojure-lsp on CI.
  - Add `format` feature using cljfmt.
  - Now, every release is available in clojars as `com.github.clojure-lsp/clojure-lsp`.

## 2021.07.01-19.49.02

- Fix --project-path option for API.
- Add --log-path option to API.

## 2021.07.01-13.46.18

- Fix find-references and references lens for `defrecord`.
- Improve `clean-ns` refactoring following the Clojure style guide.
- Fix `clean-ns` corner case issue where it would replace the `:as` value with the `:refer` value.
- Deprecate `:keep-require-at-start?` in favor of `:clean :ns-inner-blocks-indentation` with default value of `:next-line`.
- Bump `org.xerial/sqlite-jdbc` from 3.34.0 to 3.36.0.
- Avoid adding duplicated namespace for new blank files.
- Add API / CLI support, form more information check the [API](https://clojure-lsp.github.io/clojure-lsp/api/) section.

## 2021.06.24-14.24.11

- Fix missing LSP 3.16 graalvm reflection configs. #452

## 2021.06.24-01.20.01

- Fix graalvm reflect config for LSP 3.16 protocol. #449
- Use last definition when showing signature help. #446
- Create 2 new LSP custom methods: `clojure/serverInfo/raw` and `clojure/serverInfo/log` deprecating old commands `server-info` and `cursor-info`, check the documentation for more information.
- Add `:final-settings` to `serverInfo` methods. #450

## 2021.06.21-14.30.54

- Avoid removing whole require if unused refer but with used alias during clean-ns. #447
- Bump cljfmt 0.7.0 -> 0.8.0. Fixes #266
- Bump clj-kondo 2021.06.01 -> 2021.06.18

## 2021.06.14-17.00.47

- Improve source paths discoverability for `deps.edn` projects. Adds `:source-aliases` for customization. For more information, check the [settings section](https://clojure-lsp.github.io/clojure-lsp/settings/#source-paths-discovery).

## 2021.06.01-16.19.44

- Fix graalvm issue on completionItem InsertTextMode
- Bump clj-kondo and rewrite-clj to latest releases.

## 2021.05.27-17.42.34

- Fix corner case error with project paths with spaces or other special characters. #437
- Drastically improve didOpen performance, improving kondo findings parsing and making linting async avoiding blocking some editors. #435
- Fix `:auto-add-ns-to-new-files?` flag always being considered as `true`. #436

## 2021.05.22-16.50.45

- Fix completion item kind of clojure core items. Fixes #426
- Fix graalvm reflect config for SublimeText - #430
- Improve code lens for segregated code lens when on test files.

## 2021.05.14-15.26.02

- Include non full qualified vars on unused-public-var exclude filter.
- Improve hover documentation: use correct markdown for docstrings; remove unnecessary new lines; add link to filename location.
- Rollback full text changes on last release and change approach for a temporary fix. #424

## 2021.05.06-19.44.00

- Quick fix full text changes to handle it sync for semantic tokens
- Segregate main references from test references with option to disable via `lens-segregate-test-references`

## 2021.04.27-20.17.45

- Significantly improve the performance of workspace symbol filtering/searching. [See relevant commit](https://github.com/anonimitoraf/clj-flx/commit/61b2081b65b7d3be14851bac03ea508147c90054).
- Always sort refers when clean-ns.
- Add support for installing with homebrew on Linux.
- Bump clj-kondo to `2021.04.24-20210426.144134-2` adding support for finding re-frame by keyword. Fixes #411
- Fix find definition going to `declare` - Fixes #340
- Remove common already known clojure macros from `Resolve macro as` code action.

## 2021.04.23-15.49.47

- Improve resolve-macro-as command to check and log if couldn't resolve the macro.
- Improve workspace symbol filtering/searching. Now, the sole candidates shown are guaranteed to include all the characters contained in the filter/search string.
- Add more tokens to semantic tokens: keywords, functions/var usages, java classes, local variables.
- Bump Graalvm from 21.0.0 to 21.1.0
- Bump clj-kondo to 2021.04.23 fixing some keywords corner cases.

## 2021.04.13-12.47.33

- Add common snippets on completion. Check all available snippets [here](https://clojure-lsp.github.io/clojure-lsp/features/#snippets).
- Add support for custom snippets via `:additional-snippets`. Check [here](https://clojure-lsp.github.io/clojure-lsp/features/#snippets) for more information. - Fixes #403
- Bump lsp4j from 0.11.0 -> 0.12.0

## 2021.04.07-16.34.10

- Fix renaming keywords in cljc files producing duplicate edits.
- After project startup, publish all project only diagnostics. This is a approach done by other LSPs to make work features like [Project errors](https://emacs-lsp.github.io/lsp-mode/page/main-features/#project-errors-on-modeline). Feature flag via `lint-project-files-after-startup?` with default `true`.
- Add experimental support for aarch64 linux native binary
- Fix formatting issues with a regression introduced on a previous release. - Fixes #339 and #396

## 2021.04.03-18.43.55

- Fix textDocument/workspaceSymbols filter not working on native binaries.
- Report duplicate linters as default, can be disabled via `:linters :clj-kondo :report-duplicates` - Fixes #390
- Bump rewrite-clj to 1.0.605-alpha to fix exceptions when on clojure files with babashka interpreter on first line.
- Bump clj-kondo 2021.04.01-20210402.215253-6 to fix bug https://github.com/clj-kondo/clj-kondo/issues/1246

## 2021.03.30-20.42.34

- Bump clj-londo 2021.03.22-20210327.192113-4 - Fixes #385
- Add support for outgoing call hierarchy - Fixes #384
- Improve and fix missing completion item kinds.

## 2021.03.26-23.41.07

- Bump clj-kondo 2021.03.22-20210324.110254-3 - Fixes #382
- [graalvm] Fixes Unable to invoke no-args constructor for class org.eclipse.lsp4j.ShowDocumentCapabilities error.
- Fix/enhance keyword rename - #383

## 2021.03.24-00.41.55

- Migrate from lein to deps.edn
- Bump clj-kondo to 2021.03.22
- Fix clean-ns sorting according to symbols not brackets - Fixes #380
- Fix missing graalvm reflect config for CompletionItemTextEdit - Fixes #381

## 2021.03.21-23.29.19

- Add code action 'resolve macro as', it requires client to fill the chosen macro and clj-kondo config file.
- Bump rewrite-clj to 1.0.594-alpha
- Bump data.json to 2.0.1
- Bump lsp4j to 0.11.0

## 2021.03.18-19.23.41

- Add support for diagnostic tags: deprecated and unnecessary.
- Fix wrong textDocument/documentHighlight for function local-usages.
- Use new clj-kondo `copy-configs` flag to copy hooks during lint.
- Bunp clj-kondo to fix unused public linter check for `:exclude-when-defined-by`

## 2021.03.16-20.28.06

- Fix server not analyzing after a wrong code on cljs files - #367
- Rollback incremental didChange adding a new `:text-document-sync-kind` setting with `:full` as default.

## 2021.03.14-23.22.46

- Fix completion inside refers, re-add support for it - Fixes #364
- Change range of expression functions clj-kondo diagnostics to avoid collision with function signature.

## 2021.03.06-17.05.35

- Fix incremental didChange, debouncing distincting by uri, fixing some inconsistent file changes
when multiple files are changed at same time (rename, iedit, etc).
- Make unused-public-var ignore -main public functions.
- Add `:exclude-when-defined-by` option to `unused-public-var` linter, check settings documentation for more information.

## 2021.03.05-13.35.47

- Fix clojure-lsp lint crash when analyzing specific macros with clj-kondo hooks.
- Fix didChange for Nvim client.
- Add new clojure-lsp linter: unused-public-var - Fixes #359
- Add option to disable clj-kondo linter, check settings documentation for more details.

## 2021.03.01-19.18.54

- Bump clj-kondo fixing issues on require form not being analyzed.
- Fix textDocument/workspaceSymbols to use the query sent by client.

## 2021.02.27-23.35.55

- Add support to complete full qualified namespaces - Fixes #337
- Add `:log-path` setting to log to a custom file.
- Avoid exception on code actions when on cljc reader macros - Related to #346

## 2021.02.26-13.58.48

- Improve clojure-lsp config search checking always home dir considering XDG_CONFIG_HOME and project root up to system root (/) - Fixes #339.
- Handle incremental text changes on `textDocument/didChange` notifications following LSP protocol, improving performance for huge files.
- Add clj-kondo version to --version and server-info.
- Add new create private function code action.

## 2021.02.24-14.23.08

- Improve completion performance resolving the item only when documentation is requested
- Add new thread first/last all code actions

## 2021.02.21-21.02.51

- Implement support for textDocument/signatureHelp - Fixes #324
- Disable notify-references on didChange with a flag `notify-references-on-change`.
- Fix completion not working when reader macro on file - Fixes #332

## 2021.02.19-23.08.40

- Fix duplicated symbols for cljc files on textDocument/documentSymbols - Fixes #328
- Add namespace require when completing a unimported namespace - Fixes #309
- Fix completion not working for cljc files

## 2021.02.19-00.19.27

- Fix completion of invalid clojure core (e.g. `foo/`) - Fixes #270

## 2021.02.17-17.00.45

- Allow find all references across the project of simple keywords
- Allow specify custom sqlite.db location with `sqlite-db-path`, default to `<project>/.lsp/sqlite.db`

## 2021.02.14-19.46.47

- Removing false positive logs from invalid analysis from clj-kondo macro expand analysis
- Fix call hierarchy when the reference was not open yet in the editor.
- Smart re-analyze variable/function references when arguments of the definition were updated.
- Rollback Macos native image compress until it works for MacOS Big Sur - #322

## 2021.02.13-18.04.19

- Bump clj-kondo to fix false-positive unresolved-vars
- Prioritize project analysis on all analysis during find definition - Fixes #318
- Compress native binaries with UPX decreasing binary size.

## 2021.02.12-03.13.12

- Fix completion issues with graalvm native linux binaries when completing local variables.
- Fix completion exception when completing numbers - Fixes #310
- Completion inside a require suggests all available namespaces
- Change log pattern to `/tmp/clojure-lsp.<TIMESTAMP>.out` use default temp file. Should fix issues with permissions on tmp folder.

## 2021.02.11-12.43.06

- Fix auto add ns not working for projects.
- New code action: Move to let
- New code action: Change coll to map, list, set or vector

## 2021.02.10-03.01.19

- Fix config passed to clj-kondo during analysis - Fixes #308

## 2021.02.09-18.28.06

- Fix auto add ns to check project root and source paths
- Add alias on copmletion items - Fix completion items for Calva client

## 2021.02.07-22.51.26

- Fix install-latest-clojure-lsp.sh script - #304
- Fix clojure-lsp re-analysing classpath when project contains a keyword starting with a number - #305
- Allow clj-kondo to pick up config correctly in mono repos - #303

## 2021.02.07-03.04.31

- Make release's native binaries executable by default - #299
- Improve completion removing the necessity to call completion/resolveItem - #292
- New code action: Add suggested alias to namespaces - #302

## 2021.02.05-03.05.34

- Add support for keyword analysis (definition, references, completion, rename, hover)
- Reduce jar and binaries size excluding some dependencies (Need to fix a lein issue yet)

## 2021.02.04-02.08.58

 - Fix --version on graalvm native compiled binaries

## 2021.02.04-01.09.21

- Add integration tests to release process

## 2021.02.02-14.02.23

- Fix `clojure-lsp` embedded jar binary during release CI
- Fix duplicate references on cljc files

## 2021.02.02-03.06.46

- Add clojure.java.io to known requires - #291
- Add manual System/gc after first classpath scan, it should decrease memory after the first startup
- Add support for Windows GraalVM compiled native binary

## 2021.02.01-20.37.52

- Add native binaries for Linux and MacOS compiled with GraalVM #267 (Experimental)

## 2021.01.28-03.03.16

- Fix clj-kondo batch analysis when merging batchs - Fix #284

## 2021.01.27-21.47.42

- Fix missing printlng and avoid errors for next time (this was causing issues in clients like vim coc)

## 2021.01.26-22.35.27

- Fix document-symbol after #261 - Fixes #276
- Reduce memory usage on startup batch analyzing classpath via clj-kondo. - Fixes #268

## 2021.01.25-22.56.05

- Do not remove document on `textDocument/didClose`, related to #264.
- Fix default project-specs for shadow-cljs to use npx prefix.
- Fix range of `textDocument/hover` for definition usages.
- Fix `completionItem/resolve` broken after #261.

## 2021.01.25-17.22.05

- Remove references code-lens from `deftest` forms
- Fix completion for alias ns from external deps - Fixes #269

## 2021.01.22-13.04.28

Huge refactor https://github.com/clojure-lsp/clojure-lsp/pull/261 which uses clj-kondo `analysis`/ `findings` output to almost all `clojure-lsp` features.

- Should significantly increase performance and startup time
- Should fix almost all bugs/issues with windows Users since we now rely on clj-kondo analysis
- **Remove** all lint configs from `clojure-lsp` including `macro-defs`, they should be configured on `clj-kondo` side now via `.clj-kondo/config.edn`
- Move file path on documentation to bottom

## 2021.01.20-01.39.32

- Fixes args for extract-function refactoring - Fixes #263

## 2021.01.16-03.28.20

- Check for defintions when finding references with includeDeclaration as true - Fixes #260
- Add custom command cursor-info to debugging clojure-lsp.
- Fix unnecessary new-lines on imports when executing clean-ns

## 2021.01.14-23.15.54

- Check for the whole line to add-miising-* code actions instead of expect the cursor at the ns to be required/imported - Fixes #258
- Return all possible add-missing-* code actions to the same line.

## 2021.01.14-17.19.10

- Fix add missing import code actions after refactor

## 2021.01.14-12.44.42

- Fixes #208

## 2021.01.14-02.30.28

- LSP 3.16: Add support for `codeAction/resolve` improving performance if client supports it
- Bump extend lib
- [CI] Remove auto release, next releases should contain more than one PR/fix
