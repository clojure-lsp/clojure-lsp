# Changelog

## Unreleased

- General
  - Use lower-case for refer/import/require sorting. #560 #561
  - Avoid removing comments when sorting/cleaning namespace. #559
  - Break lines when sorting refers along with then new `:clean :sort :refer :max-line-length` setting with a default of `80`. #562
  - Deprecate `lens-segregate-test-references` in favor to `:code-lens :segregate-test-references`
  - Check for a default `.cljfmt.edn` config file for cljfmt config settings if no `:cljfmt-config-path` was provided. #563
  - Bump clj-kondo to `2021.09.16-20210919.160121-5` fixing false-positives with potemkin import-var analysis.

- Editor
  - Fix `resolve macro as` code action after regression introduced recently.
  - Fix `unused-public-var` not being suppressed during project startup. #554
  - Improve `hover` feature to return elements when inside a function call. #569
  
- API/CLI
  - Support renaming namespaces as well with `rename` feature.
  - Use relative paths instead of absolute paths on diff messages.
  - Add `analyze-project!` to analyze whole project only and cache analysis, useful for REPL usage for example. 

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
