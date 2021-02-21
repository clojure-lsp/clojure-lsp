# Changelog

## Unreleased

- Implement support for textDocument/signatureHelp - Fixes #324

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
