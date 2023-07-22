# CLI (terminal)

`clojure-lsp --help` should show all available commands and options:

```
Clojure development tool implementing LSP

Usage: clojure-lsp <command> [<options>]

All options:
  -h, --help                           Print the available commands and its options
      --version                        Print clojure-lsp version
      --verbose                        Use stdout for clojure-lsp logs instead of default log settings
      --trace                          Deprecated: use --trace-level instead.
      --trace-level LEVEL       off    Enable trace logs between client and server, for debugging. Set to 'messages' for basic traces, or 'verbose' for more detailed traces. Defaults to 'off' for no traces.
  -s, --settings SETTINGS              Optional settings as edn to use for the specified command. For all available settings, check https://clojure-lsp.io/settings
      --log-path PATH                  Path to use as the log path for clojure-lsp.out, debug purposes only.
      --dry                     false  Make no changes to files, only report diffs
      --raw                     false  Print only necessary data
  -p, --project-root PATH              Specify the path to the project root to clojure-lsp consider during analysis startup.
  -n, --namespace NS            []     Optional namespace to apply the action, all if not supplied. This flag accepts multiple values
      --filenames FILENAMES            Optional filenames to apply the action. Filenames can be either absolute/relatetive files or directories. This flag accepts filenames separated by comma or double colon.
      --ns-exclude-regex REGEX         Optional regex representing the namespaces to be excluded during a command
  -o, --output EDN                     Optional settings as edn on how the result should be printed. Check `clojure-lsp.api/diagnostics`/`clojure-lsp.api/dump` for all available options to this flag.
      --from FROM                      Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename/references
      --to TO                          Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename
      --analysis EDN                   Optional settings as edn on how clojure-lsp should consider the analysis. Check `clojure-lsp.api/dump` for all available options to this flag.

Available commands:
  listen (or empty)    Start clojure-lsp as server, listening to stdin.
  clean-ns             Organize ns form, removing unused requires/refers/imports and sorting alphabetically.
  diagnostics          Analyze the project and find all diagnostics (warnings, errors).
  format               Format code using cljfmt.
  rename               Rename a symbol and all references across the project, use --from and --to options.
  references           Find all references of a full qualified symbol across the project and/or dependencies, use --from option.
  dump (experimental)  Dump all project known data including classpath, source-paths, dep-graph and clj-kondo analysis data.

See https://clojure-lsp.io/settings/ for detailed documentation.
```

