# Clojure-LSP Development Guide for AI Agents

## Commands
- **Test**: `bb test` (all tests), `bb test-lib` (lib only), `bb test-cli` (CLI only)
- **Single test**: `cd lib && clojure -M:test --focus clojure-lsp.feature.completion-test/test-completion`
- **Lint**: `bb lint` (dry-run), `bb lint-fix` (apply fixes)
- **Build**: `bb debug-cli` (dev build with nREPL), `bb prod-cli` (production), `bb native-cli` (GraalVM binary)
- **Clean**: `bb clean`

## Code Style
- **Imports**: Alphabetical, grouped (clojure-lsp.* → clojure.* → external). Use `:require` with explicit aliases.
- **Formatting**: Follow cljfmt defaults. 2-space indentation. Use `set! *warn-on-reflection* true` at top of namespaces.
- **Naming**: kebab-case for functions/vars, PascalCase for Java interop. Use `f.` prefix for feature namespaces (e.g., `f.completion`).
- **Functions**: Mark private fns with `^:private` or `defn-`. Pure functions preferred; use atoms (`db*`) for state.
- **Tests**: Use `clojure.test` with `deftest`/`testing`/`is`. Test helpers in `clojure-lsp.test-helper.internal` as `h`.
- **Cross-platform**: Use `h/strings=` for string comparisons (handles `\r\n`), `h/file-path` for paths, `babashka.fs/canonicalize` for absolute paths.
- **Error handling**: Use `ex-info` with data maps. Log with `clojure-lsp.logger`.
- **Documentation**: Docstrings for public APIs. Comments for complex logic. Keep code self-documenting.

## Architecture
- `lib/` contains core logic (features, handlers, analysis). `cli/` is the CLI wrapper.
- Features in `lib/src/clojure_lsp/feature/`. Tests mirror structure in `lib/test/`.
