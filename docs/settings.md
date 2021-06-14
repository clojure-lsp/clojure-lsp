# Settings

`clojure-lsp` settings are picked up on server start and can be configured via 3 ways:

- **Project configuration**
- **Global configuration**
- **LSP InitializationOptions**

### Project

`clojure-lsp` will look for project specific settings in a file called `.lsp/config.edn`. It will search from your project root folder up the directory structure so you can have multiple projects share the same settings.

Example:
```clojure
{:cljfmt {:indents {#re ".*" ns [[:inner 0] [:inner 1]]}}
 :auto-add-ns-to-new-files? false
 :project-specs [{:project-path "deps.edn"
                  :classpath-cmd ["clojure" "-A:dev" "-Spath"]}]}
```

---
### Global

For global settings which should work for all the projects using `clojure-lsp`, you just need to add the same configs to `~/.lsp/config.edn` or `$XDG_CONFIG_HOME/.lsp/config.edn`.

For an example of a global `config.edn`, check [here](https://github.com/ericdallo/dotfiles/blob/master/.lsp/config.edn).

---
### InitializationOptions

This is specific for an client, where it sends on startup, check [LSP spec for more information](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize). This is useful if you are changing a default for a client/editor that will affect all users of that editor.

This is an [example how Emacs `lsp-mode`](https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-clojure.el#L205) pass custom information.

## All settings

| name                                | description                                                                                                                                                                                                                                                                                                                                                                                                          | default                                                                                                              |
|-------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| `source-paths`                      | project-local directories to look for clj/cljc/cljs files, if using `deps.edn`, use `:source-aliases` instead.                                                                                                                                                                                                                                                                                                       | `#{"src" "test"}`                                                                                                    |
| `source-aliases`                    | Used for `deps.edn` projects, the aliases which clojure-lsp should get the source-paths besides the root level `:paths` and `:extra-paths`. Check the `source-aliases discovery` section below.                                                                                                                                                                                                                                                                          | `#{:dev :test}`                                                                                                      |
| `linters`                           | clojure-lsp custom linters, check the diagnostics settings section below                                                                                                                                                                                                                                                                                                                                             | `{:unused-public-ns {:level :info}}`                                                                                 |
| `additional-snippets`               | Additional user snippets to be available during completing, check the snippets section below                                                                                                                                                                                                                                                                                                                         | `[]`                                                                                                                 |
| `ignore-classpath-directories`      | will not consider clojure files within the directories specified by your classpath. This is needed, for instance, if your build puts artifacts into `resources` or `target` that you want lsp to ignore.                                                                                                                                                                                                             | `false`                                                                                                              |
| `lint-project-files-after-startup?` | Whether to async lint all project only files after startup to make features like [List project errors](https://emacs-lsp.github.io/lsp-mode/page/main-features/#project-errors-on-modeline) work.                                                                                                                                                                                                                    | `true`                                                                                                               |
| `use-metadata-for-privacy?`         | Whether to use `^:private` metadata for refactorings instead of `defn-`                                                                                                                                                                                                                                                                                                                                              | `false`                                                                                                              |
| `keep-require-at-start?`            | Whether to keep first require/import at the first line instead of inserting a new line before it when using `clean-ns` refactoring.                                                                                                                                                                                                                                                                                  | `false`                                                                                                              |
| `semantic-tokens?`                  | Whether to enable LSP semantic tokens server support for syntax highlighting.  (Experimental)                                                                                                                                                                                                                                                                                                                        | `false`                                                                                                              |
| `show-docs-arity-on-same-line?`     | Whether to keep the arity on the same line of the function on hover, useful for Emacs users.                                                                                                                                                                                                                                                                                                                         | `false`                                                                                                              |
| `auto-add-ns-to-new-files?`         | Whether to automatically add the `ns` form in new blank files.                                                                                                                                                                                                                                                                                                                                                       | `true`                                                                                                               |
| `document-formatting?`              | if true or not present, document formatting is provided.                                                                                                                                                                                                                                                                                                                                                             | `true`                                                                                                               |
| `document-range-formatting?`        | if true or not present, document range formatting is provided.                                                                                                                                                                                                                                                                                                                                                       | `true`                                                                                                               |
| `text-document-sync-kind`           | The sync kind during document changes, if client should send whole buffer or just related changes. Should be `:full` or `:incremental`                                                                                                                                                                                                                                                                               | `:full`                                                                                                              |
| `dependency-scheme`                 | How the dependencies should be linked, `jar` will make urls compatible with java's JarURLConnection. You can have the client make an lsp extension request of `clojure/dependencyContents` with the jar uri and the server will return the jar entry's contents. [Similar to java clients](https://github.com/redhat-developer/vscode-java/blob/a24945453092e1c39267eac9367c759a6c7b0497/src/extension.ts#L290-L298) | `zip`                                                                                                                |
| `cljfmt`                            | Used for formatting, json encoded configuration for [cljfmt](https://github.com/weavejester/cljfmt)                                                                                                                                                                                                                                                                                                                  | `{}`                                                                                                                 |
| `project-specs`                     | A vector of a map with `project-path` and `classpath-cmd`, defining how `clojure-lsp` should find your project classpath. the `project-path` should be a file and the `classpath-cmd` the command to run to get the classpath                                                                                                                                                                                        | Check the default [here](https://github.com/clojure-lsp/clojure-lsp/blob/master/src/clojure_lsp/crawler.clj#L53-L60) |
| `lens-segregate-test-references`    | Segregate main references from test references with option to disable                                                                                                                                                                                                                                                                                                                                                | `true`                                                                                                               |
| `sqlite-db-path`                    | Where to store the project's analysis cache, used to speed up next `clojure-lsp` startup. A path relative to project root or an absolute path.                                                                                                                                                                                                                                                                       | `.lsp/sqlite.db`                                                                                                     |
| `log-path`                          | A absolute path to a file where clojure-lsp should log.                                                                                                                                                                                                                                                                                                                                                              | A JVM tmp path, usually `/tmp/clojure-lsp.*.out`                                                                     |

### Diagnostics (linter)

#### clj-kondo

`clojure-lsp` uses [clj-kondo](https://github.com/clj-kondo/clj-kondo) under the hood to lint the code and retrieve the analysis to
make most of features work, you don't have to install clj-kondo to
make it work.

For example, if you are a (neo)vim user and have [ale](https://github.com/dense-analysis/ale) installed as a plugin, you
**should not** have this configured as a linter `let g:ale_linters = {'clojure': ['clj-kondo']}` in your vimrc. Having this
linter enabled via `ale` will only conflict with the built-in clj-kondo bundled with clojure-lsp.

`clojure-lsp` will use a specific clj-kondo version that can be retrieved via `clojure-lsp --version`, but make sure you have it
properly configured in your `.clj-kondo/config.edn` file.

It has the possible key/values:

- `:level` with available values: `:off`, `:on` with default value of `:on`
- `:report-duplicates` which will show all linters of the same symbol instead of showing only the first spot. Available values: `true`, `false` with default value of `true`

Example:

```clojure
{:linters {:clj-kondo {:level :off
                       :report-duplicates false}}}
```

For more information about available configurations,
check the [clj-kondo configuration section](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md)

#### clojure-lsp

At the moment clojure-lsp has only the custom linters below:

##### unused-public-var

This linter reports public functions/vars not used over the project.

It has the possible key/values:

- `:level` with available values: `:info`, `:warning`, `:error` or `:off` with default value of `:info`.
- `:exclude` a whole namespace with `#{my-ns}` or a specific var `#{my-ns/foo}`.
- `:exclude-when-defined-by` excludes this linter when your var is defined by a macro for example, like `#{my-ns/deftest}`.

Example:

```clojure
{:linters {:unused-public-var {:level :warning
                               :exclude #{my-ns/foo
                                          my-ns/bar
                                          other-ns
                                          my-func}
                               :exclude-when-defined-by #{my-ns/defflow}}}}
```

For information on how to troubleshoot the linter, check the [troubleshooting section](https://clojure-lsp.github.io/clojure-lsp/troubleshooting/)

### Source paths discovery

Some features require know the available source paths of your project, where your code lives, clojure-lsp has some settings for that.

- If your project is not a `deps.edn` project, a `leiningen` project for example, clojure-lsp will use only the `:source-paths` setting (default `#{"src" "test"}`) which should point to the folders containing your clojure code.
- If your project is a `deps.edn`, clojure-lsp will scan the `deps.edn` file for `:paths`, `:extra-paths` and the paths from the specified `:source-aliases` setting (default `#{:dev :test}`), unless you specified `:source-paths` manually.

### Snippets

Besides the **19** built-in snippets, it's possible to configure custom additional snippets via `:additional-snippets` setting:

- `:name` the name to use while completing to reach that snippet, preferably with a `$` sufix to indicate a snippet.
- `:detail` Custom text to show along with the completion name.
- `:snippet` The body of the snippet, besides any text it can contains:
    - `$1`, `$2`, ... as the tabstops representing each place where user may change the content.
    - `$0` as the last tabstop.
    - `$current-form` to replace the current form in the snippet.

Example:

```clojure
{:additional-snippets [{:name "wrap-let-sexpr$"
                        :detail "Wrap current sexpr in let"
                        :snippet "(let [$1] $0$current-form)"}]}
```

when completion is called on the code below with the cursor as `|`
```clojure
wrap|(+ 1 2)
```

It should return a completion item that after applied should result in:

```clojure
(let [|] (+ 1 2))
```
