# Troubleshooting

## Server log

clojure-lsp logs most of what is doing to a file which location could be found:

- on `:log-path` if specified in your clojure-lsp config, for example: `{:log-path "/tmp/clojure-lsp.out"}`
- Auto generated every server start on `/tmp/clojure-lsp.<TIMESTAMP>.out`

## Client<->Server log

All LSP clients should provide a way to get the jsonrpc logs between client and server, this helps debug the requests and responses content and time.

Check below how to get the logs for most used clients:

- [Emacs/lsp-mode](https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/#log-client-server-json)
- [VSCode/Calva](https://calva.io/clojure-lsp/#viewing-the-logs-between-the-client-and-server)

## Server is not initializing

Make sure you have the [most recent version of `clojure-lsp`](./installation.md#native-binary-recommended)

Check if the executable is working running it from the command line, it should start up and start reading from stdin.
Type `{}\n\n` and you should get something like:

```
$ clojure-lsp
{}

Apr 12, 2019 7:07:02 AM org.eclipse.lsp4j.jsonrpc.json.StreamMessageProducer fireError
SEVERE: Missing header Content-Length in input "{}

"
java.lang.IllegalStateException: Missing header Content-Length in input "{}""""

"""
```

If that is ok, clojure-lsp logs to `/tmp/clojure-lsp.*.out`, so watch that file and start your editor.

LSP Clients also generally have a way to trace server interactions. Turn that on and attach both server and client logs to an issue if it's not obvious what's going on.

For example, if you are using [neovim](https://neovim.io/) with
[CoC](https://github.com/neoclide/coc.nvim), first ensure that
`trace.server` is set to `verbose` in your `coc-settings.json` file,
e.g.,

```json
  "languageserver": {
    "clojure-lsp": {
      "command": "clojure-lsp",
      "filetypes": ["clojure"],
      "disableDiagnostics": false,
      "rootPatterns": ["deps.edn", "project.clj"],
      "additionalSchemes": ["jar", "zipfile"],
      "trace.server": "verbose",
      "initializationOptions": {
        "project-specs": [{
          "project-path": "deps.edn",
          "classpath-cmd": ["clj", "-Spath"]
        }],
        "use-metadata-for-privacy?": true,
        "ignore-classpath-directories": true
      }
    }
  }
```

Then, once vim has loaded (and clojure-lsp has initialised), you can
issue this command:

`:CocCommand workspace.showOutput`

This will show the JSON request/response bodies that go between vim
and clojure-lsp. Please capture that information if you need help in
tracking down the problem you are experiencing (either by reporting
github issues, or talking with someone in Slack/Discord or
whatever...)

---

## Some features are not working

clojure-lsp uses [clj-kondo](https://github.com/clj-kondo/clj-kondo) to scan the classpath
during server initialize for most features work, so make sure you don't see any "Error while looking up classpath..." on clojure-lsp log file.

Please note that `clojure-lsp` comes bundled with `clj-kondo`, so you do not have to install it separately.

For more information, check the [Classpath scan](./settings.md#classpath-scan) settings section.

### Classpath scan error

By default clojure-lsp knows how to scan most common clojure projects using the following rules:

- If the project root has a `project.clj` file, it'll run `lein classpath` to get the classpath.
- If the project root has a `deps.edn` file, it'll run `clojure -Spath` to get the classpath.
- If the project root has a `build.boot` file, it'll run `boot show --fake-classpath` to get the classpath.
- If the project root has a `shadow-cljs.edn` file, it'll run `npx shadow-cljs classpath` to get the classpath.

If your project doesn't follow the above rules or you need a custom command to get the classpath you need to configure the `project-specs` clojure-lsp setting, for more details check the[settings section](./settings.md).

### Folders not being analyzed

By default clojure-lsp searches `src` and `test` for clj* files to read into an index.

* If the definition lives under a different source dir, you can define the `source-paths` setting, for more details check the [settings section](./settings.md).

* It is also important to get your `project-root` correct in your client otherwise the source paths will not be found, check the project-root via your LSP client.

* If you are using `deps` and using a `:local/root` dependency to reference another project, i.e.,

```clojure
{:deps {foo.bar/baz {:local/root "/path/to/foo/project/containing/a/deps.edn"}}}
```

* and you are finding that `gotoDefinition` isn't working when attempting to jump to the namespace in the referenced project, then
it could be that your `~/.lsp/config.edn` (or `~/.config/clojure-lsp/config.edn`) has a source paths entry, i.e., `:source-paths
["src" "test"]`. This will prevent the lookup from working, as it restricts clojure-lsp to only scan those folders in the
current project for sources, and not the other project referenced via the `:local/root` deps entry. It can be fixed by removing
the `:source-paths` from the config (as clojure-lsp has good defaults anyway). If you do require more specific source paths,
then those can be added at the project level.

### Wrong diagnostics/lint

- clojure-lsp persist the external jars analysis in a `.lsp/sqlite.db` file, if you have issues with some specific feature,
try to remove that file and restart the server.
- clojure-lsp use clj-kondo to lint and cache in a `.clj-kondo/.cache` dir, try to remove that file as well if you think it's not linting correctly
- If you have issues with macros, [double check your clj-kondo config](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#unrecognized-macros).

### Missing `Add require...` on code actions when using CoC and (neo)vim

If you find, when executing the command
`(coc-codeaction-line)` (or `(coc-codeaction-selected)` or
`(coc-codeaction-cursor)`), that you aren't getting back
all of the code actions you might expect, please ensure that you have,
in your `coc-settings.json` the line `disableDiagnostics` set to
`false` or better yet, don't have the line there at all :-)

---
## MacOS

In some version of MacOS, Apple restrict the binary to run, to fix that run: `xattr -d com.apple.quarantine /path/to/clojure-lsp`
