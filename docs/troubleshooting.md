# Troubleshooting

## Logs

### Server log

clojure-lsp logs most of what is doing to a file which location could be found:

- on `:log-path` if specified in your clojure-lsp config, for example: `{:log-path "/tmp/clojure-lsp.out"}`
- Auto generated every server start on `/tmp/clojure-lsp.<TIMESTAMP>.out` or `/var/folders/...` for MacOS.

<details>
<summary><b>Emacs users</b></summary>
You can open server logs in a buffer with <code>M-x</code> <code>lsp-clojure-server-log</code>.

</details>

### Client<->Server log

All LSP clients should provide a way to get the jsonrpc logs between client and server, this helps debug the requests and responses content and time.

Check below how to get the logs for most used clients:

<details>
<summary><b>Emacs/lsp-mode</b></summary>
<a href="https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/#log-client-server-json">https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/#log-client-server-json</a>

</details>

<details>
<summary><b>VSCode/Calva</b></summary>
<a href="https://calva.io/clojure-lsp/#viewing-the-logs-between-the-client-and-server">https://calva.io/clojure-lsp/#viewing-the-logs-between-the-client-and-server</a>

</details>

An alternative is to pass `--trace-level verbose` to clojure-lsp during process start and clojure-lsp will log the communication to the server-log.

### Server Info

clojure-lsp has a custom command that prints useful information about the current running clojure-lsp for the current project, it's called `clojure/serverInfo/log`, some clients already have ways to call that automatically:

- Emacs lsp-mode: via `lsp-clojure-server-info` command.
- VsCode Calva: via `Calva Diagnostics: Clojure-lsp Server Info` command.
- Intellij clojure-lsp plugin: via `Settings` `Tools` `Clojure LSP` `Copy server info to clipboard` option.

## Some features are not working

clojure-lsp uses [clj-kondo](https://github.com/clj-kondo/clj-kondo) to analyze the classpath
during server initialize for most features work, so make sure you don't see any "Error while looking up classpath..." on clojure-lsp log file.

Please note that `clojure-lsp` comes bundled with `clj-kondo`, so you do not have to install it separately.

For more information, check the [Classpath scan](settings.md#classpath-scan) settings section.

### Classpath scan error

By default clojure-lsp knows how to scan most common clojure projects using the following rules:

- If the project root has a `project.clj` file, it'll run `lein classpath` to get the classpath.
- If the project root has a `deps.edn` file, it'll run `clojure -Spath` to get the classpath.
- If the project root has a `build.boot` file, it'll run `boot show --fake-classpath` to get the classpath.
- If the project root has a `shadow-cljs.edn` file, it'll run `npx shadow-cljs classpath` to get the classpath.

If your project doesn't follow the above rules or you need a custom command to get the classpath you need to configure the `project-specs` clojure-lsp setting, for more details check the [settings section](settings.md).

### Folders not being analyzed/linted

By default clojure-lsp get source-paths from classpath, for more details check [settings section](settings.md#source-paths-discovery).

* If the definition lives under a different source dir, you can define `source-aliases` or `source-paths` setting as mentioned on [settings section](settings.md#source-paths-discovery).

* It is also important to get your `project-root` correct in your client otherwise the source paths will not be found, check the project-root via your LSP client.

* If you are using `deps` and using a `:local/root` dependency to reference another project, i.e.,

```clojure
{:deps {foo.bar/baz {:local/root "/path/to/foo/project/containing/a/deps.edn"}}}
```

* and you are finding that `gotoDefinition` isn't working when attempting to jump to the namespace in the referenced project, then
it could be that your `~/.config/clojure-lsp/config.edn` (or legacy `~/.lsp/config.edn`) has a source paths entry, i.e., `:source-paths
["src" "test"]`. This will prevent the lookup from working, as it restricts clojure-lsp to only scan those folders in the
current project for sources, and not the other project referenced via the `:local/root` deps entry. It can be fixed by removing
the `:source-paths` from the config (as clojure-lsp has good defaults anyway). If you do require more specific source paths,
then those can be added at the project level.

### Wrong diagnostics/lint

- clojure-lsp use clj-kondo to lint and cache in a `.clj-kondo/.cache` dir, try to remove that folder if you think it's not linting correctly
- clojure-lsp persist the external jars analysis in a `.lsp/.cache/` folder, if you have issues with some specific feature,
try to remove that dir and restart the server.
- If you have issues with macros, [double check your clj-kondo config](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#unrecognized-macros).

### Missing `Add require...` on code actions when using CoC and (neo)vim

If you find, when executing the command
`(coc-codeaction-line)` (or `(coc-codeaction-selected)` or
`(coc-codeaction-cursor)`), that you aren't getting back
all of the code actions you might expect, please ensure that you have,
in your `coc-settings.json` the line `disableDiagnostics` set to
`false` or better yet, don't have the line there at all :-

---)

## Server is not initializing

Make sure you have the [most recent version of `clojure-lsp`](installation.md#native-binary-recommended)

Check if the executable is working running it from the command line, it should start up and start reading from stdin.
Type `Content-Length: 51\n\n{"jsonrpc":"2.0","method":"foo","id":1,"params":{}}`. After a few moments you should get something like:

```
$ ./clojure-lsp
Content-Length: 51

{"jsonrpc":"2.0","method":"foo","id":1,"params":{}}

Content-Length: 101

{"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"Method not found","data":{"method":"foo"}}}
```

If that is ok, clojure-lsp logs to `/tmp/clojure-lsp.*.out`, so watch that file and start your editor.

LSP Clients also generally have a way to trace server interactions. Turn that on and attach both server and client logs to an issue if it's not obvious what's going on.

<details>
<summary><b>Vim users</b></summary>
For example, if you are using <a href="https://neovim.io/">neovim</a> with
<a href="https://github.com/neoclide/coc.nvim">CoC</a>, first ensure that
<code>trace.server</code> is set to <code>verbose</code> in your <code>coc-settings.json</code> file,
e.g.,

<pre>
<code>
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
      }
    }
  }
</code>
</pre>

Then, once vim has loaded (and clojure-lsp has initialised), you can
issue this command:

<code>:CocCommand workspace.showOutput</code>

This will show the JSON request/response bodies that go between vim
and clojure-lsp. Please capture that information if you need help in
tracking down the problem you are experiencing (either by reporting
github issues, or talking with someone in Slack/Discord or
whatever...)

</details>

---
## High memory usage

clojure-lsp uses a native image that helps a lot with memory usage, but there is no hard limit so for huge Clojure projects or projects with lots of dependencies, memory may be a issue because of multiple analysis (keywords, function usages, java class/members etc).

It's possible to retrieve the project analysis of a running clojure-lsp process via the [serverInfo](#server-info) command, a `:analysis` map will contain both internal (your project) and external (project dependencies) count of each analysis used by clojure-lsp, extremally high count of elements (> hundred thousand) tends to increase memory usage + cache size (`.lsp/.cache`).

As last resource if your project has a enormous number of some specific elements, it's possible to disable some analysis which should help decrease memory usage and even increase performance via the `:analysis` setting, but keep in mind that some features related to those analysis may not work, for example, `{:analysis {:keywords false}}` will remove support for find-references of keywords, or `{:analysis {:java {:class-definitions false :member-definitions false}}}` will completly disable java features like auto completion of java elements.

## MacOS

In some version of MacOS, Apple restrict the binary to run, to fix that run: `xattr -d com.apple.quarantine /path/to/clojure-lsp`
