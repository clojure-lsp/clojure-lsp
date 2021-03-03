# Troubleshooting

Below are the common issues ordered by the most important to specific ones:

## Server is not initializing 

Make sure you have the [most recent version of `clojure-lsp`](https://clojure-lsp.github.io/clojure-lsp/installation/#native-binary-recommended)

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

---

## Some features are not working

clojure-lsp uses [clj-kondo](https://github.com/clj-kondo/clj-kondo) to scan the classpath 
during server initialize for most features work, so make sure you don't see any "Error while looking up classpath..." on clojure-lsp log file.

### Classpath scan error

By default clojure-lsp knows how to scan most common clojure projects using the following rules:

- If the project root has a `project.clj` file, it'll run `lein classpath` to get the classpath.
- If the project root has a `deps.edn` file, it'll run `clojure -Spath` to get the classpath.
- If the project root has a `build.boot` file, it'll run `boot show --fake-classpath` to get the classpath.
- If the project root has a `shadow-cljs.edn` file, it'll run `npx shadow-cljs classpath` to get the classpath.

If your project doesn't follow the above rules or you need a custom command to get the classpath you need to configure the `project-specs` clojure-lsp setting, for more details check the[settings section](https://clojure-lsp.github.io/clojure-lsp/settings/).

### Folders not being analyzed

By default clojure-lsp searches `src` and `test` for clj* files to read into an index.
If the definition lives under a different source dir, you can define the `src-paths` setting, for more details check the [settings section](https://clojure-lsp.github.io/clojure-lsp/settings/).

It is also important to get your `project-root` correct in your client otherwise the source paths will not be found, check the project-root via your LSP client.

### Wrong diagnostics/lint

- clojure-lsp persist the external jars analysis in a `.lsp/sqlite.db` file, if you have issues with some specific feature, 
try to remove that file and restart the server.
- clojure-lsp use clj-kondo to lint and cache in a `.clj-kondo/.cache` dir, try to remove that file as well if you think it's not linting correctly
- If you have issues with macros, [double check your clj-kondo config](https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#unrecognized-macros).

---
## MacOS

In some version of MacOS, Apple restrict the binary to run, to fix that run: `xattr -d com.apple.quarantine /path/to/clojure-lsp`
