# Troubleshooting

## It doesn't seem to be working

If you're downloading the release, try running it from the command line, it should start up and start reading stdin.
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

If that is ok, clojure-lsp logs to `/tmp/clojure-lsp.out`, so watch that file and start your editor.

LSP Clients also generally have a way to trace server interactions. Turn that on and attach both server and client logs to an issue if it's not obvious what's going on.

---
## Go to definition doesn't work

### Double check settings

By default clojure-lsp searches `src` and `test` for clj* files to read into an index.
If the definition lives under a different source dir, you can defiine `src-paths` in your client's `InitializationOptions`.

It is also important to get your `project-root` correct in your client otherwise the source paths will not be found.
Usually there's an option to search for a project file, so for a lein project you would have your client search for `project.clj`.

### Check the logs

There are a couple reasons why go to definition could fail. First, start tailing `/tmp/clojure-lsp.out`.

If there's a parse error in either the definition or usage files then lsp can't add anything from those files into the index, there will be a `Cannot parse: <file>` log for that situation. Please raise an issue if you run have a file that cannot be parsed.

Otherwise, the logs will generally explain why go to definition failed.

If the logs explain the symbol under the cursor is not found (and you disagree), there could be a bug or your client is not properly synchronizing the file's contents. Please raise an issue if this happens.

### The definition is still not found

If the logs explain that a definition could not be found, there could be reasons:

You can check the definition by going to the definition and invoking lsp's `Hover` action, the definition should have a tag of `:declare` and if it's in another file from the usage it also needs a `:public` tag.

If the definition is declared with an unknown macro then you could use `macro-defs` to let lsp learn about it.

You can make sure that the definition and the usage's fully qualified symbols (using `Hover`) are the same.

If none of that helps raise an issue or ask in clojurians slack.

---
## Windows

See https://github.com/snoe/clojure-lsp/issues/28 and https://github.com/snoe/clojure-lsp/issues/25