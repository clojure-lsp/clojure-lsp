# clojure-lsp Development

There are several ways of finding and fixing a bug or implementing a new feature:

- [The Clojure Way](#the-clojure-way)
- Create a test for your bug/feature, then implement the code following the test (TDD).
- Build `clojure-lsp` using `make` each time you have made changes, and test it manually in your client. This is the slowest option.

Whichever development path you choose: For final testing, it is good to rebuild the binary with `make`.

There are two custom LSP methods `clojure/serverInfo/log` and `clojure/cursorInfo/log`. They can assist in debugging.

## The Clojure Way

With a **clojure-lsp + [nREPL](https://nrepl.org)** powered Clojure editor you can modify your editor session's clojure-lsp server using the Clojure REPL.

Here's demo video: https://www.youtube.com/watch?v=4UvT0yqBDw8

These are the steps:

1. `make` - to build a `clojure-lsp` executable that includes cider-nrepl in the jar. This executable will be saved at the root of the project.
1. Configure your editor to use this `clojure-lsp` executable
1. Have your editor restart its clojure-lsp server
1. Issue the clojure-lsp `serverInfo` command
1. Find the `port` entry in the output
1. Connect your editors nREPL client to this port
1. Hack away!

**Seeing is believing.** An easy way to convince yourself that you can actually change clojure-lsp mid-flight is to:

1. Modify the `server-info` function in `src/clojure_lsp/handlers.clj`
    - Say, you add a `:foo :bar` entry to the map returned
1. Evaluate the new `server-info` function definition
1. Issue the clojure-lsp `serverInfo` command
1. Find `:foo :bar` in the output

You have just modified the LSP server powering your editor while it was running! This is the Clojure way. No recompiling and restarting and reloading. That is some other, non-Clojure, way.

The details in how to perform these steps can vary a bit between the various Clojure editors/plugins.

### Visual Studio Code with Calva

* This project comes with [Calva](https://calva.io) configuration to use the `clojure-lsp` executable built in step 1 above. You can skip step 2.
* To restart the clojure-lsp server, use the VS Code command **Developer: Reload Window**
* The **Hack away!** step needs to start with you issuing the command **Calva: Load Current File and Dependencies**.

### Emacs with CIDER

* To configure Emacs to use the nREPL-enabled executable, run `(setq lsp-clojure-custom-server-command '("~/path/to/clojure-lsp/clojure-lsp"))`, adjusting the path as necessary. If you add this to your Emacs config, you can skip this step in the future.
* To restart the clojure-lsp server, execute the Emacs command `lsp-workspace-restart`.
* To find the server info, execute `lsp-clojure-server-info`.
* To connect the nREPL client, run `cider-connect-clj`, with "localhost" and the port.

If you re-connect regulary, you may want to add this Emacs shortcut:

```emacs-lisp
(defun lsp-clojure-nrepl-connect ()
  "Connect to the running nrepl debug server of clojure-lsp."
  (interactive)
  (let ((info (lsp-clojure-server-info-raw)))
    (save-match-data
      (when-let (port (and (string-match "\"port\":\\([0-9]+\\)" info)
                           (match-string 1 info)))
        (cider-connect-clj `(:host "localhost"
                             :port ,port))))))
```

### Your Favorite Editor

TBD. PR welcome.
