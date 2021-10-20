# clojure-lsp Development

There are several ways of finding and fixing a bug or implementing a new feature:

- [The Clojure Way](#the-clojure-way)
- Create a test for your bug/feature, then implement the code following the test (TDD).
- Build `clojure-lsp` using `make` each time you have made changes, and test it manually in your client. This is the slowest option.

Whichever development path you choose: For final testing, it is good to rebuild the binary with `make`.

There are two custom LSP methods `clojure/serverInfo/log` and `clojure/cursorInfo/log`. They can assist in debugging.

## The Clojure Way

With a **clojure-lsp + [nREPL](https://nrepl.org)** powered Clojure editor you can modify your editor session's clojure-lsp server using the Clojure REPL. These are the steps:

1. Configure your editor to use the `clojure-lsp` executable from this project
1. `make` - to build the clojure-lsp executable
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

* This project comes with [Calva](https://calva.io) configuration to use the `clojure-lsp` executable built in step 2 above. You can skip step 1.
* To restart the clojure-lsp server, use the VS Code command **Developer: Reload Window**
* The **Hack away!** step needs to start with you issuing the command **Calva: Load Current File and Dependencies**.

### Emacs with CIDER

TBD. PR welcome.

### Your Favorite Editor

TBD. PR welcome.