<img src="images/logo-dark.svg" width="180" align="right">

![CI](https://img.shields.io/github/workflow/status/clojure-lsp/clojure-lsp/CI?style=flat-square)
[![Slack community](https://img.shields.io/badge/Slack-chat-blue?style=flat-square)](https://clojurians.slack.com/archives/CPABC1H61)
[![Github starts](https://img.shields.io/github/stars/clojure-lsp/clojure-lsp?style=social)](https://github.com/clojure-lsp/clojure-lsp)


# clojure-lsp

A [Language Server](https://microsoft.github.io/language-server-protocol/) for Clojure. Taking a Cursive-like approach of statically analyzing code.

<hr>
<p align="center">
  <a href="https://clojure-lsp.github.io/clojure-lsp"><strong>homepage</strong></a> •
  <a href="https://clojure-lsp.github.io/clojure-lsp/features/"><strong>features</strong></a> •
  <a href="https://clojure-lsp.github.io/clojure-lsp/installation/"><strong>installation</strong></a> •
  <a href="https://clojure-lsp.github.io/clojure-lsp/settings"><strong>settings</strong></a> •
  <a href="https://clojure-lsp.github.io/clojure-lsp/clients"><strong>clients</strong></a> •
  <a href="https://clojure-lsp.github.io/clojure-lsp/troubleshooting"><strong>troubleshooting</strong></a> •
  <a href="https://opencollective.com/clojure-lsp"><strong>support us</strong></a>
</p>
<hr>

## Overview 

The goal of this project is to bring great editing tools for Clojure to all editors.
It aims to work alongside you to help you navigate, identify and fix errors, perform refactors and much more!

You will get:

- **Autocomplete**
- **Jump to definition**
- **Find references**
- **Renaming**
- **Code actions**
- **Errors**
- **Automatic ns management**
- **Refactorings**
- **Code lens**
- **Semantic tokens (syntax highlighting)**
- **Call hierarchy**

---
## Contribution

Contributions to `clojure-lsp` are very welcome! You can open an issue or a PR and we'd love to help.

---
## Building

For building the jar manually, run `lein bin` to generate the embedded jar inside `target` folder or `lein uberjar` for building the standalone jar.

### GraalVM

Every release the native binaries (Windows, Linux and MacOS) are compiled with __GraalVM__ and uploaded.

To build a native image with GraalVM:

- Install the GraalVM 21.0.0 for Java 11 and set the `GRAALVM_HOME` to the installation dir 
- Install `native-image` with `$GRAALVM_HOME/bin/gu install native-image` 
- Run from `clojure-lsp` project root `./graalvm/native-unix-compile.sh` or `./graalvm/native-windows-compile.bat`. 

The build may take some minutes and the result will be a `./clojure-lsp` native binary.

---
## Development

For `clojure-lsp` development, there are 3 possible ways of finding a bug or implementing a new feature:

- Create a test for your bug/feature, then implement the code following the test.
- `clojure-lsp` starts a NREPL server if built with `lein with-profile :debug bin`, with that it's possible to change the code of a running instance and see the changes on your client in real time. To get the NREPL port, you can check the `/tmp/clojure-lsp.*.out` log, it will print the NREPL port on server startup or you can get it via `server-info` custom LSP command.
- Build `clojure-lsp` with your changes and test it manually in your client, this is the slowest option, but it makes sense for final tests.
- For debugging purposes, there is two custom commands `server-info` and `cursor-info`.

---
## Support the project

`clojure-lsp` has more than 8.000 lines of code, to keep all of this working, we need to help the community on a lot of issues and implement new features. As a LSP server, this project is the base for Clojure clients like Emacs(lsp-mode), VSCode(Calva) and vim.

You can help us keep going and improving it by **[supporting the project](https://opencollective.com/clojure-lsp)**

[![](https://opencollective.com/clojure-lsp/tiers/backer.svg)](https://opencollective.com/clojure-lsp)

Special thanks to [Eccentric-J](https://eccentric-j.com/) for the `clojure-lsp` logo
