<img src="https://user-images.githubusercontent.com/7820865/103157675-3da6b700-4794-11eb-9771-d2da1dd9b7a7.png" width="180" align="right">

![CI](https://github.com/snoe/clojure-lsp/workflows/CI/badge.svg?branch=master)

# clojure-lsp

A [Language Server](https://microsoft.github.io/language-server-protocol/) for Clojure. Taking a Cursive-like approach of statically analyzing code.

## What is this?

The goal of this project is to bring great editing tools for Clojure to all editors.
It aims to work alongside you to help you navigate, identify and fix errors, and perform refactorings.

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

---
## Installation

### Manually

- You need `java` on your $PATH.
- Grab the latest `clojure-lsp` from github [LATEST](https://github.com/snoe/clojure-lsp/releases/latest)
- Place it in your $PATH with a chmod 755
- Follow the documentation for your editor's language client. See [Clients](#clients) below.

### Nix

`clojure-lsp` is available in the [nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/misc/clojure-lsp/default.nix):

```bash
nix-shell -p clojure-lsp
```

---
## Building manually

For building manually, run `lein bin` to generate the binary inside `target` folder or `lein uberjar` for building the standalone jar.

---
## Development

For `clojure-lsp` development, there are 3 possible ways of finding a bug or implementing a new feature:
- Create a test for your bug/feature, then implement the code following the test.
- `clojure-lsp` starts a NREPL server, with that it's possible to change the code of a running instance and see the changes on your client in real time. To get the NREPL port, you can check the `/tmp/clojure-lsp.out` log, it will print the NREPL port on server startup or you can get it via `server-info` custom LSP command.
- Build `clojure-lsp` with your changes and test it manually in your client, this is the slowest option, but it makes sense final tests.

---
## Contribution

Contributions to `clojure-lsp` are very welcome! You can open an issue or a PR and we'd love to help.

---
## Support the project

`clojure-lsp` has more than 8.000 lines of code, to keep all of this working, we need to help the community on a lot of issues and implement new features. As a LSP server, this project is the base for Clojure clients like Emacs(lsp-mode), VSCode(Calva) and vim.

You can help us keep going and improving it by **[supporting the project](https://opencollective.com/clojure-lsp)**

[![](https://opencollective.com/clojure-lsp/tiers/backer.svg)](https://opencollective.com/clojure-lsp)
