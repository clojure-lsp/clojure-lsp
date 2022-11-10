### CI

Having [clojure-lsp cli](./cli.md) in your CI machine is enough to run any task, if using Github Actions, you can use [setup-clojure-lsp](https://github.com/marketplace/actions/setup-clojure-lsp) GitHub action to install clojure-lsp in your CI.

Remember to install your build tool, like `clojure` or `leiningen` as well in your CI according to your project to clojure-lsp be able to scan the classpath correctly.
