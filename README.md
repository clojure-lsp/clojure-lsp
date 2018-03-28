A [Language Server](https://microsoft.github.io/language-server-protocol/) for Clojure. Taking a Cursive like approach of statically analyzing code rather than depending on a repl and runtime.

This is a very early work in progress, contributions are very welcome.

## Clients

Tested in various language server clients. In general use stdio and a command like `['bash', '-c', 'cd ~/dev/lsp/ && clj -m clojure-lsp.main']` (I didn't want to figure out working dir/classpaths for `clj`, let me know a better command). You can get `clj` on macs with `brew install clojure`

Something that took me too long to realize about LSP is that, in general, the client is responsible for launching the server process. 

### Vim 
Both http://github.com/autozimu/LanguageClient-neovim and https://github.com/prabirshrestha/vim-lsp work well. I think supporting completionItem `additionalTexts` is important for auto-imports and the former might be a bit closer (various PRs with omnicomplete)

### Intellij / Cursive
https://github.com/gtache/intellij-lsp tested only briefly. 

### vscode
I was able to modify [lsp-sample](https://github.com/Microsoft/vscode-extension-samples/tree/master/lsp-sample) to run the server. I plan to package the client as part of this repo unless someone points me at a generic client that can launch different servers based on user settings. (One extension per language server feels weird to me, what's the reasoning behind it?).

### atom
I tried making a client but my hello world attempt didn't seem to work. If someone wants to take this on, I'd be willing to package it here too. 

### emacs
https://github.com/emacs-lsp looks promising but I haven't had a chance to try it (similar one client per server).

## TODO

### Diagnostics 
- crawl through parsed results and see if symbols exist where used
- unused imports

### Others
- Better completion item kinds
- rename
- find usages
- formatting (clj-format?)
- other lsp capabilities?
- Cursive style "resolve macros as" def/defn/let etc.. to expose more vars
- crawl classpath for exported vars 
- crawl project.clj / build.boot src paths
- keep separate cljs and clj environments
- handle cljc reader-macros

## Out of scope for now:
 - :import and deftypes, defrecord etc...

