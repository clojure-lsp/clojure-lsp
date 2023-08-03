# Clients

Clients are either editors with built in LSP support like Oni or nvim, or an appropriate plugin.
**Clients are responsible for launching the server, the server is a subprocess of your editor not a daemon.**

In general, make sure to configure the client to use stdio and a server launch command like `['/usr/local/bin/clojure-lsp']`.
If that fails, you may need to have your client launch inside a shell, so use something like `['bash', '-c', '/usr/local/bin/clojure-lsp']`.
In windows you probably need to use the `clojure-lsp.bat`.

**TIP**: If your editor is not started from the terminal then it is possible that its `$PATH` is quite different from the one in the shell and clojure-lsp will not be able to find the executables it needs, such as `clojure` and `npx`. To alleviate that, create a wrapper script that sets the `PATH` correctly. Make sure that nothing in the script uses stdin / stdout as these are used for communication with the editor and any extra input/output could mess it up. An example script (also remember to make it executable, e.g. `chmod u+x <script>`):

```bash
#!/bin/bash -l
if [ ! -e npx ]; then
  export PATH=$PATH:/Users/me/.nvm/versions/node/v10.24.1/bin
fi
exec /usr/local/bin/clojure-lsp
```

---

## Emacs

[lsp-mode](https://emacs-lsp.github.io/lsp-mode) has built in support for `clojure-lsp`. With `use-package`, add the following to your emacs config:

```elisp
(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("/path/to/clojure-lsp"))) ;; Optional: In case `clojure-lsp` is not in your $PATH
```

Optionally you can add `lsp-ui` for UI feedback and `company-mode` for completion:

```elisp
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t)
```

In `lsp-mode`, `lsp-clojure-server-command` variable is available to override the command to start the `clojure-lsp` server, might be necessary to do this on a Windows environment.

For a detailed guide on how to configure Emacs with LSP, check [here](https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/)

For more `lsp-mode` clojure settings, check [here](https://emacs-lsp.github.io/lsp-mode/page/lsp-clojure/)

---

## Visual Studio Code

[Calva](https://calva.io/clojure-lsp/) is a extension for VSCode for Clojure Development that includes clojure-lsp.

---

## Vim

I prefer https://github.com/neoclide/coc.nvim but both http://github.com/autozimu/LanguageClient-neovim and https://github.com/prabirshrestha/vim-lsp work well.

See my [nvim/init.vim](https://github.com/snoe/dotfiles/blob/master/home/.vimrc) and [coc-settings.json](https://github.com/snoe/dotfiles/blob/master/home/.vim/coc-settings.json)

Another example of a Neovim Conjure/CoC/clojure-lsp setup can be found on [dharrigan's github](https://github.com/dharrigan/vimconfig) repository.

LanguageClient-neovim can be configure with:

Refactorings:
```vim
function! Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : "file://" . l:result
endfunction

nnoremap <silent> crcc :call LanguageClient#workspace_executeCommand('cycle-coll', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crth :call LanguageClient#workspace_executeCommand('thread-first', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtt :call LanguageClient#workspace_executeCommand('thread-last', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtf :call LanguageClient#workspace_executeCommand('thread-first-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtl :call LanguageClient#workspace_executeCommand('thread-last-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crml :call LanguageClient#workspace_executeCommand('move-to-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> cril :call LanguageClient#workspace_executeCommand('introduce-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> crel :call LanguageClient#workspace_executeCommand('expand-let', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> cram :call LanguageClient#workspace_executeCommand('add-missing-libspec', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
```

`initializationOptions` can be sent by setting:
`let g:LanguageClient_settingsPath=".lsp/settings.json"`

Project-local `.lsp/settings.json` would have content like:
```clojure
{"initializationOptions": {
   "source-paths": ["shared-src", "src", "test", "dashboard/src"],
   "macro-defs": {"project.macros/dofor": ["bindings", "bound-elements"]}}}
```

Further refactoring mappings include being able to invoke the
`(coc-codeaction-*)` function to provide actions such as `move to
let`, `change coll to map` and so on.

```
nmap <silent> <Leader>cr            <Plug>(coc-rename)
nmap <silent> <Leader>cf            <Plug>(coc-references)
xmap <silent> <Leader>c             <Plug>(coc-codeaction-selected)
nmap <silent> <Leader>c             <Plug>(coc-codeaction-line)
nmap <silent> gd                    <Plug>(coc-definition)
```

### Nvim

An very detailed document about how configure Nvim as Clojure IDE using fennel, clojure-lsp and conjure.  
[nvim-fennel-lsp-conjure-as-clojure-ide](https://github.com/rafaeldelboni/nvim-fennel-lsp-conjure-as-clojure-ide)

---

## Sublime Text

Clojure LSP can be installed by first installing the [LSP plugin](https://packagecontrol.io/packages/LSP) which brings Language Server Protocol support to Sublime Text editor and then following the set-up instructions [here](https://lsp.sublimetext.io/language_servers/#clojure) to download Clojure LSP and how to configure it in Sublime Text.

---

## Intellij

Install the [LSP Support plugin](https://github.com/ballerina-platform/lsp4intellij) and configure it to launch clojure-lsp for Clojure/Script files:

1. Go to Preferences / Languages & Frameworks / Language Server Protocol / Server Definitions. Select *Raw command*.
2. In the *Extension* field enter `clj;cljs;cljc;edn`.
3. In the *Command* field enter the path to the executable, for instance `/usr/local/bin/clojure-lsp`.

BEWARE: The LSP Support plugin has short timeouts and will kill clojure-lsp silently if it exceeds the limit. It is highly recommended that you increase the *Init* timeout from 10000 (10s) to e.g. 300000 (5 min). The first scan can easily take over a minute on a bigger project. See Preferences / Languages & Frameworks / Language Server Protocol / Timeouts. If the plugin's icon in the status bar (a circle) turns red, it means starting clojure-lsp failed. Click on it and select *Show timeouts* - any limits that were exceeded will be in red.

---

## Oni

Seems to work reasonably well but couldn't get rename to work reliably https://github.com/onivim/oni

---
## Atom

I tried making a client but my hello world attempt didn't seem to work. If someone wants to take this on, I'd be willing to package it here too.
