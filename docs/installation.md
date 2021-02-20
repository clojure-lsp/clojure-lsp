# Installation

Below are all available `clojure-lsp` installation methods, after installed, follow the documentation for your editor's language client. See [Clients](//clojure-lsp.github.io/clojure-lsp/clients).

## Native binary (recommended)

`clojure-lsp` builds using GraalVM a native executable for each OS: Linux, MacOS and Windows.

### Manually

The binaries are available on [Github releases](https://github.com/clojure-lsp/clojure-lsp/releases) as `clojure-lsp-native-<os>-<arch>.zip`, after downloading you just need to unzip it.

### Script

You can install it running [./install-latest-clojure-lsp.sh](https://github.com/clojure-lsp/clojure-lsp/blob/master/install-latest-clojure-lsp.sh)

## Embedded jar (Legacy executable)

In Github releases you will find a `clojure-lsp` file that runs a embedded jar. This should be removed soon, use GraalVM binaries instead.

- You need `java` on your $PATH.
- Grab the latest `clojure-lsp` from github [LATEST](https://github.com/snoe/clojure-lsp/releases/latest)
- Place it in your $PATH with a chmod 755

## Nix

`clojure-lsp` is available in the [nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/misc/clojure-lsp/default.nix):

```bash
nix-shell -p clojure-lsp
```

## Arch Linux

`clojure-lsp` is
[available](https://aur.archlinux.org/packages/clojure-lsp-bin/) in the [Arch User Repository](https://aur.archlinux.org). It can be installed using your favorite [AUR](https://aur.archlinux.org) helper such as
[yay](https://github.com/Jguer/yay), [yaourt](https://github.com/archlinuxfr/yaourt), [apacman](https://github.com/oshazard/apacman) and [pacaur](https://github.com/rmarquis/pacaur). Here is an example using `yay`:

    yay -S clojure-lsp-bin

## Windows

`clojure-lsp` is available as a native executable in the [Clojure Scoop bucket](https://github.com/littleli/scoop-clojure). You need to [install Scoop](https://github.com/littleli/scoop-clojure/wiki/Getting-started#installation) if you don't have it already.

Issue following command to download the `clojure-lsp` executable.

```bash
scoop install clojure-lsp
```
