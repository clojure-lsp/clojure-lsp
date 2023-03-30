# Installation

Below are all available `clojure-lsp` installation methods, after installed, follow the documentation for your editor's language client. See [Clients](clients.md).

## Homebrew (MacOS and Linux)

We have a custom tap using the native compiled binaries for users that use homebrew:

```bash
brew remove clojure-lsp # if you have old clojure-lsp installed via brew
brew install clojure-lsp/brew/clojure-lsp-native
```

## Script

Stable release:

```bash
sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)
```

nightly build:

```bash
curl -O https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install
chmod a+x install
sudo ./install --version nightly
```

## Nix

`clojure-lsp` is available in the [nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/misc/clojure-lsp/default.nix):

```bash
nix-shell -p clojure-lsp
```

or if using flakes:

```bash
nix run github:clojure-lsp/clojure-lsp
# or
nix shell github:clojure-lsp/clojure-lsp
```

or consume the flake overlay:

```nix
inputs.clojure-lsp.url = "github:clojure-lsp/clojure-lsp";
nixpkgs.overlays = [ inputs.clojure-lsp.overlays.default ];
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

## Native binary (recommended)

`clojure-lsp` builds using GraalVM a native executable for each OS: Linux, MacOS and Windows.

### Manually

The binaries are available on [Github releases](https://github.com/clojure-lsp/clojure-lsp/releases) as `clojure-lsp-native-<os>-<arch>.zip`, after downloading you just need to unzip it.

## Embedded jar (Legacy executable)

In Github releases you will find a `clojure-lsp` file that runs a embedded jar. This should be removed soon, use GraalVM binaries instead.

- You need `java` on your $PATH.
- Grab the latest `clojure-lsp` from github [LATEST](https://github.com/clojure-lsp/clojure-lsp/releases/latest)
- Place it in your $PATH with a chmod 755
