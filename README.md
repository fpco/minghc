# Minimum GHC Installer [![Build Status](https://img.shields.io/travis/fpco/minghc.svg?style=flat)](https://travis-ci.org/fpco/minghc)

This project provides a Windows installer with:

* [GHC](https://www.haskell.org/ghc/), so you can write Haskell code.
* [Cabal](https://www.haskell.org/cabal/), so you can install Haskell packages.
* [MSYS](http://www.mingw.org/wiki/MSYS), so packages with configure scripts (notably [network](https://hackage.haskell.org/package/network)) compile.

It _does not_ provide all the packages included with the [Haskell Platform](https://www.haskell.org/platform/), but it _does_ provide an environment where you can install those packages.


## Using the Installer

* [**Download installer with GHC 7.10.1 (32-bit)**](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.10.1-i386.exe)
* [**Download installer with GHC 7.8.4 (64-bit)**](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.8.4-x86_64.exe)
* [**Download installer with GHC 7.8.3 (32-bit)**](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.8.3.exe)

To use MinGHC, download and run the installer. There are two options you may wish to modify:

* "Add programs to PATH" - select this if you want to make this version of GHC the standard one you use for general development. It will modify your `%PATH%` environment variable so the MinGHC installed copies of `ghc` and `cabal` are used by default.
* "Add switcher to PATH" - select this if you want to use a different GHC normally, but occasionally switch to this version. After installation, type `minghc-7.8.3` at a command prompt to temporarily add the MinGHC copies of `ghc` and `cabal`.

_Caveats:_ 
* To build certain packages (e.g. [haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts)) you may need the tools [Happy](https://www.haskell.org/happy/) or [Alex](https://www.haskell.org/alex/) on your `%PATH%`. Youc an install these packages in the right place by typing `cabal install happy && cabal install alex`.
* The `network` library doesn't go good with [Cygwin](https://cygwin.com/). Hence, it is not recommended to use `cabal install` in Cygwin terminal: instead, use Command Prompt (`cmd.exe`) or Windows PowerShell.

### Older installer links

* [GHC 7.6.3 (32-bit)](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.6.3.exe)
* [GHC 7.4.2 (32-bit)](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.4.2.exe)
* [GHC 7.2.2 (32-bit)](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.2.2.exe)

## Motivation

There are two existing ways to get GHC on Windows, straight from the [GHC distribution](https://www.haskell.org/ghc/) and using the [Haskell Platform](https://www.haskell.org/platform/). The GHC distribution is hard to unpack (`.xv` files are not Windows friendly), doesn't setup the `%PATH%`, lacks Cabal and cannot build the `network` library on its own. The Haskell Platform is easy to install and comes with more libraries, but still won't build the `network` library and usually lags the GHC release by months. This installer is the GHC distribution with all the issues above fixed.

## Possible future enhancements

* Add in C libraries to ease use of Haskell packages on Windows. Pull requests welcome.

## Building the Installer

Users of the installer have no need to build it, these are mostly notes for developers of the installer. To build one of the installers:

* Download [NSIS](http://nsis.sourceforge.net/) and put it on your `%PATH%`.
* Make sure you have copies of `tar`, `wget`, `bunzip` and `gzip` on your `%PATH%`. For `tar`, a version of BSD Tar is recommended. ([GNU on Windows (GOW)](https://github.com/bmatzelle/gow) might help you here.)
* Run `cabal install`.
* Run `minghc-generate`. That will generate a file `.build/minghc-7.8.3.exe` (takes about 10 minutes).
* To build for other versions of GHC, pass the version on the command line, for example `minghc-generate 7.6.3`.
