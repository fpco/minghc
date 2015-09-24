# Minimum GHC Installer [![Build Status](https://img.shields.io/travis/fpco/minghc.svg?style=flat)](https://travis-ci.org/fpco/minghc)

## Wait, are you sure you don't want `stack`?

For most use cases we actually recommend [you download and use
`stack`](https://github.com/commercialhaskell/stack/wiki/Downloads#windows)
instead of proceeding with MinGHC. The MinGHC approach to installing both GHC
and MSYS has been adopted by `stack` and `stack` provides some additional
benefits:

* Easier to upgrade to newer GHC versions
* Support for working with multiple GHC versions without switching environments
* Simple upgrade procedure for `stack` itself
* Isolated environment that avoids contaminating your `PATH` variable

For more information on using `stack`, please [read the `stack`
guide](https://github.com/commercialhaskell/stack/blob/master/GUIDE.md).

One note: when using stack, you need to make a few changes to how you
call some tools outside of a project:

* `stack ghc -- ...` to compile files
* `stack runghc -- ...` to interpret files
* `stack exec -- ...` to run arbitrary commands

## MinGHC

MinGHC is still a supported and active project, and makes sense for people
looking for network-free installers for a Haskell toolchain. For more
information, see [issue #75](https://github.com/fpco/minghc/issues/75).

This project provides a Windows installer with:

* [GHC](https://www.haskell.org/ghc/), so you can write Haskell code.
* [stack](https://github.com/commercialhaskell/stack#readme), a modern build tool for Haskell.
* [MSYS](http://www.mingw.org/wiki/MSYS), so packages with configure scripts (notably [network](https://hackage.haskell.org/package/network)) compile.
* [Cabal](https://www.haskell.org/cabal/), an older but still commonly used Haskell build tool.

It _does not_ provide all the packages included with the [Haskell Platform](https://www.haskell.org/platform/), but it _does_ provide an environment where you can install those packages.  Some require [installing c libraries](docs/InstallingCLibs.md).


## Using the Installer

* **[Download installer for the desired GHC version and CPU architecture from the latest Github release](https://github.com/fpco/minghc/releases/latest)**

To use MinGHC, download and run the installer. There are two options you may wish to modify:

* "Add programs to PATH" - select this if you want to make this version of GHC the standard one you use for general development. It will modify your `%PATH%` environment variable so the MinGHC installed copies of `ghc` and `cabal` are used by default.
* "Add switcher to PATH" - select this if you want to use a different GHC normally, but occasionally switch to this version. After installation, type `minghc-7.8.3` at a command prompt to temporarily add the MinGHC copies of `ghc` and `cabal`.

_Caveats:_
* The `network` library doesn't work well with [Cygwin](https://cygwin.com/). Hence, it is not recommended that you use `cabal install` in a Cygwin terminal. Use Command Prompt (`cmd.exe`) or Windows PowerShell instead.

### Older installer links

* [GHC 7.6.3 (32-bit)](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.6.3.exe)
* [GHC 7.4.2 (32-bit)](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.4.2.exe)
* [GHC 7.2.2 (32-bit)](https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghc-7.2.2.exe)

## Motivation

There are two existing ways to get GHC on Windows, straight from the [GHC distribution](https://www.haskell.org/ghc/) and using the [Haskell Platform](https://www.haskell.org/platform/). The GHC distribution is hard to unpack (`.xv` files are not Windows friendly), doesn't setup the `%PATH%`, lacks Cabal and cannot build the `network` library on its own. The Haskell Platform is easy to install and comes with more libraries, but still won't build the `network` library and usually lags the GHC release by months. This installer is the GHC distribution with all the issues above fixed.

## Building

Users of MinGHC installers do not need to build it themselves. Below are instructions for anyone who wants to contribute to future installers.

### System Dependencies

You need NSIS installed:

* Download [NSIS 3.0b2](http://nsis.sourceforge.net/), install it, and place the installation directory on your `PATH`.
* Patch NSIS with the **large strings build for 3.0b2** found among its
  [special builds](http://nsis.sourceforge.net/Special_Builds). (The patch is applied by copying
  the files in the patch archive over top the NSIS installation.)

### Building Installers

Creating installers requires a two-step process: 1) build the installer-generation script and then 2) run the script
to create installers.

  1. To build the installer-generation script, run `stack build`.
  2. To execute the installer generator, run `stack exec minghc-generate`.

By default, the installer generator will create an installer for the most recent GHC release (32-bit).
You can build installers for other official releases by providing a version number and possibly `--arch64` to use 64-bit
GHC.

For example, the following will build an older release as 64-bit.

    > stack exec minghc-generate -- 7.8.4 --arch64

The resulting installer can be found in the `.build` directory.
