minghc
======

Windows installer for GHC including msys.

Motivation: a number of Haskell packages, and in particular the network
package, require some shell script to run autotools-based configuration. As a
result, it can be difficult to install new versions of these packages on
Windows. This installer bundles msys together with GHC and cabal-install to
give a minimal Windows installer that should remove as many pain points as
possible.
