
module Main(main) where

import Development.Shake
import Installer

main :: IO ()
main = shake shakeOptions $ do
    want ["ltshaskell.exe"]

    ".cabal-1.20" %> \out -> do
        () <- cmd "wget --no-check-certificate https://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-mingw32.tar.gz"
        () <- cmd "rm -rf cabal-1.20"
        () <- cmd "tar zxfv cabal-1.20.0.3-i386-unknown-mingw32.tar.gz"
        () <- cmd "mkdir -p cabal-1.20/bin"
        () <- cmd "mv cabal.exe cabal-1.20/bin"
        writeFile' out ""

    ".ghc-7.8.3" %> \out -> do
        () <- cmd "wget --no-check-certificate https://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-i386-unknown-mingw32.tar.xz"
        () <- cmd "rm -rf ghc-7.8.3"
        () <- cmd "tar xf ghc-7.8.3-i386-unknown-mingw32.tar.xz"
        writeFile' out ""

    ".msys-1.0" %> \out -> do
        () <- cmd "rm -rf msys-1.0"
        () <- cmd "unzip msys-1.0.zip"
        writeFile' out ""

    "ltshaskell.exe" %> \out -> do
        need ["ltshaskell.nsi",".cabal-1.20",".ghc-7.8.3",".msys-1.0"]
        cmd "makensis ltshaskell.nsi"

    "ltshaskell.nsi" %> \out -> do
        writeFile' out installer
