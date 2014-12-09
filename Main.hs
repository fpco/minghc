
module Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import Data.List.Extra
import System.Directory
import Installer

main :: IO ()
main = shake shakeOptions $ do
    want ["ltshaskell.exe"]

    "cabal-*.tar.gz" %> \out -> do
        let ver = splitOn "-" (dropExtension $ takeBaseName out) !! 1
        let url = "https://www.haskell.org/cabal/release/cabal-install-" ++ ver ++ "/cabal-" ++ ver ++ "-i386-unknown-mingw32.tar.gz"
        cmd "wget --no-check-certificate" url "-O" out

    "ghc-*.tar.bz2" %> \out -> do
        let ver = splitOn "-" (dropExtension $ takeBaseName out) !! 1
        let url = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-i386-unknown-mingw32.tar.bz2"
        cmd "wget --no-check-certificate" url "-O" out

    ".cabal-1.20" %> \out -> do
        writeFile' out ""
        need ["cabal-1.20.0.3.tar.gz"]
        liftIO $ ignore $ removeDirectoryRecursive "cabal-1.20"
        liftIO $ createDirectoryIfMissing True "cabal-1.20/bin"
        cmd "tar zxfv cabal-1.20.0.3.tar.gz -C cabal-1.20/bin"

    ".ghc-7.8.3" %> \out -> do
        writeFile' out ""
        need ["ghc-7.8.3.tar.bz2"]
        liftIO $ ignore $ removeDirectoryRecursive "ghc-7.8.3"
        cmd "tar xf ghc-7.8.3.tar.bz2"

    ".msys-1.0" %> \out -> do
        writeFile' out ""
        liftIO $ ignore $ removeDirectoryRecursive "msys-1.0"
        cmd "unzip msys-1.0.zip"

    "ltshaskell.exe" %> \out -> do
        need ["ltshaskell.nsi",".cabal-1.20",".ghc-7.8.3",".msys-1.0"]
        cmd "makensis ltshaskell.nsi"

    "ltshaskell.nsi" %> \out -> do
        writeFile' out installer
