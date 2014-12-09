
module Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import System.Directory
import Data.List.Extra
import System.Directory
import Installer
import Config

main :: IO ()
main = do
    createDirectoryIfMissing True ".build"
    setCurrentDirectory ".build"
    shakeArgs shakeOptions $ do
        want ["minghc-" ++ versionGHC ++ ".exe"]

        "cabal-*.tar.gz" %> \out -> do
            let ver = splitOn "-" (dropExtension $ takeBaseName out) !! 1
            let url = "https://www.haskell.org/cabal/release/cabal-install-" ++ ver ++ "/cabal-" ++ ver ++ "-i386-unknown-mingw32.tar.gz"
            cmd "wget --no-check-certificate" url "-O" out

        "ghc-*.tar.bz2" %> \out -> do
            let ver = splitOn "-" (dropExtension $ takeBaseName out) !! 1
            let url = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-i386-unknown-mingw32.tar.bz2"
            cmd "wget --no-check-certificate" url "-O" out

        ".cabal-*" %> \out -> do
            let ver = splitOn "-" out !! 1
            writeFile' out ""
            need ["cabal-" ++ ver ++ ".tar.gz"]
            liftIO $ ignore $ removeDirectoryRecursive $ "cabal-" ++ ver
            liftIO $ createDirectoryIfMissing True $ "cabal-" ++ ver ++ "/bin"
            cmd "tar zxfv" ["cabal-" ++ ver ++ ".tar.gz"] "-C" ["cabal-" ++ ver ++ "/bin"]

        ".ghc-*" %> \out -> do
            let ver = splitOn "-" out !! 1
            writeFile' out ""
            need ["ghc-" ++ ver ++ ".tar.bz2"]
            liftIO $ ignore $ removeDirectoryRecursive $ "ghc-" ++ ver
            cmd "tar xf" ["ghc-" ++ ver ++ ".tar.bz2"]

        ".msys-*" %> \out -> do
            let ver = splitOn "-" out !! 1
            writeFile' out ""
            liftIO $ ignore $ removeDirectoryRecursive $ "msys-" ++ ver
            cmd "unzip" ["../msys-" ++ ver ++ ".zip"]

        ("minghc-" ++ versionGHC ++ ".exe") %> \out -> do
            need ["ltshaskell.nsi",".cabal-" ++ versionCabal,".ghc-" ++ versionGHC,".msys-" ++ versionMSYS]
            cmd "makensis" [out -<.> "nsi"]

        ("minghc-" ++ versionGHC ++ ".nsi") %> \out -> do
            writeFile' out installer
