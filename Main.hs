
module Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import Data.List.Extra
import Data.Char
import System.Directory
import Installer
import Config


version :: String -> String
version = intercalate "." . filter (all isDigit) . wordsBy (`elem` "-.")


main :: IO ()
main = do
    createDirectoryIfMissing True ".build"
    setCurrentDirectory ".build"
    shakeArgs shakeOptions $ do
        want ["minghc-" ++ versionGHC ++ ".exe"]

        "cabal-*.tar.gz" %> \out -> do
            let ver = version out
            let url = "https://www.haskell.org/cabal/release/cabal-install-" ++ ver ++ "/cabal-" ++ ver ++ "-i386-unknown-mingw32.tar.gz"
            cmd "wget --no-check-certificate" url "-O" out

        "ghc-*.tar.bz2" %> \out -> do
            let ver = version out
            let url = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-i386-unknown-mingw32.tar.bz2"
            cmd "wget --no-check-certificate" url "-O" out

        ".cabal-*" %> \out -> do
            let ver = version out
            writeFile' out ""
            need ["cabal-" ++ ver ++ ".tar.gz"]
            liftIO $ ignore $ removeDirectoryRecursive $ "cabal-" ++ ver
            liftIO $ createDirectoryIfMissing True $ "cabal-" ++ ver ++ "/bin"
            cmd "tar zxfv" ["cabal-" ++ ver ++ ".tar.gz"] "-C" ["cabal-" ++ ver ++ "/bin"]

        ".ghc-*" %> \out -> do
            let ver = version out
            writeFile' out ""
            need ["ghc-" ++ ver ++ ".tar.bz2"]
            liftIO $ ignore $ removeDirectoryRecursive $ "ghc-" ++ ver
            cmd "tar xf" ["ghc-" ++ ver ++ ".tar.bz2"]

        ".msys-*" %> \out -> do
            let ver = version out
            writeFile' out ""
            liftIO $ ignore $ removeDirectoryRecursive $ "msys-" ++ ver
            cmd "unzip" ["../msys-" ++ ver ++ ".zip"]

        "minghc-*.exe" %> \out -> do
            let ver = version out
            need [out -<.> "nsi",".cabal-" ++ versionCabal,".ghc-" ++ ver,".msys-" ++ versionMSYS]
            cmd "makensis" [out -<.> "nsi"]

        "minghc-*.nsi" %> \out -> do
            writeFile' out $ installer $ version out
