
module Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import System.Console.GetOpt
import Control.Monad.Extra
import Data.List.Extra
import Data.Char
import System.Directory.Extra
import Installer
import Config


data Flags = Quick deriving Eq
flags = [Option "" ["quick"] (NoArg $ Right Quick) "Build without solid compression."]

main :: IO ()
main = do
    createDirectoryIfMissing True ".build"
    withCurrentDirectory ".build" $ shakeArgsWith shakeOptions flags $ \flags ver -> return $ Just $ do
        want ["minghc-" ++ last (defaultVersion GHC : ver) ++ ".exe"]

        let wget from to = unit $ cmd "wget --no-check-certificate" [from] "-O" [to]
        "cabal-*.tar.gz" %> \out -> wget (source Cabal $ extractVersion out) out
        "ghc-*.tar.bz2" %> \out -> wget (source GHC $ extractVersion out) out
        "msys-*.zip" %> \out -> wget (source MSYS $ extractVersion out) out

        ".cabal-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["cabal-" ++ ver ++ ".tar.gz"]
            liftIO $ createDirectoryIfMissing True "bin"
            unit $ cmd "tar zxfv" ["cabal-" ++ ver ++ ".tar.gz"] "-C" ["bin"]
            needed ["bin/cabal.exe"] -- make sure that we check we have this version in PATH

        ".ghc-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["ghc-" ++ ver ++ ".tar.bz2"]
            liftIO $ ignore $ removeDirectoryRecursive $ "ghc-" ++ ver
            cmd "tar xf" ["ghc-" ++ ver ++ ".tar.bz2"]

        ".msys-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["msys-" ++ ver ++ ".zip"]
            liftIO $ ignore $ removeDirectoryRecursive $ "msys-" ++ ver
            cmd "unzip" ["msys-" ++ ver ++ ".zip"]

        "minghc-*.exe" %> \out -> do
            let ver = extractVersion out
            need $ [out -<.> "nsi"
                   ,".ghc-" ++ extractVersion out] ++
                   ["." ++ lower (show prog) ++ "-" ++ defaultVersion prog
                   | prog <- [minBound..maxBound], prog /= GHC]
            cmd "makensis -V3" [out -<.> "nsi"]

        "minghc-*.nsi" %> \out -> do
            need ["../Installer.hs"]
            writeFile' out $ installer (Quick `elem` flags) $
                \prog -> if prog == GHC then extractVersion out else defaultVersion prog
