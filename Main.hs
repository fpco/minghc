
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
    createDirectoryIfMissing True ".build/bin/bin"
    withCurrentDirectory ".build" $ shakeArgsWith shakeOptions flags $ \flags ver -> return $ Just $ do
        want ["minghc-" ++ v ++ ".exe" | v <- if null ver then [defaultVersion GHC] else ver]

        let wget from to = unit $ cmd "wget --no-check-certificate" [from] "-O" [to]
        "cabal-*.tar.gz" %> \out -> wget (source Cabal $ extractVersion out) out
        "ghc-*.tar.bz2" %> \out -> wget (source GHC $ extractVersion out) out
        "msys-*.zip" %> \out -> wget (source MSYS $ extractVersion out) out
        "alex-*.exe" %> \out -> wget (source Alex $ extractVersion out) out
        "happy-*.exe" %> \out -> wget (source Happy $ extractVersion out) out

        ".cabal-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["cabal-" ++ ver ++ ".tar.gz"]
            unit $ cmd "tar zxfv" ["cabal-" ++ ver ++ ".tar.gz"] "-C" ["bin/bin"]
            needed ["bin/bin/cabal.exe"] -- make sure that we check we have this version in PATH

        ".alex-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            copyFile' ("alex-" ++ ver ++ ".exe") "bin/bin/alex.exe"
            needed ["bin/bin/alex.exe"]

        ".happy-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            copyFile' ("happy-" ++ ver ++ ".exe") "bin/bin/happy.exe"
            needed ["bin/bin/happy.exe"]

        ".ghc-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["ghc-" ++ ver ++ ".tar.bz2"]
            liftIO $ ignore $ removeDirectoryRecursive $ "ghc-" ++ ver
            liftIO $ createDirectoryIfMissing True $ "ghc-" ++ ver
            cmd "tar xf" ["ghc-" ++ ver ++ ".tar.bz2"] "-C" ["ghc-" ++ ver]

        ".msys-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["msys-" ++ ver ++ ".zip"]
            liftIO $ ignore $ removeDirectoryRecursive $ "msys-" ++ ver
            liftIO $ createDirectoryIfMissing True $ "msys-" ++ ver
            cmd "unzip" ["msys-" ++ ver ++ ".zip"] "-d" ["msys-" ++ ver]

        "minghc-*.exe" %> \out -> do
            let ver = extractVersion out
            need ["../Config.hs"]
            need $ [out -<.> "nsi"
                   ,".ghc-" ++ extractVersion out] ++
                   ["." ++ lower (show prog) ++ "-" ++ defaultVersion prog
                   | prog <- [minBound..maxBound], prog /= GHC]
            cmd "makensis -V3" [out -<.> "nsi"]

        "minghc-*.nsi" %> \out -> do
            need ["../Installer.hs","../Config.hs"]
            writeFile' out $ installer (Quick `elem` flags) $
                \prog -> if prog == GHC then extractVersion out else defaultVersion prog
