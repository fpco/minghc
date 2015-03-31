{-# LANGUAGE FlexibleContexts #-}

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


data Flags = Quick | Flag64 deriving Eq
flags =
    [ Option "" ["quick"] (NoArg $ Right Quick) "Build without solid compression."
    , Option "" ["arch64"] (NoArg $ Right Flag64) "Use 64-bit GHC."
    ]

main :: IO ()
main = do
    createDirectoryIfMissing True ".build/bin/bin"
    withCurrentDirectory ".build" $ shakeArgsWith shakeOptions flags $ \flags ver -> return $ Just $ do
        let arch = if Flag64 `elem` flags then Arch64 else Arch32
        want ["minghc-" ++ v ++ "-" ++ showArch arch ++ ".exe" | v <- if null ver then [defaultVersion GHC] else ver]

        let wget from to = unit $ cmd "wget --no-check-certificate" [from] "-O" [to]
        "cabal-*.tar.gz" %> \out -> wget (source arch Cabal $ extractVersion out) out
        "ghc-*.tar.bz2" %> \out -> wget (source arch GHC $ extractVersion out) out
        "msys-*.zip" %> \out -> wget (source arch MSYS $ extractVersion out) out
        "alex-*.exe" %> \out -> wget (source arch Alex $ extractVersion out) out
        "happy-*.exe" %> \out -> wget (source arch Happy $ extractVersion out) out

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
                verarch = ver ++ "-" ++ showArch arch
            writeFile' out ""
            need ["ghc-" ++ verarch ++ ".tar.bz2"]
            liftIO $ ignore $ removeDirectoryRecursive $ "ghc-" ++ verarch
            liftIO $ createDirectoryIfMissing True $ "ghc-" ++ verarch
            cmd "tar xf" ["ghc-" ++ verarch ++ ".tar.bz2"] "-C" ["ghc-" ++ verarch]

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
                   ,".ghc-" ++ extractVersion out ++ "-" ++ showArch arch] ++
                   ["." ++ lower (show prog) ++ "-" ++ defaultVersion prog
                   | prog <- [minBound..maxBound], prog /= GHC]
            cmd "makensis -V3" [out -<.> "nsi"]

        "minghc-*.nsi" %> \out -> do
            need ["../Installer.hs","../Config.hs"]
            writeFile' out $ installer arch (Quick `elem` flags) $
                \prog -> if prog == GHC then extractVersion out else defaultVersion prog
