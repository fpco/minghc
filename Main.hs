{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import System.Console.GetOpt
import Data.List.Extra
import System.Directory.Extra
import Installer
import Config
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO
import qualified Data.ByteString as S
import Control.Monad

data Flags = Flag64 deriving Eq
flags =
    [ Option "" ["arch64"] (NoArg $ Right Flag64) "Use 64-bit GHC."
    ]

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    createDirectoryIfMissing True ".build/bin/bin"
    withCurrentDirectory ".build" $ shakeArgsWith shakeOptions flags $ \flags ver -> return $ Just $ do
        let arch = if Flag64 `elem` flags then Arch64 else Arch32
        want ["minghc-" ++ v ++ "-" ++ showArch arch ++ ".exe" | v <- if null ver then [defaultVersion GHC] else ver]

        let fetch from to = liftIO $ do
                req <- parseUrl from
                withResponse req man $
                    \res -> withBinaryFile to WriteMode $
                    \h -> do
                        let loop = do
                                bs <- brRead $ responseBody res
                                unless (S.null bs) $ do
                                    S.hPut h bs
                                    loop
                        loop
        "ghc-*.7z" %> \out -> fetch (source arch GHC $ extractVersion out) out
        "minghcbin-*.7z" %> \out -> fetch (source arch Minghcbin $ extractVersion out) out
        "PortableGit-*.7z.exe" %> \out -> fetch (source arch Git $ extractVersion out) out
        "7z.exe" %> \out -> fetch (source arch SevenZexe $ extractVersion out) out
        "7z.dll" %> \out -> fetch (source arch SevenZdll $ extractVersion out) out

        ".sevenzexe-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["7z.exe"]

            liftIO $ do
                createDirectoryIfMissing True "bin/bin"
                copyFile "7z.exe" "bin/bin/7z.exe"

        ".sevenzdll-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["7z.dll"]

            liftIO $ do
                createDirectoryIfMissing True "bin/bin"
                copyFile "7z.dll" "bin/bin/7z.dll"

        ".minghcbin-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["minghcbin-" ++ ver ++ ".7z"]

        ".ghc-*" %> \out -> do
            let ver = extractVersion out
                verarch = ver ++ "-" ++ showArch arch
            writeFile' out ""
            need ["ghc-" ++ verarch ++ ".7z"]

        ".git-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["PortableGit-" ++ ver ++ ".7z.exe"]

        "minghc-*.exe" %> \out -> do
            need ["../Config.hs"]
            need $ [out -<.> "nsi"
                   ,".ghc-" ++ extractVersion out ++ "-" ++ showArch arch] ++
                   ["." ++ lower (show prog) ++ "-" ++ defaultVersion prog
                   | prog <- [minBound..maxBound], prog /= GHC]
            cmd "makensis -V3" [out -<.> "nsi"]

        "minghc-*.nsi" %> \out -> do
            need ["../Installer.hs","../Config.hs"]
            writeFile' out $ installer arch $
                \prog -> if prog == GHC then extractVersion out else defaultVersion prog
