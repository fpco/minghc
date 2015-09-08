{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import Development.Shake
import Development.Shake.FilePath
import Control.Exception.Extra
import System.Console.GetOpt
import Data.List.Extra
import Data.Maybe
import System.Directory (getCurrentDirectory)
import System.Directory.Extra as D
import Installer
import Config
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO
import qualified Data.ByteString as S
import Control.Monad

data Flags = Flag64
           | FlagStack (Maybe String)
 deriving Eq
flags =
    [ Option "" ["arch64"] (NoArg $ Right Flag64) "Use 64-bit GHC."
    , Option "" ["stack"] (OptArg (Right . FlagStack) "VERSION") "Specify stack version."
    ]

replaceDir :: FilePath -> FilePath -> IO ()
replaceDir src dest = do
    exists <- D.doesDirectoryExist dest
    when exists $ D.removeDirectoryRecursive dest
    createDirectoryIfMissing True dest

    contents <- D.getDirectoryContents src
    forM_ contents $ \base -> do
        let fp = src </> base
        exists' <- D.doesFileExist fp
        when exists' $ copyFile fp (dest </> base)

main :: IO ()
main = do
    man <- newManager tlsManagerSettings
    projRoot <- getCurrentDirectory
    replaceDir "bin" ".build/bin/bin"

    withCurrentDirectory ".build" $ shakeArgsWith shakeOptions flags $ \flags ver -> return $ Just $ do
        let arch = if Flag64 `elem` flags then Arch64 else Arch32
            stackVer = fromMaybe (defaultVersion Stack) $ listToMaybe
                [ver | FlagStack (Just ver) <- flags]
        want ["minghc-" ++ v ++ "-" ++ showArch arch ++ ".exe" | v <- if null ver then [defaultVersion GHC] else ver]

        let fetch from to = liftIO $ do
                putStrLn $ "Downloading " ++ from
                let tmp = to <.> "tmp"
                req <- parseUrl from
                withResponse req man $
                    \res -> withBinaryFile tmp WriteMode $
                    \h -> do
                        let loop = do
                                bs <- brRead $ responseBody res
                                unless (S.null bs) $ do
                                    S.hPut h bs
                                    loop
                        loop
                renameFile tmp to
        "ghc-*.tar.xz" %> \out -> fetch (source arch GHC $ extractVersion out) out
        "git-*.7z" %> \out -> fetch (source arch Git $ extractVersion out) out
        "bin/bin/stack-*.zip" %> \out -> fetch (source arch Stack $ extractVersion out) out

        ".ghc-*" %> \out -> do
            let ver = extractVersion out
                verarch = ver ++ "-" ++ showArch arch
            writeFile' out ""
            need ["ghc-" ++ verarch ++ ".7z"]

        ".git-*" %> \out -> do
            let ver = extractVersion out
            writeFile' out ""
            need ["PortableGit-" ++ ver ++ ".7z.exe"]

        ".stack-*" %> \out -> do
            let ver = extractVersion out
                verarch = ver ++ "-" ++ showArch arch
            writeFile' out ""
            need ["bin/bin/stack-" ++ verarch ++ ".zip"]

        let getVer ghcVer GHC = ghcVer
            getVer _ Git = defaultVersion Git
            getVer _ Stack = stackVer

        "minghc-*.exe" %> \out -> do
            let ghcVer = extractVersion out
            need ["../Config.hs", "../Main.hs", out -<.> "nsi"]
            need $ map (\p -> dest (getVer ghcVer p) arch p) [minBound..maxBound]
            cmd "makensis -V3" [out -<.> "nsi"]

        "minghc-*.nsi" %> \out -> do
            need ["../Installer.hs","../Config.hs"]
            writeFile' out $ installer projRoot arch $ getVer $ extractVersion out
