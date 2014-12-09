{-# LANGUAGE OverloadedStrings #-}

module Installer(installer) where

import Config
import Data.String
import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

installer :: String -> String
installer versionGHC = nsis $ do
    name $ fromString $ "MinGHC-" ++ versionGHC
    outFile $ fromString $ "minghc-" ++ versionGHC ++ ".exe"
    installDir $ fromString $ "$PROGRAMFILES/MinGHC-" ++ versionGHC
    requestExecutionLevel User

    page Directory
    page InstFiles

    let path = map fromString
            ["$APPDATA/cabal/bin"
            ,"$INSTDIR/ghc-" ++ versionGHC ++ "/bin"
            ,"$INSTDIR/ghc-" ++ versionGHC ++ "/mingw/bin"
            ,"$INSTDIR/cabal-" ++ versionCabal ++ "/bin"
            ,"$INSTDIR/msys-" ++ versionMSYS ++ "/bin"]

    section "" [] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeUninstaller "uninstall.exe"

        file [Recursive] $ fromString $ "ghc-" ++ versionGHC
        file [Recursive] $ fromString $ "cabal-" ++ versionCabal
        file [Recursive] $ fromString $ "msys-" ++ versionMSYS

        -- Should use HKLM instead of HKCU for all but APPDATA.
        -- However, we need to ensure that the APPDATA path comes first.
        -- And this is the only way I could make that happen.

        mapM_ (setEnvVarAppend HKCU "PATH") path

    uninstall $ do
        rmdir [Recursive] "$INSTDIR"
        mapM_ (setEnvVarRemove HKCU "PATH") path
