{-# LANGUAGE OverloadedStrings #-}

module Installer(installer) where

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

installer :: String
installer = nsis $ do
    name "LTSHaskell"
    outFile "LTSHaskell.exe"
    installDir "$PROGRAMFILES/LTSHaskell"
    requestExecutionLevel User

    page Directory
    page InstFiles

    let path = ["$APPDATA/cabal/bin"
               ,"$INSTDIR/ghc-7.8.3/bin"
               ,"$INSTDIR/ghc-7.8.3/mingw/bin"
               ,"$INSTDIR/cabal-1.20.0.3/bin"
               ,"$INSTDIR/msys-1.0/bin"]

    section "" [] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeUninstaller "uninstall.exe"

        file [Recursive] "ghc-7.8.3"
        file [Recursive] "cabal-1.20.0.3"
        file [Recursive] "msys-1.0"

        -- Should use HKLM instead of HKCU for all but APPDATA.
        -- However, we need to ensure that the APPDATA path comes first.
        -- And this is the only way I could make that happen.

        mapM_ (setEnvVarAppend HKCU "PATH") path

    uninstall $ do
        rmdir [Recursive] "$INSTDIR"
        mapM_ (setEnvVarRemove HKCU "PATH") path
