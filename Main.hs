{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

main :: IO ()
main = writeFile "ltshaskell.nsi" $ nsis $ do
    name "LTSHaskell"
    outFile "LTSHaskell.exe"
    installDir "$PROGRAMFILES/LTSHaskell"
    requestExecutionLevel User

    page Directory
    page InstFiles

    section "" [] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeUninstaller "uninstall.exe"

        file [Recursive] "ghc-7.8.3"
        file [Recursive] "cabal-1.20"
        file [Recursive] "msys-1.0"

        -- Should use HKLM instead of HKCU for all but APPDATA.
        -- However, we need to ensure that the APPDATA path comes first.
        -- And this is the only way I could make that happen.

        setEnvVarAppend HKCU "PATH" "$APPDATA/cabal/bin"
        setEnvVarAppend HKCU "PATH" "$INSTDIR/ghc-7.8.3/bin"
        setEnvVarAppend HKCU "PATH" "$INSTDIR/ghc-7.8.3/mingw/bin"
        setEnvVarAppend HKCU "PATH" "$INSTDIR/cabal-1.20/bin"
        setEnvVarAppend HKCU "PATH" "$INSTDIR/msys-1.0/bin"

    uninstall $ do
        rmdir [Recursive] "$INSTDIR"
        setEnvVarRemove HKCU "PATH" "$INSTDIR/msys-1.0/bin"
        setEnvVarRemove HKCU "PATH" "$INSTDIR/cabal-1.20/bin"
        setEnvVarRemove HKCU "PATH" "$INSTDIR/ghc-7.8.3/mingw/bin"
        setEnvVarRemove HKCU "PATH" "$INSTDIR/ghc-7.8.3/bin"
        setEnvVarRemove HKCU "PATH" "$APPDATA/cabal/bin"
