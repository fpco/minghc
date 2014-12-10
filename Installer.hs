{-# LANGUAGE OverloadedStrings #-}

module Installer(installer) where

import Config
import Data.String
import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate

installer :: String -> String
installer versionGHC = nsis $ do
    _ <- constant "GHC" (fromString versionGHC :: Exp String)
    _ <- constant "CABAL" (fromString versionCabal :: Exp String)
    _ <- constant "MSYS" (fromString versionMSYS :: Exp String)

    name "MinGHC-$GHC"
    outFile "minghc-$GHC.exe"
    installDir "$PROGRAMFILES/MinGHC-$GHC"
    requestExecutionLevel User
    setCompressor LZMA [Solid]

    page Components
    page Directory
    page InstFiles

    let path =
            ["$APPDATA/cabal/bin"
            ,"$INSTDIR/ghc-$GHC/bin"
            ,"$INSTDIR/ghc-$GHC/mingw/bin"
            ,"$INSTDIR/cabal-$CABAL/bin"
            ,"$INSTDIR/msys-$MSYS/bin"]

    section "Install" [Required, Description "Install GHC, Cabal and MSYS"] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeUninstaller "uninstall.exe"

        file [Recursive] "ghc-$GHC"
        file [Recursive] "cabal-$CABAL"
        file [Recursive] "msys-$MSYS"

        createDirectory "$INSTDIR/switch"
        writeFileLines "$INSTDIR/switch/minghc-$GHC.bat" $
            ["set PATH=" & x & ";%PATH%" | x <- path] ++
            ["ghc --version"]

    section "Add programs to PATH" [Description "Put GHC, Cabal and MSYS on the %PATH%"] $ do
        -- Should use HKLM instead of HKCU for all but APPDATA.
        -- However, we need to ensure that the APPDATA path comes first.
        -- And this is the only way I could make that happen.
        mapM_ (setEnvVarPrepend HKCU "PATH") path

    section "Add switcher to PATH" [Description "Put minghc-$GHC.bat on the %PATH%, which puts the other programs on the %PATH%"] $ do
        setEnvVarPrepend HKCU "PATH" "$INSTDIR/switch"

    uninstall $ do
        rmdir [Recursive] "$INSTDIR"
        -- make sure we don't remove $APPDATA/cabal/bin, since users may have had that on their $PATH before
        mapM_ (setEnvVarRemove HKCU "PATH") $ "$INSTDIR/switch" : drop 1 path
