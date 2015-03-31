{-# LANGUAGE OverloadedStrings #-}

module Installer(installer) where

import Config
import Control.Monad
import Data.String
import Data.List.Extra
import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate


installer :: Arch -> Bool -> (Program -> Version) -> String
installer arch quick version = nsis $ do
    forM_ [minBound..maxBound] $ \prog ->
        constant (upper $ show prog) (fromString $ version prog :: Exp String)
    constant "ARCH"      (fromString $ showArch arch     :: Exp String)
    constant "ARCH_ABBR" (fromString $ showArchAbbr arch :: Exp String)

    name "MinGHC-$GHC-$ARCH"
    outFile "minghc-$GHC-$ARCH.exe"
    installDir $ case arch of
        Arch32 -> "$PROGRAMFILES/MinGHC-$GHC"
        Arch64 -> "$PROGRAMFILES64/MinGHC-$GHC"
    requestExecutionLevel Highest
    unless quick $ setCompressor LZMA [Solid]

    page Components
    page Directory
    page InstFiles
    unpage Confirm
    unpage InstFiles

    -- important that $APPDATA/cabal/bin is first because we prepend to the PATH
    -- meaning it ends up being on the PATH lower-priority than our stuff,
    -- since the user may have their own old version of cabal in $INSTDIR
    let path =
            ["$APPDATA/cabal/bin"
            ,"$INSTDIR/bin"
            ,"$INSTDIR/ghc-$GHC/bin"
            ,"$INSTDIR/ghc-$GHC/mingw/bin"
            ,"$INSTDIR/msys-$MSYS/bin"]

    section "Install" [Required, Description "Install GHC, Cabal and MSYS"] $ do
        setOutPath "$INSTDIR"
        writeUninstaller "uninstall.exe"

        file [Recursive] "bin/*"
        file [Recursive] "ghc-$GHC-$ARCH/*"
        file [Recursive] "msys-$MSYS/*"

        createDirectory "$INSTDIR/switch"

        let scriptAliases = ["$INSTDIR/switch/minghc-$GHC.bat"
                            ,"$INSTDIR/switch/minghc-$GHC-$ARCH_ABBR.bat"]

        forM_ scriptAliases $ flip writeFileLines $
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
        mapM_ (setEnvVarRemove HKCU "PATH") $ "$INSTDIR/switch" : tail path


showArchAbbr :: Arch -> String
showArchAbbr Arch32 = "32"
showArchAbbr Arch64 = "64"
