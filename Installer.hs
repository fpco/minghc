{-# LANGUAGE OverloadedStrings #-}

module Installer(installer) where

import Config
import Control.Monad
import Data.String
import Data.List.Extra
import Development.NSIS as NSIS
import Development.NSIS.Plugins.EnvVarUpdate


installer :: FilePath -> Arch -> (Program -> Version) -> String
installer projRoot arch version = nsis $ do
    forM_ [minBound..maxBound] $ \prog ->
        constantStr (upper $ show prog) (fromString $ version prog)
    constantStr "ARCH"    (fromString $ showArch arch)
    constantStr "APPNAME" "MinGHC-$GHC-$ARCH"

    name "$APPNAME"
    outFile "minghc-$GHC-$ARCH.exe"
    -- See: http://stackoverflow.com/questions/1831810/is-appdata-now-the-correct-place-to-install-user-specific-apps-which-modify-t/1845459#1845459
    installDir "$LOCALAPPDATA/Programs/minghc-$GHC-$ARCH"

    installIcon   $ fromString projRoot & "/ico/minghc-install.ico"
    uninstallIcon $ fromString projRoot & "/ico/minghc-uninstall.ico"

    page Components
    page Directory
    page InstFiles
    unpage Confirm
    unpage InstFiles

                         -- Precedence: low to high
    let pathAddRemove    = ["$INSTDIR/bin"
                           ,"$INSTDIR/ghc-$GHC/bin"
                           ,"$INSTDIR/ghc-$GHC/mingw/bin"
                           ,"$INSTDIR/git-$GIT/usr/bin"
                           ,"$INSTDIR/git-$GIT/cmd"
                           ]
    let pathPreExisting  = ["$APPDATA/cabal/bin"]

    -- (potentially) preexisting paths should precede (i.e. have lower precedence than) new paths
    let path             = pathPreExisting ++ pathAddRemove

    let uninstallRegKey = (HKEY_CURRENT_USER, "Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\$APPNAME")

    section "Install" [Required, Description "Install GHC, Cabal, and PortableGit"] $ do
        setOutPath "$INSTDIR"
        buildUninstaller uninstallRegKey

        let rootArchives = map (dest "$GHC" arch) [minBound..maxBound]
        mapM_ (file [] . fromString) rootArchives
        file [Recursive] "bin/*"

        execWait "\"$INSTDIR/bin/7z.exe\" x -y \"-o$INSTDIR/bin\" \"$INSTDIR/bin/minghc-post-install.exe.7z\""
        NSIS.delete [] "$INSTDIR/bin/minghc-post-install.exe.7z"
        let quote :: String -> String
            quote x = concat ["\"", x, "\""]
        execWait $ fromString $ unwords $ map quote
            $ "$INSTDIR/bin/minghc-post-install.exe"
            : rootArchives
        NSIS.delete [] "$INSTDIR/bin/minghc-post-install.exe"

        createDirectory "$INSTDIR/switch"

        let switcherAliases = [fromString ("$INSTDIR/switch/minghc" ++ x ++ ".bat") | x <- switcherNameSuffixes]
        forM_ switcherAliases $ flip writeFileLines $
            ["set PATH=" & x & ";%PATH%" | x <- path] ++
            ["ghc --version"]

    section "Add programs to PATH" [Description "Put GHC, Cabal, and PortableGit on the %PATH%"] $ do
        mapM_ (setEnvVarPrepend HKCU "PATH") path

    section "Add switcher to PATH" [Description "Put minghc-$GHC.bat on the %PATH%, which puts the other programs on the %PATH%"] $ do
        setEnvVarPrepend HKCU "PATH" "$INSTDIR/switch"

    uninstall $ do
        rmdir [Recursive] "$INSTDIR"
        mapM_ (setEnvVarRemove HKCU "PATH") $ "$INSTDIR/switch" : pathAddRemove

        uncurry deleteRegKey uninstallRegKey

    where
        switcherNameSuffixes
            = "" -- no suffix
            : map (concatMap ('-':))
                [[showArchAbbr arch]
                ,[version GHC]
                ,[version GHC, showArchAbbr arch]
                ,[majorVersion (version GHC)]
                ,[majorVersion (version GHC), showArchAbbr arch]]

        buildUninstaller uninstallRegKey = do
            let uninstaller = "uninstall.exe"
            writeUninstaller uninstaller

            let setUninstallStr   = uncurry writeRegStr   uninstallRegKey
            let setUninstallDWORD = uncurry writeRegDWORD uninstallRegKey

            -- See possible settings here: http://nsis.sourceforge.net/Add_uninstall_information_to_Add/Remove_Programs
            setUninstallStr   "DisplayName"     "$APPNAME"
            setUninstallStr   "UninstallString" ("$INSTDIR/" & uninstaller)
            setUninstallStr   "DisplayIcon"     ("$INSTDIR/" & uninstaller)
            setUninstallStr   "InstallLocation" "$INSTDIR"
            setUninstallStr   "Readme"          "https://github.com/fpco/minghc/blob/master/README.md"
            setUninstallStr   "DisplayVersion"  (fromString $ version GHC)
            setUninstallDWORD "NoModify"        1
            setUninstallDWORD "NoRepair"        1


showArchAbbr :: Arch -> String
showArchAbbr Arch32 = "32"
showArchAbbr Arch64 = "64"

majorVersion :: Version -> Version
majorVersion ver = intercalate "." (take 2 parts)
    where parts = wordsBy (== '.') ver
