
module Config(
    Arch(..), showArch,
    Program(..), Version,
    defaultVersion, source, dest,
    extractVersion
    ) where

import Data.List.Extra
import Development.Shake.FilePath
import Data.Char

data Arch = Arch32 | Arch64

showArch :: Arch -> String
showArch Arch32 = "i386"
showArch Arch64 = "x86_64"

data Program = GHC | Git | Stack deriving (Eq,Show,Enum,Bounded)

type Version = String

defaultVersion :: Program -> Version
-- Latest released versions of all
defaultVersion GHC = "7.10.2"
defaultVersion Git = "2.4.5.1"
defaultVersion Stack = "0.1.4.0"

source :: Arch -> Program -> Version -> String
-- Official GHC release, available in xv and bz2, but the xv one is harder to extract on Windows systems
source arch GHC ver = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-" ++ showArch arch ++ "-unknown-mingw32.tar.xz"
source _ Git "2.4.5.1" = "https://github.com/git-for-windows/git/releases/download/v2.4.5.windows.1/PortableGit-2.4.5.1-4th-release-candidate-32-bit.7z.exe"
source arch Stack ver = concat
    [ "https://github.com/commercialhaskell/stack/releases/download/v"
    , ver
    , "/stack-"
    , ver
    , "-"
    , showArch arch
    , "-windows.zip"
    ]

dest :: Version
     -> Arch
     -> Program
     -> FilePath
dest ver arch GHC = concat
    [ "ghc-"
    , ver
    , "-"
    , showArch arch
    , ".tar.xz"
    ]
dest ver _arch Git = concat
    [ "git-"
    , ver
    , ".7z"
    ]
dest ver arch Stack = concat
    [ "bin/bin/stack-"
    , ver
    , "-"
    , showArch arch
    , ".zip"
    ]

-- | Given a filename containing a version-like bit, extract the version
extractVersion :: String -> Version
extractVersion = intercalate "." . takeWhile f . dropWhile (not . f) . wordsBy (`elem` "-.") . takeFileName
    where f = all isDigit
