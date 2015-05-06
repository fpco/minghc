
module Config(
    Arch(..), showArch,
    Program(..), Version,
    defaultVersion, source,
    extractVersion
    ) where

import Data.List.Extra
import Development.Shake.FilePath
import Data.Char

data Arch = Arch32 | Arch64

showArch :: Arch -> String
showArch Arch32 = "i386"
showArch Arch64 = "x86_64"

data Program = GHC | Minghcbin | Git | SevenZexe | SevenZdll deriving (Eq,Show,Enum,Bounded)

type Version = String

defaultVersion :: Program -> Version
-- Latest released versions of all
defaultVersion GHC = "7.10.1"
defaultVersion Git = "2.4.0.1"
defaultVersion Minghcbin = "20150506"
defaultVersion SevenZexe = "XXX"
defaultVersion SevenZdll = "XXX"

source :: Arch -> Program -> Version -> String
-- Official GHC release, available in xv and bz2, but the xv one is harder to extract on Windows systems
source arch GHC ver = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-" ++ showArch arch ++ ".7z"

source _ Minghcbin ver = "https://s3.amazonaws.com/download.fpcomplete.com/minghc/minghcbin-" ++ ver ++ ".7z"
source _ Git ver = "https://s3.amazonaws.com/download.fpcomplete.com/minghc/PortableGit-" ++ ver ++ ".7z.exe"
source _ SevenZexe _ = "https://s3.amazonaws.com/download.fpcomplete.com/minghc/7z.exe"
source _ SevenZdll _ = "https://s3.amazonaws.com/download.fpcomplete.com/minghc/7z.dll"


-- | Given a filename containing a version-like bit, extract the version
extractVersion :: String -> Version
extractVersion = intercalate "." . takeWhile f . dropWhile (not . f) . wordsBy (`elem` "-.") . takeFileName
    where f = all isDigit
