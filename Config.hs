
module Config(
    Program(..), Version,
    defaultVersion, source,
    extractVersion
    ) where

import Data.List.Extra
import Development.Shake.FilePath
import Data.Char


data Program = GHC | Cabal | MSYS deriving (Eq,Show,Enum,Bounded)

type Version = String

defaultVersion :: Program -> Version
-- Latest released versions of all
defaultVersion GHC = "7.8.3"
defaultVersion MSYS = "1.0.1"
defaultVersion Cabal = "1.20.0.3"

source :: Program -> Version -> String
-- Official GHC release, available in xv and bz2, but the xv one is harder to extract on Windows systems
source GHC ver = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-i386-unknown-mingw32.tar.bz2"
-- Official Cabal release as a binary snapshot
source Cabal ver = "https://www.haskell.org/cabal/release/cabal-install-" ++ ver ++ "/cabal-" ++ ver ++ "-i386-unknown-mingw32.tar.gz"
-- A mirror, from where???
source MSYS ver = "https://s3.amazonaws.com/download.fpcomplete.com/minghc/msys-" ++ ver ++ ".zip"


-- | Given a filename containing a version-like bit, extract the version
extractVersion :: String -> Version
extractVersion = intercalate "." . takeWhile f . dropWhile (not . f) . wordsBy (`elem` "-.") . takeFileName
    where f = all isDigit
