
module Config(
    Program(..), Version,
    defaultVersion, source
    ) where


data Program = GHC | Cabal | MSYS deriving (Eq,Show,Enum,Bounded)

type Version = String

defaultVersion :: Program -> Version
defaultVersion GHC = "7.8.3"
defaultVersion MSYS = "1.0.1"
defaultVersion Cabal = "1.20.0.3"

source :: Program -> Version -> String
source GHC ver = "https://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-i386-unknown-mingw32.tar.bz2"
source MSYS ver = "https://s3.amazonaws.com/download.fpcomplete.com/minghc/msys-" ++ ver ++ ".zip"
source Cabal ver = "https://www.haskell.org/cabal/release/cabal-install-" ++ ver ++ "/cabal-" ++ ver ++ "-i386-unknown-mingw32.tar.gz"
