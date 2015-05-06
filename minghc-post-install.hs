-- Note: we should only depend on libraries that ship with GHC for this. No
-- external dependencies!
import System.Environment (getExecutablePath)
import System.FilePath
import System.Directory
import Control.Monad
import Data.Version
import System.Process
import Text.ParserCombinators.ReadP
import System.IO
import System.Exit

main :: IO ()
main = do
    binPath <- fmap takeDirectory getExecutablePath
    let sevenz = binPath </> "7z.exe"
        un7zDir dir =
            getDirectoryContents dir >>= mapM_ (un7z binPath sevenz)
    un7zDir binPath
    un7zDir $ takeDirectory binPath
    removeOldCabal (binPath </> "cabal.exe")

un7z :: FilePath -- ^ bin path
     -> FilePath -- ^ 7z.exe
     -> FilePath -- ^ to be unpacked
     -> IO ()
un7z binPath sevenz =
    go . (binPath </>)
  where
    go fp = when toUn7z $ do
        putStrLn $ "Decompressing " ++ fp
        ec <- rawSystem sevenz
            [ "x"
            , "-o" ++ binPath
            , "-y"
            , fp
            ]
        removeFile fp
        when (ec /= ExitSuccess)
            $ error $ "Could not decompress: " ++ fp
        go base
      where
        (base, ext) = splitExtension fp

        toUn7z = ext `elem`
            [ ".7z"
            , ".tar"
            , ".xz"
            ]

removeOldCabal :: FilePath -- ^ new cabal
               -> IO ()
removeOldCabal newCabal = do
    cabalDir <- getAppUserDataDirectory "cabal"
    let oldCabal = cabalDir </> "bin/cabal.exe"
    exists <- doesFileExist oldCabal
    when exists $ do
        oldVersion <- getCabalVersion oldCabal
        newVersion <- getCabalVersion newCabal
        when (oldVersion < newVersion) $ do
            putStrLn "You have an older version of cabal-install at:"
            putStrLn oldCabal
            putStr "It is recommended that you remove it. Shall I do that for you now? (y/n) "
            hFlush stdout
            let loop = do
                    s <- getLine
                    case s of
                        "y" -> return True
                        "n" -> return False
                        _ -> do
                            putStr "Invalid response, please enter y or n: "
                            hFlush stdout
                            loop
            toDelete <- loop
            when toDelete $ removeFile oldCabal

getCabalVersion :: FilePath -> IO Version
getCabalVersion fp = do
    str <- fmap (unwords . lines) $ readProcess fp ["--numeric-version"] ""
    case filter (null . snd) $ readP_to_S parseVersion str of
        [(v, "")] -> return v
        _ -> error $ "Incorrect version: " ++ show (fp, str)
