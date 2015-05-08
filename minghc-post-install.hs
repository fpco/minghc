-- Note: we should only depend on libraries that ship with GHC for this. No
-- external dependencies!
import           Control.Monad                (when)
import           Data.Version                 (Version, parseVersion)
import           Prelude                      (Bool (..), FilePath, elem, error,
                                               filter, fmap, getLine, lines,
                                               mapM_, null, putStr, putStrLn,
                                               return, show, snd, unwords, ($),
                                               (++), (.), (/=), (<), (==),
                                               (>>=))
import           System.Directory             (doesFileExist,
                                               getAppUserDataDirectory,
                                               getDirectoryContents, removeFile)
import           System.Environment           (getArgs, getExecutablePath)
import           System.Exit                  (ExitCode (ExitSuccess))
import           System.FilePath              (splitExtension, takeDirectory,
                                               takeExtension, (</>))
import           System.IO                    (IO, hFlush, stdout)
import           System.Process               (rawSystem, readProcess)
import           Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    binPath <- fmap takeDirectory getExecutablePath
    let sevenz = binPath </> "7z.exe"
    getDirectoryContents binPath
        >>= mapM_ (un7z binPath sevenz . (binPath </>))

    mapM_ (handleArg sevenz) args
    removeOldCabal (binPath </> "cabal.exe")

handleArg
    :: FilePath -- ^ 7z.exe
    -> FilePath -- ^ command line argument
    -> IO ()
handleArg sevenz arg = do
    putStrLn $ show (sevenz, arg, base, ext)
    case ext of
        ".7z" -> un7z base sevenz arg
        ".xz" -> do
            un7z (takeDirectory base) sevenz arg
            handleArg sevenz base
        ".tar" -> un7z (takeDirectory base) sevenz arg
        _ -> error $ "handleArg: " ++ show (sevenz, arg, base, ext)
  where
    (base, ext) = splitExtension arg

un7z :: FilePath -- ^ dest path
     -> FilePath -- ^ 7z.exe
     -> FilePath -- ^ to be unpacked
     -> IO ()
un7z destPath sevenz =
    go
  where
    exts = [".7z", ".xz", ".tar"]
    go fp = when (ext `elem` exts) $ do
        putStrLn $ "Decompressing " ++ fp ++ " to " ++ destPath
        ec <- rawSystem sevenz
            [ "x"
            , "-o" ++ destPath
            , "-y"
            , fp
            ]
        removeFile fp
        when (ec /= ExitSuccess)
            $ error $ "Could not decompress: " ++ fp
      where
        ext = takeExtension fp

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
