{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Process
import Data.Aeson
import GHC.Generics
import Data.Text hiding (unpack)
import qualified Data.Text as T (unpack)
import Text.Printf
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Applicative
import System.Exit
import System.Environment
import System.IO.Temp
import System.IO (hPutStrLn, stderr)
import System.FilePath
import System.Directory
import Data.Monoid

data Flags = Insecure | None deriving Eq

data SlugInfo = SlugInfo {
  blob :: !BlobInfo,
  id :: !Text
} deriving (Show, Eq, Generic)

data BlobInfo = BlobInfo {
  url :: !Text
} deriving (Show, Eq, Generic)

instance FromJSON SlugInfo
instance FromJSON BlobInfo

type CurlOpt = String
type HerokuToken = String

forceDecode :: FromJSON a => LB.ByteString -> a
forceDecode s = case eitherDecode s of
                  Left e -> error $ printf "Could not decode '%s': %s" (B.unpack . LB.toStrict $ s) e
                  Right a -> a


ensureProcessOk :: ProcessHandle -> String -> IO ()
ensureProcessOk ph name = do
  code <- waitForProcess ph
  case code of
    ExitSuccess -> return ()
    ExitFailure i -> error $ printf "Process %s exited with code %d" name i

debugLn = hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  main' args None Nothing
  where
    main' :: [String] -> Flags -> Maybe HerokuToken -> IO ()
    main' ("-k":args) _ t = main' args Insecure t
    main' ("-t":token:args) f _ = main' args f $ Just token
    main' [appName, execName] f t = deploy appName execName f t
    main' ["-h"] _ _ = help
    main' _ _ _ = help >> exitFailure

help = do
  name <- getProgName
  debugLn $ printf "usage: %s [-k] [-t <token>] <heroku app name> <executable>" name
  debugLn "-k: don't check ssl certificate when uploading slug (helps on windows)"
  debugLn "-t <token>: use given heroku token"

deploy :: String -> String -> Flags -> Maybe HerokuToken -> IO ()
deploy appName execName flags maybeToken = do
  let baseCurlOpt = ["-nsS"] -- use netrc, be silent but show errors
  let defCurlOpt = if flags == Insecure then baseCurlOpt <> ["-k"] else baseCurlOpt
  let herokuCurlOpt = case maybeToken of
                        Nothing -> defCurlOpt
                        Just token -> defCurlOpt <> ["-H", "Authorization: Bearer "++token]
  validateArgs

  debugLn $ printf "deploying executable '%s' to app %s" execName appName
  !s@(SlugInfo (BlobInfo slugUrl) slugId) <- createSlug herokuCurlOpt execName appName
  print $ "created slug: " ++ show s
  uploadSlug defCurlOpt slugUrl execName appName
  releaseSlug herokuCurlOpt slugId appName
  print "Done"
  where
    validateArgs = do
      execExists <- doesFileExist execName
      unless execExists $ error $ printf "File %s does not exist!" execName

createSlug :: [CurlOpt] -> String -> String -> IO SlugInfo
createSlug curlOpt execName appName = do
  slugRespStr <- (let binary = "curl"
                      stdin = ""
                      args = curlOpt <> ["-X", "POST",
                       "-H", "Content-Type: application/json",
                       "-H", "Accept: application/vnd.heroku+json; version=3",
                       "-d", printf "{\"process_types\":{\"web\":\"./%s\"}}" $ takeFileName execName,
                       printf "https://api.heroku.com/apps/%s/slugs" appName]
                   in readProcess binary args stdin)
  return . forceDecode . LB.fromStrict . B.pack $ slugRespStr

uploadSlug :: [CurlOpt] -> Text -> String -> String -> IO ()
uploadSlug curlOpt slugUrl execName appName = do
  print $ "Packing binary " <> execName
  withSystemTempDirectory "heroku-deploy-binary" $ \workDir -> do
    createDirectory $ workDir </> "app"
    copyFile execName $ workDir </> "app" </> (takeFileName execName)
    let tgzFileName = workDir </> "slug.tgz"
    _ <- readCreateProcess (proc "tar" ["-cz", "./app", "-f", tgzFileName]) { cwd = Just workDir } ""
    print "Uploading binary"
    callProcess "curl" $ curlOpt <> ["-X", "PUT",
       "-H", "Content-Type:",
       "--data-binary", "@" ++ tgzFileName,
       T.unpack slugUrl]
    print "Binary uploaded"

releaseSlug :: [CurlOpt] -> Text -> String -> IO ()
releaseSlug curlOpt slugId appName = do
  print $ "Releasing slug " <> T.unpack slugId
  _ <- (let binary = "curl"
            stdin = ""
            args = curlOpt <> ["-X", "POST",
              "-H", "Content-Type: application/json",
              "-H", "Accept: application/vnd.heroku+json; version=3",
              "-d", printf "{\"slug\":\"%s\"}" (T.unpack slugId),
              printf "https://api.heroku.com/apps/%s/releases" appName]
         in readProcess binary args stdin)
  print "Slug released"