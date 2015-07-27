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

data SlugInfo = SlugInfo {
  blob :: !BlobInfo,
  id :: !Text
} deriving (Show, Eq, Generic)

data BlobInfo = BlobInfo {
  url :: !Text
} deriving (Show, Eq, Generic)

instance FromJSON SlugInfo
instance FromJSON BlobInfo

forceLeft :: Show e => Either e a -> a
forceLeft (Right a) = a
forceLeft (Left e) = error $ show e

forceDecode :: FromJSON a => LB.ByteString -> a
forceDecode s = case eitherDecode s of
                  Left e -> error $ printf "Could not decode '%s': %s" (B.unpack . LB.toStrict $ s) e
                  Right a -> a

defCurlOpt = "-nsS" -- use netrc, be silent but show errors

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
  case args of
    [appName, execName] -> deploy appName execName
    _ -> help

help = do
  name <- getProgName
  debugLn $ printf "usage: %s <heroku app name> <executable>" name

deploy appName execName = do
  execExists <- doesFileExist execName
  unless execExists $ error $ printf "File %s does not exist!" execName
  debugLn $ printf "deploying executable '%s' to app %s" execName appName
  slugRespStr <- (let binary = "curl"
                      stdin = ""
                      args = ["-X", "POST",
                       "-H", "Content-Type: application/json",
                       "-H", "Accept: application/vnd.heroku+json; version=3",
                       "-d", printf "{\"process_types\":{\"web\":\"./%s\"}}" execName,
                       defCurlOpt,
                       printf "https://api.heroku.com/apps/%s/slugs" appName]
                   in readProcess binary args stdin)
  let !s@(SlugInfo (BlobInfo slugUrl) slugId) = forceDecode . LB.fromStrict . B.pack $ slugRespStr
  print s
  print "Uploading binary"
  print "Packing binary"
  withSystemTempDirectory "heroku-deploy-binary" $ \workDir -> do
    createDirectory $ workDir </> "app"
    copyFile execName $ workDir </> "app" </> (takeFileName execName)
    (_, Just tarOut, _, tarHandle) <- createProcess $ (proc "tar" ["-cz", "app"]) { std_out = CreatePipe, cwd = Just workDir }
    (_,_,_,uploadHandle) <- createProcess $ (proc "curl" ["-X", "PUT",
       "-H", "Content-Type:",
       "--data-binary", "@-",
       defCurlOpt,
       T.unpack slugUrl]) { std_in = UseHandle tarOut }
    sequence_ [ensureProcessOk tarHandle "tar", ensureProcessOk uploadHandle "curl"]

  print "Releasing slug"
  _ <- (let binary = "curl"
            stdin = ""
            args = ["-X", "POST",
              "-H", "Content-Type: application/json",
              "-H", "Accept: application/vnd.heroku+json; version=3",
              "-d", printf "{\"slug\":\"%s\"}" (T.unpack slugId),
              defCurlOpt,
              printf "https://api.heroku.com/apps/%s/releases" appName]
         in readProcess binary args stdin)
  print "Done"

--
-- {"blob":{"method":"put","url":"https://s3-external-1.amazonaws.com/herokuslugs/heroku.com/v1/b9325f64-51df-44ab-8281-7278a2607465?AWSAccessKeyId=AKIAJWLOWWHPBWQOPJZQ&Signature=EufM%2F1yoC2E5YC4ux6XcWq8mIE8%3D&Expires=1438020738"}
-- ,"buildpack_provided_description":null,"commit":null,"commit_description":null,"created_at":"2015-07-27T17:12:18Z",
-- "id":"b9325f64-51df-44ab-8281-7278a2607465",
-- "process_types":{"web":"greenticket"},"size":null,"updated_at":"2015-07-27T17:12:18Z","stack":{"id":"f9f9cbd7-2970-41ef-8db5-3df7123b041f","name":"cedar-14"}}
--
-- --
-- curl -X PUT \