module Main where

import GenericFeed

import System.Environment

import Control.Arrow ((&&&))
import Data.Maybe

import Control.Monad
import Control.Exception

import Data.ByteString.Lazy.Char8 (unpack)

import Network.Wreq
import Network.HTTP.Client (HttpException)
import Control.Lens
import Text.Feed.Import
import Text.Feed.Types

main :: IO ()
main = do
    [uFile, cFile] <- getArgs
    us <- parseFeedsConfig <$> readFile uFile
    feeds <- fmap catMaybes $ forM us $ \u -> do
      putStr $ u ++ "... "
      flip catch handler $ do
        x <- get u
        putStrLn "ok"
        pure $ parseFeedString $ unpack $ x ^. responseBody
    let gFeeds = map (feedToGeneric &&& itemsToGeneric) feeds
    writeFile cFile $ show gFeeds
  where
    handler :: HttpException -> IO (Maybe Feed)
    handler _ = do
      putStrLn "HttpException"
      pure Nothing
