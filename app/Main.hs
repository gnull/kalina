module Main where

import GenericFeed

import System.Environment

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
    [f] <- getArgs
    us <- parseFeedsConfig <$> readFile f
    feeds <- forM us $ \u -> do
      putStr $ u ++ "... "
      flip catch handler $ do
        x <- get u
        putStrLn "ok"
        pure $ parseFeedString $ unpack $ x ^. responseBody
    putStrLn $ unlines $ map (showGenericFeed . feedToGeneric) $ catMaybes feeds
  where
    handler :: HttpException -> IO (Maybe Feed)
    handler _ = do
      putStrLn "HttpException"
      pure Nothing
