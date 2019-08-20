module Main where

import GenericFeed

import System.Environment

import Data.Char
import Data.List
import Data.Maybe

import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (unpack)

import Network.Wreq
import Control.Lens
import Text.Feed.Import
import Text.Feed.Types
import qualified Text.Atom.Feed as A
import qualified Text.RSS.Syntax as R
import qualified Text.RSS1.Syntax as R1

main :: IO ()
main = do
  [f] <- getArgs
  us <- parseFeedsConfig <$> readFile f
  feeds <- forM us $ \u -> do
    x <- get u
    pure $ fromJust $ parseFeedString $ unpack $ x ^. responseBody
  putStrLn $ unlines $ map (showGenericFeed . feedToGeneric) feeds
