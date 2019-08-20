{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GenericFeed where
    
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

-- Removes empty lines and #-comments, extracts URLs from first word in each
-- line
parseFeedsConfig :: String -> [String]
parseFeedsConfig = filter (not . null) . map processUrlLine . lines
  where
    processUrlLine = headDefault "" . words . takeWhile (/= '#')
    headDefault d [] = d
    headDefault _ (x:_) = x

showFeed :: Feed -> String
showFeed (AtomFeed (A.Feed {A.feedTitle, A.feedEntries})) = show feedTitle ++ " ---- " ++ show feedEntries
showFeed (RSSFeed (R.RSS {R.rssChannel = (R.RSSChannel {R.rssTitle, R.rssItems})})) = show rssTitle ++ " ---- " ++ show rssItems

data GenericItem = GenericItem
  { giTitle :: Maybe Text -- Title displayed in list
  , giURL :: Maybe Text -- URL to follow
  , giAuthor :: Maybe Text
  , giBody :: Maybe A.EntryContent -- Contents displayed when Enter is pressed
  } deriving (Show)

data GenericFeed = GenericFeed
  { gfTitle :: String
  , gfURL :: Text
  , gfItems :: [GenericItem]
  } deriving (Show)

atomItemToGeneric :: A.Entry -> GenericItem
atomItemToGeneric e = GenericItem
  { giTitle = Just $ T.pack $ A.txtToString $ A.entryTitle e
  , giURL = Just $ A.entryId e
  , giAuthor = Just $ T.pack $ show $ A.entryAuthors e
  , giBody = A.entryContent e
  }

rssItemToGeneric :: R.RSSItem -> GenericItem
rssItemToGeneric e = GenericItem
  { giTitle = R.rssItemTitle e
  , giURL = R.rssItemLink e
  , giAuthor = R.rssItemAuthor e
  , giBody = A.TextContent <$> R.rssItemDescription e
  }

rss1ItemToGeneric :: R1.Item -> GenericItem
rss1ItemToGeneric (R1.Item {..}) = GenericItem
  { giTitle = Just itemTitle
  , giURL = Just itemURI
  , giAuthor = Nothing
  , giBody = A.TextContent <$> itemDesc
  }

feedToGeneric :: Feed -> GenericFeed
feedToGeneric (AtomFeed f) = GenericFeed
  { gfTitle = A.txtToString $ A.feedTitle f
  , gfURL = A.feedId f
  , gfItems = map atomItemToGeneric $ A.feedEntries f
  }
feedToGeneric (RSSFeed (R.RSS {R.rssChannel = r})) = GenericFeed
  { gfTitle = T.unpack $ R.rssTitle r
  , gfURL = R.rssLink r
  , gfItems = map rssItemToGeneric $ R.rssItems r
  }
feedToGeneric (RSS1Feed (R1.Feed {R1.feedItems = i, R1.feedChannel = c})) = GenericFeed
  { gfTitle = T.unpack $ R1.channelTitle c
  , gfURL = R1.channelURI c
  , gfItems = map rss1ItemToGeneric i
  }
feedToGeneric (XMLFeed _) = error "Unrecognized feed format"

showGenericItem :: GenericItem -> [String]
showGenericItem (GenericItem {..}) =
  [ "----"
  , show giTitle ++ " by " ++ show giAuthor
  , show giURL
  , show giBody
  ]

showGenericFeed :: GenericFeed -> String
showGenericFeed (GenericFeed {..}) = unlines
  $ [gfTitle ++ " (" ++ T.unpack gfURL ++ ")"]
  ++ (map ("  " ++) $ concatMap showGenericItem gfItems)
