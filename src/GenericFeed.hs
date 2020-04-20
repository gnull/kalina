{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GenericFeed where

import Control.Arrow ((&&&))
import Control.Monad (join)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import GHC.Generics (Generic)
import Data.Binary (Binary) -- only to derive instances here

import Text.Feed.Types
import qualified Text.Atom.Feed as A
import qualified Text.RSS.Syntax as R
import qualified Text.RSS1.Syntax as R1

data GenericItem = GenericItem
  { giTitle :: Maybe Text -- Title displayed in list
  , giURL :: Maybe Text -- URL to follow
  , giDate :: Maybe Text -- TODO: Replace this with some better type for date.
                         -- Also, newsboat seems to write `now` into this field instead of Nothing
  , giAuthor :: Maybe Text
  , giBody :: Maybe Text -- Contents displayed when Enter is pressed (HTML by default)
  } deriving (Show, Read, Eq, Generic)
instance Binary GenericItem

giURLL :: Lens' GenericItem (Maybe Text)
giURLL f st = (f $ giURL st) <&> \u -> st {giURL = u}

data GenericFeed = GenericFeed
  { gfTitle :: Text
  , gfURL :: Text
  } deriving (Show, Read, Generic)
instance Binary GenericFeed

entryContentToText :: A.EntryContent -> Text
entryContentToText (A.TextContent x) = x
entryContentToText (A.ExternalContent _ x) = x
entryContentToText _ = T.pack "*HTML gibbreish*"

removeBadCharacters :: Text -> Text
removeBadCharacters = T.unwords . T.words

atomItemToGeneric :: A.Entry -> GenericItem
atomItemToGeneric e = GenericItem
  { giTitle = Just $ removeBadCharacters $ textContentToText $ A.entryTitle e
  , giURL = Just $ A.entryId e
  , giDate = Just $ removeBadCharacters $ A.entryUpdated e
  , giAuthor = Just $ removeBadCharacters $ T.pack $ show $ A.entryAuthors e
  , giBody = entryContentToText <$> A.entryContent e
  }

rssItemToGeneric :: R.RSSItem -> GenericItem
rssItemToGeneric e = GenericItem
  { giTitle = removeBadCharacters <$> R.rssItemTitle e
  , giURL = R.rssItemLink e
  , giDate = removeBadCharacters <$> R.rssItemPubDate e
  , giAuthor = removeBadCharacters <$> R.rssItemAuthor e
  , giBody = R.rssItemDescription e
  }

rss1ItemToGeneric :: R1.Item -> GenericItem
rss1ItemToGeneric (R1.Item {..}) = GenericItem
  { giTitle = Just $ removeBadCharacters itemTitle
  , giURL = Just itemURI
  , giDate = Nothing
  , giAuthor = Nothing
  , giBody = entryContentToText <$> A.TextContent <$> itemDesc
  }

itemsToGeneric :: Feed -> [GenericItem]
itemsToGeneric (AtomFeed (A.Feed {A.feedEntries = f})) = map atomItemToGeneric f
itemsToGeneric (RSSFeed (R.RSS {R.rssChannel = R.RSSChannel {R.rssItems = r}}) ) = map rssItemToGeneric r
itemsToGeneric (RSS1Feed (R1.Feed {R1.feedItems = i})) = map rss1ItemToGeneric i
itemsToGeneric (XMLFeed _ ) = error "Unrecognized feed format"

textContentToText :: A.TextContent -> Text
textContentToText (A.TextString x) = x
textContentToText (A.HTMLString x) = x
textContentToText _ = error "Unexpected content format"

feedToGeneric :: Feed -> GenericFeed
feedToGeneric (AtomFeed f) = GenericFeed
  { gfTitle = removeBadCharacters $ textContentToText $ A.feedTitle f
  , gfURL = A.feedId f
  }
feedToGeneric (RSSFeed (R.RSS {R.rssChannel = r})) = GenericFeed
  { gfTitle = removeBadCharacters $ R.rssTitle r
  , gfURL = R.rssLink r
  }
feedToGeneric (RSS1Feed (R1.Feed {R1.feedChannel = c})) = GenericFeed
  { gfTitle = removeBadCharacters $ R1.channelTitle c
  , gfURL = R1.channelURI c
  }
feedToGeneric (XMLFeed _) = error "Unrecognized feed format"

-- Removes empty lines and #-comments, extracts URLs from first word in each
-- line
parseFeedsConfig :: String -> [String]
parseFeedsConfig = filter (not . null) . map processUrlLine . lines
  where
    processUrlLine = headDefault "" . words . takeWhile (/= '#')
    headDefault d [] = d
    headDefault _ (x:_) = x

type ItemStatus = Bool
type CacheEntry = (GenericFeed, [(GenericItem, ItemStatus)])

type CacheFile = [(String, Maybe CacheEntry)]

refreshCacheFileWithUrls :: [FilePath] -> CacheFile -> CacheFile
refreshCacheFileWithUrls us cf = map f us
  where f u = (u, join $ lookup u cf)
