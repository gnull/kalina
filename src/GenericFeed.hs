{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GenericFeed where

import Data.Maybe
import Data.List
    
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Feed.Types
import qualified Text.Atom.Feed as A
import qualified Text.RSS.Syntax as R
import qualified Text.RSS1.Syntax as R1

data GenericItem = GenericItem
  { giTitle :: Maybe Text -- Title displayed in list
  , giURL :: Maybe Text -- URL to follow
  , giDate :: Maybe Text -- TODO: Replace this with some better type for date
  , giAuthor :: Maybe Text
  , giBody :: Maybe Text -- Contents displayed when Enter is pressed
  } deriving (Show, Read)

data GenericFeed = GenericFeed
  { gfTitle :: String
  , gfURL :: Text
  , gfItems :: [GenericItem]
  } deriving (Show, Read)

entryContentToText :: A.EntryContent -> Text
entryContentToText (A.TextContent x) = x
entryContentToText (A.ExternalContent _ x) = x
entryContentToText _ = T.pack "*HTML gibbreish*"

atomItemToGeneric :: A.Entry -> GenericItem
atomItemToGeneric e = GenericItem
  { giTitle = Just $ T.pack $ A.txtToString $ A.entryTitle e
  , giURL = Just $ A.entryId e
  , giDate = Just $ A.entryUpdated e
  , giAuthor = Just $ T.pack $ show $ A.entryAuthors e
  , giBody = entryContentToText <$> A.entryContent e
  }

rssItemToGeneric :: R.RSSItem -> GenericItem
rssItemToGeneric e = GenericItem
  { giTitle = R.rssItemTitle e
  , giURL = R.rssItemLink e
  , giDate = R.rssItemPubDate e
  , giAuthor = R.rssItemAuthor e
  , giBody = R.rssItemDescription e
  }

rss1ItemToGeneric :: R1.Item -> GenericItem
rss1ItemToGeneric (R1.Item {..}) = GenericItem
  { giTitle = Just itemTitle
  , giURL = Just itemURI
  , giDate = Nothing
  , giAuthor = Nothing
  , giBody = entryContentToText <$> A.TextContent <$> itemDesc
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

-- Removes empty lines and #-comments, extracts URLs from first word in each
-- line
parseFeedsConfig :: String -> [String]
parseFeedsConfig = filter (not . null) . map processUrlLine . lines
  where
    processUrlLine = headDefault "" . words . takeWhile (/= '#')
    headDefault d [] = d
    headDefault _ (x:_) = x

-- The next two functions are for debugging purposes

printGenericItem :: GenericItem -> IO ()
printGenericItem (GenericItem {..}) = do
  T.putStrLn $ fm giTitle <> " by " <> fm giAuthor
  T.putStrLn $ fm giURL
  T.putStrLn $ fm giBody
  where fm = fromMaybe ("" :: Text)

showGenericFeed :: GenericFeed -> IO ()
showGenericFeed (GenericFeed {..}) = do
  T.putStrLn $ T.pack gfTitle <> " (" <> gfURL <> ")"
  sequence_ $ intersperse (putStrLn "---") $ map printGenericItem gfItems
