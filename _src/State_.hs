{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module State_ where

import Control.Lens hiding (element)
import Data.Foldable

import Data.Functor.Compose (Compose(..))

-- These are the types contained in our application state
import GenericFeed
import State.Fetch
import MZipper_

-- Items and feeds filtering preferences
data FilterPrefs = FilterPrefs
  { _showUnreadFeeds :: Bool
  , _showUnreadItems :: Bool
  , _forceShowCurrentFeed :: Bool
  , _forceShowCurrentItem :: Bool
  }

$(makeLenses ''FilterPrefs)

-- The part of the state which is present in both feels menu and items menus
data CommonState = CommonState
                   { _filterPrefs :: FilterPrefs   -- Filtering preferences
                   , _displayHelp :: Bool          -- Are we showing help right now?
                   , _fetchState :: FetchState     -- The feeds fetching queue
                   }

$(makeLenses ''CommonState)

-- The part of the state which is present only in the feeds menu
type FeedsState = (MZipper (FilePath, Maybe CacheEntry))

-- The part of the state which is present only in the items menu
data ItemsState = ItemsState
  { _liBefore :: [(FilePath, Maybe CacheEntry)]
  , _liAfter :: [(FilePath, Maybe CacheEntry)]
  , _liUrl :: FilePath
  , _liFeed :: GenericFeed
  , _liItems :: MZipper (GenericItem, ItemStatus)
  }

$(makeLenses ''ItemsState)

-- The two previous types are convertible into one-another, and in fact `Either
-- FeedsState ItemsState' forms a zipper for walking in the menu tree.

-- This is the global state of our application
data State = State CommonState (Either FeedsState ItemsState)

-- Enter the current feed if possible
--
-- This function is good to use with
-- <https://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Combinators.html#v:maybeToLeft>
enterFeedMaybe :: FeedsState -> Maybe ItemsState
enterFeedMaybe (Compose Nothing) = Nothing
enterFeedMaybe (Compose (Just (Zipper _ (_, Nothing) _))) = Nothing
enterFeedMaybe (Compose (Just (Zipper bef (url, Just (feed, is)) aft))) = Just $ ItemsState bef aft url feed $ fromList is

-- Quit the current feed and go to the feeds list
quitFeed :: ItemsState -> FeedsState
quitFeed (ItemsState {..}) = mZipper $ Zipper _liBefore (_liUrl, Just (_liFeed, toList _liItems)) _liAfter
