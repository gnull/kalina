{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module State where

import Data.Text () -- Instances

import Control.Lens

import GenericFeed

import State.Menu
import State.Fetch

data FilterPrefs = FilterPrefs
  { _showUnreadFeeds :: Bool
  , _showUnreadItems :: Bool
  }

makeLenses ''FilterPrefs

data State = State { _menuState :: MenuState
                   , _filterPrefs :: FilterPrefs
                   , _displayHelp :: Bool
                   , _fetchState :: FetchState
                   }

makeLenses ''State

initialState :: CacheFile -> State
initialState c = State
  { _menuState = menuFromCache c
  , _filterPrefs = FilterPrefs
    { _showUnreadFeeds = True
    , _showUnreadItems = True
    }
  , _displayHelp = False
  , _fetchState = fetchInitial
  }

feedsFilterPredicate :: FilterPrefs -> (String, Maybe CacheEntry) -> Bool
feedsFilterPredicate (FilterPrefs {_showUnreadFeeds = True}) _ = True
feedsFilterPredicate _ (_, Nothing) = False
feedsFilterPredicate _ (_, Just (_, is)) = any (not . snd) is

itemsFilterPredicate :: FilterPrefs -> (GenericItem, ItemStatus) -> Bool
itemsFilterPredicate (FilterPrefs {_showUnreadItems = True}) _ = True
itemsFilterPredicate _ (_, r) = not r

-- This function updates (if needed) the menu list indices, depending on the
-- current filtering settings.
touchListIdex :: State -> State
touchListIdex s = case s ^. menuState of
  MenuFeeds z -> set menuState (MenuFeeds $ over (listState . listStateFilter' False (feedsFilterPredicate $ s ^. filterPrefs)) id z) s
  MenuItems False i -> set menuState (MenuItems False $ over (liItems . listState . listStateFilter' False (itemsFilterPredicate $ s ^. filterPrefs)) id i) s
  _ -> s
