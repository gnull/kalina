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
  , _forceShowFeed :: Maybe FilePath
  , _forceShowItem :: Maybe GenericItem
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
    , _forceShowFeed = Nothing
    , _forceShowItem = Nothing
    }
  , _displayHelp = False
  , _fetchState = fetchInitial
  }

feedsFilterPredicate :: FilterPrefs -> (String, Maybe CacheEntry) -> Bool
feedsFilterPredicate (FilterPrefs {_showUnreadFeeds = True}) _ = True
feedsFilterPredicate (FilterPrefs {_forceShowFeed = u}) (u', f)
  = u == Just u' || case f of
    Nothing -> False
    Just (_, is) -> any (not . snd) is

itemsFilterPredicate :: FilterPrefs -> (GenericItem, ItemStatus) -> Bool
itemsFilterPredicate (FilterPrefs {_showUnreadItems = True}) _ = True
itemsFilterPredicate (FilterPrefs {_forceShowItem = i}) (i', r)
  = i == Just i' || not r

forceShowCurrentFeed :: State -> State
forceShowCurrentFeed s = set (filterPrefs . forceShowFeed) (s ^? menuState . selectedFeedUrl) s

clearForceShowFeed :: State -> State
clearForceShowFeed = set (filterPrefs . forceShowFeed) Nothing

forceShowCurrentItem :: State -> State
forceShowCurrentItem s = set (filterPrefs . forceShowItem) (s ^? menuState . selectedItem . _1) s

clearForceShowItem :: State -> State
clearForceShowItem = set (filterPrefs . forceShowItem) Nothing

-- This function updates (if needed) the menu list indices, depending on the
-- current filtering settings.
touchListIdex :: State -> State
touchListIdex s = case s ^. menuState of
  MenuFeeds z -> set menuState (MenuFeeds $ over (listState . listStateFilter' False (feedsFilterPredicate $ s ^. filterPrefs)) id z) s
  MenuItems i -> set menuState (MenuItems $ over (liItems . listState . listStateFilter' False (itemsFilterPredicate $ s ^. filterPrefs)) id i) s
  _ -> s
