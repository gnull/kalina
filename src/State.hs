{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module State where

import Data.Text () -- Instances

import Control.Lens

import GenericFeed

import State.Menu
import State.Fetch

data Prefs = Prefs
  { _showUnreadFeeds :: Bool
  , _showUnreadItems :: Bool
  }

makeLenses ''Prefs

data State = State { _menuState :: MenuState
                   , _menuPrefs :: Prefs
                   , _displayHelp :: Bool
                   , _fetchState :: FetchState
                   , _forceShowFeed :: Maybe Int
                   , _forceShowItem :: Maybe Int
                   }

makeLenses ''State

initialState :: CacheFile -> State
initialState c = State
  { _menuState = menuFromCache c
  , _menuPrefs = Prefs
    { _showUnreadFeeds = True
    , _showUnreadItems = True
    }
  , _displayHelp = False
  , _fetchState = fetchInitial
  , _forceShowFeed = Nothing
  , _forceShowItem = Nothing
  }

feedsFilterPredicate :: Prefs -> (String, Maybe CacheEntry) -> Bool
feedsFilterPredicate (Prefs {_showUnreadFeeds = True}) _ = True
feedsFilterPredicate _ (_, Nothing) = False
feedsFilterPredicate _ (_, Just (_, is)) = any (not . snd) is

itemsFilterPredicate :: Prefs -> (GenericItem, ItemStatus) -> Bool
itemsFilterPredicate (Prefs {_showUnreadItems = True}) _ = True
itemsFilterPredicate _ (_, r) = not r

-- This function updates (if needed) the menu list indices, depending on the
-- current filtering settings.
touchListIdex :: State -> State
touchListIdex s = case s ^. menuState of
  MenuFeeds z -> set menuState (MenuFeeds $ over (listState . listStateFilter Nothing (feedsFilterPredicate $ s ^. menuPrefs)) id z) s
  MenuItems False i -> set menuState (MenuItems False $ over (liItems . listState . listStateFilter Nothing (itemsFilterPredicate $ s ^. menuPrefs)) id i) s
  _ -> s

forceShowSelected :: State -> State
forceShowSelected s = case s ^. menuState of
    MenuFeeds z -> set forceShowFeed (maybePos z) s
    MenuItems _ i -> set forceShowItem (maybePos $ i ^. liItems) s

cancelForceShowSelected :: State -> State
cancelForceShowSelected s = touchListIdex $ case s ^. menuState of
  MenuFeeds _ -> set forceShowFeed Nothing s
  MenuItems _ _ -> set forceShowItem Nothing s
