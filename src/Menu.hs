{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Menu where

import Data.Text () -- Instances

import Control.Lens

import GenericFeed

import New

data Preferences = Preferences
  { _showUnreadFeeds :: Bool
  , _showUnreadItems :: Bool
  , _displayHelp :: Bool
  }

makeLenses ''Preferences

data State = State { _menuState :: MenuState
                   , _menuPrefs :: Preferences
                   }

makeLenses ''State

initialState :: CacheFile -> State
initialState c = State
  { _menuState = menuFromCache c
  , _menuPrefs = Preferences
    { _showUnreadFeeds = True
    , _showUnreadItems = True
    , _displayHelp = False
    }
  }

feedsFilterPredicate :: State -> (String, Maybe CacheEntry) -> Bool
feedsFilterPredicate (State {_menuPrefs = Preferences {_showUnreadFeeds = True}}) _ = True
feedsFilterPredicate _ (_, Nothing) = False
feedsFilterPredicate _ (_, Just (_, is)) = any (not . snd) is

itemsFilterPredicate :: State -> (GenericItem, ItemStatus) -> Bool
itemsFilterPredicate (State {_menuPrefs = Preferences {_showUnreadItems = True}}) _ = True
itemsFilterPredicate _ (_, r) = not r

-- This function updates (if needed) the menu list indices, depending on the
-- current filtering settings.
touchListIdex :: State -> State
touchListIdex s = case s ^. menuState of
  MenuFeeds z -> set menuState (MenuFeeds $ over (listState . listStateFilter (feedsFilterPredicate s)) id z) s
  MenuItems False i -> set menuState (MenuItems False $ over (liItems . listState . listStateFilter (itemsFilterPredicate s)) id i) s
  _ -> s
