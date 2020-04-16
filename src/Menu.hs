{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Menu where

import Data.Text () -- Instances

import Control.Lens

import GenericFeed

import New

data State = State { _menuState :: MenuState
                   , _showUnreadFeeds :: Bool
                   , _showUnreadItems :: Bool
                   , _displayHelp :: Bool
                   }

makeLenses ''State

-- A lens which looks at all the items of the currently selected feed
selectedFeedItems :: Traversal' MenuState (GenericItem, ItemStatus)
selectedFeedItems f (MenuFeeds z) = MenuFeeds <$> (mFocus . _2 . _Just . _2 . traverse) f z
selectedFeedItems f (MenuItems b is) = MenuItems b <$> (liItems . traverse) f is

-- This is similar to the previous one, but looks at only one selected item (if
-- one is selected).
selectedItem :: Traversal' MenuState (GenericItem, ItemStatus)
selectedItem _ (MenuFeeds z) = pure $ MenuFeeds z
selectedItem f (MenuItems b is) = MenuItems b <$> (liItems . mFocus) f is

type Getting' s a = Getting a s a

allFeeds :: Getting' MenuState (MZipper (FilePath, Maybe CacheEntry))
allFeeds f s = case menuUp $ menuUp s of
  MenuFeeds x -> Const $ getConst $ f x
  _ -> undefined

initialState :: CacheFile -> State
initialState c = State { _menuState = menuFromCache c
                       , _showUnreadFeeds = True
                       , _showUnreadItems = True
                       , _displayHelp = False
                       }
