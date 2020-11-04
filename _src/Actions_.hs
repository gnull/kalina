module Actions_ where
    
-- This module constains the functions to manipulate the application state

import Control.Monad.State hiding (State)
import Control.Monad.Reader

import Data.Either.Combinators (maybeToRight)

import Data.Foldable (traverse_)

import Control.Lens

import State_
import MZipper_
import Utility_
import Config_
import Brick (EventM(..), Next, continue)

-- The Monads describing actions that can be performed in different menus
type FeedsMonad = ReaderT Config (StateT (CommonState, FeedsState) (EventM ()))
type ItemsMonad = ReaderT Config (StateT (CommonState, ItemsState) (EventM ()))
type MenuMonad  = ReaderT Config (StateT State (EventM ()))

-- The types of actions executed in feeds and items menus, they both return
-- State, thus they can switch between different menus
type FeedsAction = FeedsMonad (Next State)
type ItemsAction = ItemsMonad (Next State)
type MenuAction  = MenuMonad  (Next State)

-- Execute an action guarranteeing that it will not go to the items menu, but it
-- may change the FeedsState
onlyFeedsAction :: FeedsMonad () -> FeedsAction
onlyFeedsAction m = do
  m
  (cs, fs) <- get
  lift $ lift $ continue $ State cs $ Left fs

-- to ensure read-only access to the State
--
-- inside such a read-only monad, one
-- can use `ask' to read Config, and `lift ask' to read State
constMenuAction :: ReaderT Config (ReaderT State (EventM ())) () -> MenuAction
constMenuAction = mapReaderT f
  where
    f :: ReaderT State (EventM ()) () -> StateT State (EventM ()) (Next State)
    f r = do
      liftReaderT r
      s <- get
      lift $ continue s

-- The available feeds actions

enterFeed :: FeedsAction
enterFeed = do
  (cs, fs) <- get
  let isMaybe = enterFeedMaybe fs
  let s = State cs $ maybeToRight fs isMaybe
  lift $ lift $ continue s

fetchOneFeed :: (FilePath -> IO ()) -> MenuAction
fetchOneFeed queue = constMenuAction $ do
  (State _ ms) <- lift ask
  case ms of
    Left fs -> void $ (mFocus . _1 . preservingResult) (liftIO . queue) fs
    Right is -> void $ (liUrl . preservingResult) (liftIO . queue) is

fetchAllFeeds :: (FilePath -> IO ()) -> MenuAction
fetchAllFeeds queue = constMenuAction $ do
  (State _ ms) <- lift ask
  case ms of
    Left fs -> traverse_ (liftIO . queue . fst) $ fs
    Right is -> do
      traverse_ (liftIO . queue . fst) $ reverse $ is ^. liBefore
      void $ (liUrl . preservingResult) (liftIO . queue) is
