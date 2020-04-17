{-# LANGUAGE TupleSections #-}
module Concurrency where

import Control.Arrow ((&&&))
import Control.Lens (over)

import Control.Exception.Safe (catchAny)

import Brick.BChan

import Network.Wreq
import Control.Lens ((^.))

import Text.Feed.Import

import GenericFeed
import State
import State.Menu
import State.Fetch

fetchFeed :: FilePath -> IO (Maybe (GenericFeed, [GenericItem]))
fetchFeed u = do
  x <- get u
  let x' = parseFeedSource $ x ^. responseBody -- TODO: Proper error handling here
  pure $ (feedToGeneric &&& itemsToGeneric) <$> x'

-- TODO: Add a String to Failed constructor for error message
data WorkerEvent
 = Started FilePath                               -- We started downloading a feed
 | Finished FilePath (GenericFeed, [GenericItem]) -- Here is the downloaded feed
 | Failed FilePath                                -- Downloading failed

-- The second argument is required to be BChan by Brick, so I set first to the
-- same type for uniformity.
workerThread :: BChan FilePath -> BChan WorkerEvent -> IO ()
workerThread from to = sequence_ $ repeat $ do
  u <- readBChan from
  writeBChan to $ Started u
  r <- catchAny (maybe (Failed u) (Finished u) <$> fetchFeed u) $ \_ -> pure (Failed u)
  writeBChan to r

handleThreadEvent :: WorkerEvent -> State -> State
handleThreadEvent (Finished u f) s = over fetchState (fetchUpdate u FetchOK) $ over menuState (appendNewItems u f) s
handleThreadEvent (Started u) s = over fetchState (fetchUpdate u FetchStarted) s
handleThreadEvent (Failed u) s = over fetchState (fetchUpdate u FetchFailed) s
