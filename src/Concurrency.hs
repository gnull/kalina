{-# LANGUAGE TupleSections #-}
module Concurrency where

import Data.Maybe
import Data.List (delete)
import Control.Arrow ((&&&))

import Control.Exception.Safe (catchAny)

import Brick
import Brick.BChan

import Network.Wreq
import Control.Lens

import Data.ByteString.Lazy.Char8 (unpack)

import Text.Feed.Import

import GenericFeed
import Menu

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
  r <- catchAny (maybe (Failed u) (Finished u) <$> fetchFeed u) $ \e -> pure (Failed u)
  writeBChan to r

patch :: (GenericFeed, [GenericItem]) -> Maybe CacheEntry -> Maybe CacheEntry
patch (f, is) Nothing = Just $ newCacheEntry f is
patch (f, is) (Just (f', is')) = Just (f, goodIs ++ is')
  where goodIs = map (,False) $ foldr Data.List.delete is $ map fst is'

patchList :: L (String, Maybe CacheEntry) -> (String, (GenericFeed, [GenericItem])) -> L (String, Maybe CacheEntry)
patchList fs (u, f) = fs <&> \(u', f') -> if u == u' then (u, patch f f') else (u', f')

handleThreadEvent :: MenuState -> WorkerEvent -> EventM () (Next MenuState)
handleThreadEvent s (Finished u f) = case s of
  LevelFeeds fs -> continue $ LevelFeeds $ patchList fs (u, f)
  LevelItems fs _ -> let
        fs' = patchList fs (u, f)
        x' = selectedElement fs'
        is' = fromJust $ snd x'
      in continue $ LevelItems fs' $ toGenericList $ snd is'
  LevelContents fs is -> let
        fs' = patchList fs (u, f)
        x' = selectedElement fs'
        is' = fromJust $ snd x'
      in continue $ LevelContents fs' (toGenericList $ snd is')
handleThreadEvent s _ = continue s
