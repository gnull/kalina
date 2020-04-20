{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, mapM_)

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Data.Text () -- Instances
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)

import Control.Lens ((^.))

import GenericFeed

import Brick
import Brick.Widgets.List
import Brick.BChan
import qualified Graphics.Vty as V

import Options.Applicative hiding (str)

import State
import State.Menu
import Interface
import GenericFeed.Binary (readCacheFile, writeCacheFile)

import Concurrency (workerThread, WorkerEvent, handleThreadEvent)
import Control.Concurrent.Async (async, cancel)

cacheFromState :: State -> CacheFile
cacheFromState s = menuToCache $ s ^. menuState

handle :: (FilePath -> IO ()) -> State -> BrickEvent () WorkerEvent -> EventM () (Next State)
handle queue s (VtyEvent e) = handleMenu queue s e
handle _ st (AppEvent e) = continue $ handleThreadEvent e st
handle _ s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (listAttr,            V.white `on` V.black)
    , (listSelectedAttr,    V.white `on` V.blue)
    , ("unread-item",       V.withStyle V.currentAttr V.bold)
    , ("hightlight",        V.withStyle V.currentAttr V.bold)
    , ("title",             V.black `on` V.white)
    , ("FetchNothing",      V.withStyle V.currentAttr V.dim)
    , ("FetchStarted",      V.withStyle (V.withForeColor V.currentAttr V.yellow) V.blink)
    , ("FetchFailed",       V.withForeColor V.currentAttr V.red)
    , ("FetchOK",           V.withForeColor V.currentAttr V.green)
    ]

app :: TimeZone -> (FilePath -> IO ()) -> App State WorkerEvent ()
app z q = App
  { appDraw = draw z
  , appHandleEvent = handle q
  , appStartEvent = return
  , appAttrMap = const $ theMap
  , appChooseCursor = neverShowCursor
  }

data Options = Options
  { oUrls :: FilePath
  , oCache :: FilePath
  } deriving (Show)

options :: FilePath -> Parser Options
options home = Options
  <$> strOption
      ( long "urls"
     <> short 'u'
     <> help "A file with list of RSS feeds (compatible with newsboat)"
     <> metavar "FILE"
     <> value (home </> ".newsboat/urls")
     <> action "file"
     <> showDefault )
  <*> strOption
      ( long "cache"
     <> short 'c'
     <> help "A file in which to store cached RSS entries and read/unread status"
     <> metavar "FILE"
     <> value "/tmp/news-cache"
     <> action "file"
     <> showDefault )

parseOpts :: IO Options
parseOpts = do
    home <- getHomeDirectory
    execParser $ opts home
  where
    opts home = info (options home <**> helper)
      ( fullDesc
     <> progDesc "Open TUI to fetch and display RSS feeds"
     <> header "news-view -- An RSS reader written in Haskell and inspired by newsboat" )

defaultMain' :: (Ord n) => BChan e -> App s e n -> s -> IO s
defaultMain' ch ap st = do
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    customMain initialVty builder (Just ch) ap st

-- Note: https://hackage.haskell.org/package/filemanip-0.3.6.3/docs/System-FilePath-Manip.html#v:modifyInPlace
main :: IO ()
main = do
  Options {..} <- parseOpts
  urls <- parseFeedsConfig <$> readFile oUrls
  feeds <- refreshCacheFileWithUrls urls <$> readCacheFile oCache
  let s = initialState feeds
  from <- newBChan 100
  to <- newBChan 20
  th <- replicateM 10 $ async $ workerThread from to
  z <- getCurrentTimeZone
  writeCacheFile oCache =<< cacheFromState <$> defaultMain' to (app z $ writeBChan from) s
  mapM_ cancel th
