{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Data.Text () -- Instances

import GenericFeed

import Brick
import Brick.Widgets.List
import qualified Graphics.Vty as V

import Options.Applicative hiding (str)

import Menu

data State = State MenuState

cacheFromState :: State -> CacheFile
cacheFromState (State m) = listElements s
  where
    s = case m of
      LevelFeeds x -> x
      LevelItems x _ _ -> x
      LevelContents x _ _ _ -> x

initialState :: CacheFile -> State
initialState = State . initialMenuState

draw :: State -> [Widget ()]
draw (State s) = [drawMenu s]

handle :: State -> BrickEvent () () -> EventM () (Next State)
handle (State s) (VtyEvent e) = fmap State <$> handleMenu s e
handle s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (listAttr,            V.white `on` V.black)
    , (listSelectedAttr,    V.white `on` V.blue)
    , ("unread-item",       V.withStyle V.currentAttr V.bold)
    ]

app :: App State () ()
app = App
  { appDraw = draw
  , appHandleEvent = handle
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

-- Note: https://hackage.haskell.org/package/filemanip-0.3.6.3/docs/System-FilePath-Manip.html#v:modifyInPlace
main :: IO ()
main = do
  Options {..} <- parseOpts
  urls <- parseFeedsConfig <$> readFile oUrls
  feeds <- refreshCacheFileWithUrls urls <$> readCacheFile oCache
  let s = initialState feeds
  writeCacheFile oCache =<< cacheFromState <$> defaultMain app s
