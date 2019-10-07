{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.Maybe
import Data.List
import Data.Text () -- Instances
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))

import GenericFeed

import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V

import Options.Applicative hiding (str)

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

toGenericList :: [a] -> MyList () a
toGenericList x = list () x 1

-- Internal state of the interface and a couple of itw utility functions

data State
  = LevelFeeds    (L (String, Maybe CacheEntry))
  | LevelItems    (L (String, Maybe CacheEntry)) (String, Maybe CacheEntry) (L (GenericItem, ItemStatus))
  | LevelContents (L (String, Maybe CacheEntry)) (String, Maybe CacheEntry) (L (GenericItem, ItemStatus)) (GenericItem, ItemStatus)

stateDown :: State -> State
stateDown s@(LevelFeeds fs) =
  let f = snd $ fromJust $ listSelectedElement fs
  in case snd f of
    Nothing -> s
    Just (_, is) -> LevelItems fs f $ toGenericList is
stateDown (LevelItems fs f is) =
  let i = snd $ fromJust $ listSelectedElement is
  in LevelContents fs f is i
stateDown s@(LevelContents _ _ _ _) = s

stateUp :: State -> State
stateUp s@(LevelFeeds _) = s
stateUp (LevelItems fs _ _) = LevelFeeds fs
stateUp (LevelContents fs f is _) = LevelItems fs f is

renderContents :: GenericItem -> Widget ()
renderContents (GenericItem {..}) =
  txtWrap $ T.unlines $ catMaybes
    [ ("Title: " <>) <$> giTitle
    , ("Link: " <>) <$> giURL
    , ("Author: " <>) <$> giAuthor
    , Just ""
    , giBody
    ]

renderItem :: Bool -> (GenericItem , ItemStatus)-> Widget ()
renderItem _ (GenericItem {..}, _) = padRight Max $ txt $ fromMaybe "*No Date*" giDate <> "  " <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle)

renderFeed :: Bool -> GenericFeed -> Widget ()
renderFeed _ (GenericFeed {..}) = padRight Max $ txt $ T.pack gfTitle <> " (" <> gfURL <> ")"

renderCache :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderCache b (_, Just (f, _)) = renderFeed b f
renderCache _ (s, Nothing) = padRight Max $ txt $ T.pack s <> " *Not Fetched*"

draw :: State -> [Widget ()]
draw s =
    case s of
      LevelFeeds fs -> g $ renderList renderCache True fs
      LevelItems _ _ is -> g $ renderList renderItem True is
      LevelContents _ _ _ (c, _) -> [f $ padBottom Max $ renderContents c]
  where
    f x = vBox [hCenter $ x, str "", hCenter $ str "Press Q to go back or quit"]
    g x = [vCenter $ f x]

handle :: State -> BrickEvent () () -> EventM () (Next State)
handle s (VtyEvent (EvKey (KChar 'r') _)) =
  case s of
    LevelFeeds fs -> do
      let (u, c) = snd $ fromJust $ listSelectedElement fs
      (f, is) <- liftIO $ fetchFeed u
      let c' = Just $ fromMaybe (newCacheEntry f is) $ fmap (updateCacheEntry f is) c
      continue $ LevelFeeds $ listModify (const (u, c')) fs
    _ -> continue s
handle s (VtyEvent (EvKey (KChar 'q') _)) =
  case s of
    LevelFeeds _ -> halt s
    _ -> continue $ stateUp s
handle s (VtyEvent (EvKey KEnter _)) = continue $ stateDown s
handle s (VtyEvent e) =
  case s of
    LevelFeeds fs -> do
      fs' <- handleListEventVi handleListEvent e fs
      continue $ LevelFeeds fs'
    LevelItems fs f is -> do
      is' <- handleListEventVi handleListEvent e is
      continue $ LevelItems fs f is'
    LevelContents _ _ _ _ -> continue s
handle s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (listAttr,            V.white `on` V.black)
    , (listSelectedAttr,    V.white `on` V.blue)
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

options :: Parser Options
options = Options
  <$> strOption
      ( long "urls"
     <> short 'u'
     <> help "A file with list of RSS feeds (compatible with newsboat)"
     <> metavar "FILE"
     <> value "~/.newsboat/urls"
     <> showDefault )
  <*> strOption
      ( long "cache"
     <> short 'c'
     <> help "A file in which to store cached RSS entries and read/unread status"
     <> metavar "FILE"
     <> value "/tmp/news-cache"
     <> showDefault )

parseOpts :: IO Options
parseOpts = execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Open TUI to fetch and display RSS feeds"
     <> header "news-view -- An RSS reader written in Haskell and inspired by newsboat" )

main :: IO ()
main = do
  Options {..} <- parseOpts
  urls <- parseFeedsConfig <$> readFile oUrls
  feeds <- refreshCacheFileWithUrls urls <$> readCacheFile oCache
  writeCacheFile oCache feeds
  let s = LevelFeeds (toGenericList feeds)
  pure () <* defaultMain app s
