{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Menu where

import Data.Maybe
import Data.List
import Data.Text () -- Instances
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

import Data.Text.Markup (fromText)

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

toGenericList :: [a] -> MyList () a
toGenericList x = list () x 1

data MenuState
  = LevelFeeds    (L (String, Maybe CacheEntry))
  | LevelItems    (L (String, Maybe CacheEntry)) (String, Maybe CacheEntry) (L (GenericItem, ItemStatus))
  | LevelContents (L (String, Maybe CacheEntry)) (String, Maybe CacheEntry) (L (GenericItem, ItemStatus)) (GenericItem, ItemStatus)

initialMenuState :: CacheFile -> MenuState
initialMenuState = LevelFeeds . toGenericList

stateDown :: MenuState -> MenuState
stateDown s@(LevelFeeds fs) =
  let f = snd $ fromJust $ listSelectedElement fs
  in case snd f of
    Nothing -> s
    Just (_, is) -> LevelItems fs f $ toGenericList is
stateDown (LevelItems fs f is) =
  let i = snd $ fromJust $ listSelectedElement is
  in LevelContents fs f is i
stateDown s@(LevelContents _ _ _ _) = s

stateUp :: MenuState -> MenuState
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
renderItem _ (GenericItem {..}, r) = padRight Max $ markup
  $   (fromText $ fromMaybe "*No Date*" giDate)
   <> fromText "  "
   <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle) @? if r then "read-item" else "unread-item"

renderFeed :: Bool -> GenericFeed -> Widget ()
renderFeed _ (GenericFeed {..}) = padRight Max $ txt $ T.pack gfTitle <> " (" <> gfURL <> ")"

renderCache :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderCache b (_, Just (f, _)) = renderFeed b f
renderCache _ (s, Nothing) = padRight Max $ txt $ T.pack s <> " *Not Fetched*"

drawMenu :: MenuState -> Widget ()
drawMenu s =
    case s of
      LevelFeeds fs -> g $ renderList renderCache True fs
      LevelItems _ _ is -> g $ renderList renderItem True is
      LevelContents _ _ _ (c, _) -> f $ padBottom Max $ renderContents c
  where
    f x = vBox
      [x
      , str ""
      , vLimit 3 $ borderWithLabel (str "help") $
            str " q - back/quit "
        <+> vBorder
        <+> str " r - fetch selected feed "
        <+> vBorder
        <+> str " Enter - open an entry "
        <+> vBorder
        <+> str " h,j,k,l - navigation "]
    g x = vCenter $ f x

handleMenu :: MenuState -> Event -> EventM () (Next MenuState)
handleMenu s (EvKey (KChar 'r') _) =
  case s of
    LevelFeeds fs -> do
      let (u, c) = snd $ fromJust $ listSelectedElement fs
      (f, is) <- liftIO $ fetchFeed u
      let c' = Just $ fromMaybe (newCacheEntry f is) $ fmap (updateCacheEntry f is) c
      continue $ LevelFeeds $ listModify (const (u, c')) fs
    _ -> continue s
handleMenu s (EvKey (KChar 'q') _) =
  case s of
    LevelFeeds _ -> halt s
    _ -> continue $ stateUp s
handleMenu s (EvKey KEnter _) = continue $ stateDown s
handleMenu s e =
  case s of
    LevelFeeds fs -> do
      fs' <- handleListEventVi handleListEvent e fs
      continue $ LevelFeeds fs'
    LevelItems fs f is -> do
      is' <- handleListEventVi handleListEvent e is
      continue $ LevelItems fs f is'
    LevelContents _ _ _ _ -> continue s
