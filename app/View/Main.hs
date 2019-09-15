{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.Maybe
import Data.List
import Data.Text () -- Instances
import qualified Data.Text as T

import GenericFeed

import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V

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
renderItem _ (GenericItem {..}, _) = txt $ fromMaybe "*No Date*" giDate <> "  " <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle)

renderFeed :: Bool -> GenericFeed -> Widget ()
renderFeed _ (GenericFeed {..}) = txt $ T.pack gfTitle <> " (" <> gfURL <> ")"

renderCache :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderCache b (_, Just (f, _)) = renderFeed b f
renderCache _ (s, Nothing) = txt $ T.pack s <> " *Not Fetched*"

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

main :: IO ()
main = do
  [cFile] <- getArgs
  feeds <- (read :: String -> [(String, Maybe CacheEntry)]) <$> readFile cFile
  let s = LevelFeeds (toGenericList feeds)
  pure () <* defaultMain app s
