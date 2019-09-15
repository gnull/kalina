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

toGenericList :: [a] -> MyList () a
toGenericList x = list () x 1

data MenuPosition
  = MenuFeeds
  | MenuItems (MyList () (GenericItem, ItemStatus))
  | MenuContents (MyList () (GenericItem, ItemStatus)) GenericItem

data State = State
  { sFeeds :: MyList () (String, Maybe CacheEntry)
  , sMenu  :: MenuPosition
  }

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
draw (State {..}) =
    case sMenu of
      MenuFeeds -> g $ renderList renderCache True sFeeds
      MenuItems is -> g $ renderList renderItem True is
      MenuContents _ c -> [f $ padBottom Max $ renderContents c]
  where
    f x = vBox [hCenter $ x, str "", hCenter $ str "Press Q to go back or quit"]
    g x = [vCenter $ f x]

handle :: State -> BrickEvent () () -> EventM () (Next State)
handle s@(State {..}) (VtyEvent (EvKey (KChar 'q') _)) =
  case sMenu of
    MenuFeeds -> halt s
    MenuItems _ -> continue $ s { sMenu = MenuFeeds }
    MenuContents is _ -> continue $ s { sMenu = MenuItems is }
handle s@(State {..}) (VtyEvent (EvKey KEnter _)) =
  case sMenu of
    MenuFeeds -> continue
      $ case snd $ snd $ fromJust $ listSelectedElement sFeeds of
          Nothing -> s
          Just (_, is) -> s { sMenu = MenuItems $ toGenericList is}
    MenuItems is -> continue
      $ s { sMenu = MenuContents is $ fst $ snd $ fromJust $ listSelectedElement is }
    MenuContents _ _ -> continue s
handle s@(State {..}) (VtyEvent e) =
  case sMenu of
    MenuFeeds -> do
      l <- handleListEventVi handleListEvent e sFeeds
      continue $ s { sFeeds = l }
    MenuItems is -> do
      l <- handleListEventVi handleListEvent e is
      continue $ s { sMenu = MenuItems l }
    MenuContents _ _ -> continue s
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
  let s = State (toGenericList feeds) MenuFeeds
  pure () <* defaultMain app s
