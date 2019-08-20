{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.Maybe
import Data.List
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

data State = State
  { sFeeds :: MyList () GenericFeed
  , sItems :: Maybe (MyList () GenericItem)
  }

renderItem :: Bool -> GenericItem -> Widget ()
renderItem _ (GenericItem {..}) = txt $ fromMaybe "*Empty*" giTitle

renderFeed :: Bool -> GenericFeed -> Widget ()
renderFeed _ (GenericFeed {..}) = txt $ T.pack gfTitle <> " (" <> gfURL <> ")"

draw :: State -> [Widget ()]
draw (State {..}) = [vCenter $ vBox [hCenter $ l, str "", hCenter $ str "Press Q to go back or quit"]]
  where
    l = case sItems of
      Nothing -> renderList renderFeed True sFeeds
      Just is -> renderList renderItem True is

handle :: State -> BrickEvent () () -> EventM () (Next State)
handle s@(State {..}) (VtyEvent (EvKey (KChar 'q') _)) =
  case sItems of
    Nothing -> halt s
    Just _ -> continue $ s { sItems = Nothing }
handle s@(State {..}) (VtyEvent (EvKey KEnter _)) =
  case sItems of
    Nothing -> continue
      $ s { sItems = Just $ toGenericList $ gfItems $ snd $ fromJust $ listSelectedElement sFeeds }
    Just _ -> continue s
handle s@(State {..}) (VtyEvent e) =
  case sItems of
    Nothing -> do
      l <- handleListEventVi handleListEvent e sFeeds
      continue $ s { sFeeds = l }
    Just is -> do
      l <- handleListEventVi handleListEvent e is
      continue $ s { sItems = Just l }
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
  feeds <- (read :: String -> [GenericFeed]) <$> readFile cFile
  let s = State (toGenericList feeds) Nothing
  pure () <* defaultMain app s
