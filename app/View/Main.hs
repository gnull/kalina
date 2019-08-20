{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.Bool
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

data State = State
  { sFeeds :: MyList () GenericFeed
  , sItems :: Maybe (MyList () GenericItem)
  }

renderFeed :: Bool -> GenericFeed -> Widget ()
renderFeed b (GenericFeed {..}) = txt $ bool "   " "-> " b <> T.pack gfTitle <> " (" <> gfURL <> ")"

draw :: State -> [Widget ()]
draw (State {..}) = [vCenter $ vBox [hCenter $ renderList renderFeed True sFeeds, str "", hCenter $ str "Press Q/Esc to quit"]]

-- EvKey (KChar 'q') []
handle :: State -> BrickEvent () () -> EventM () (Next State)
handle s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handle s (VtyEvent (EvKey KEsc _)) = halt s
handle s (VtyEvent e) = do
  l <- handleListEvent e (sFeeds s)
  continue $ s { sFeeds = l }
handle s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (listAttr,            V.white `on` V.blue)
    , (listSelectedAttr,    V.blue `on` V.white)
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
  let s = State (list () feeds 1) undefined
  pure () <* defaultMain app s
