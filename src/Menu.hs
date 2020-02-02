{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Menu where

import Data.Maybe
import Data.List
import Data.Text () -- Instances
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))

import Data.Functor ((<&>))

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

toGenericList :: [a] -> MyList () a
toGenericList x = list () x 1

data MenuState
  = LevelFeeds    (L (String, Maybe CacheEntry))
  | LevelItems    (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))
  | LevelContents (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))

initialMenuState :: CacheFile -> MenuState
initialMenuState = LevelFeeds . toGenericList

selectedElement :: L x -> x
selectedElement = snd . fromJust . listSelectedElement

stateDown :: MenuState -> MenuState
stateDown s@(LevelFeeds fs) =
  let f = selectedElement fs
  in case snd f of
    Nothing -> s
    Just (_, is) -> LevelItems fs $ toGenericList is
stateDown (LevelItems fs is) =
  let (i, _) = selectedElement is
      x' = (i, False)
      is' = listModify (const (i, True)) is
      fs' = listModify (\(u, c) -> (u, c <&> \(gf, _) -> (gf, listElements is'))) fs
      -- fs' = listModify (second $ (Just .) $ const $ listElements is') fs
  in LevelContents fs' is'
stateDown s@(LevelContents _ _) = s

stateUp :: MenuState -> MenuState
stateUp s@(LevelFeeds _) = s
stateUp (LevelItems fs _) = LevelFeeds fs
stateUp (LevelContents fs is) = LevelItems fs is

renderContents :: GenericItem -> Widget ()
renderContents (GenericItem {..}) =
  txtWrap $ T.unlines $ catMaybes
    [ ("Title: " <>) <$> giTitle
    , ("Link: " <>) <$> giURL
    , ("Author: " <>) <$> giAuthor
    , ("Date: " <>) <$> giDate
    , Just ""
    , giBody
    ]

renderItem :: Bool -> (GenericItem , ItemStatus)-> Widget ()
renderItem _ (GenericItem {..}, r) = padRight Max $ markup
  $ (@? if r then "read-item" else "unread-item") $
      (if r then "   " else " N ")
   <> (fromMaybe "*No Date*" giDate)
   <> "  "
   <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle)

renderFeed :: Bool -> GenericFeed -> Widget ()
renderFeed _ (GenericFeed {..}) = padRight Max $ txt $ gfTitle <> " (" <> gfURL <> ")"

renderCache :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderCache b (_, Just (f, _)) = renderFeed b f
renderCache _ (s, Nothing) = padRight Max $ txt $ T.pack s <> " *Not Fetched*"

drawMenu :: MenuState -> Widget ()
drawMenu s =
    case s of
      LevelFeeds fs -> g $ renderList renderCache True fs
      LevelItems _ is -> g $ renderList renderItem True is
      LevelContents _ is -> f $ padBottom Max $ renderContents (fst $ selectedElement is)
  where
    f x = vBox
      [x
      , str ""
      , vLimit 3 $ borderWithLabel (str "help") $
            str " q - back/quit "
        <+> vBorder
        <+> str " r - fetch selected feed "
        <+> vBorder
        <+> str " R - fetch all feeds "
        <+> vBorder
        <+> str " Enter - open an entry "
        <+> vBorder
        <+> str " h,j,k,l - navigation "]
    g x = vCenter $ f x

handleMenu :: (FilePath -> IO ()) -> MenuState -> Event -> EventM () (Next MenuState)
handleMenu queue s (EvKey (KChar 'r') _) =
  case s of
    LevelFeeds fs -> do
      let (u, _) = selectedElement fs
      liftIO $ queue u
      continue s
    _ -> continue s
handleMenu queue s (EvKey (KChar 'R') _) =
  case s of
    LevelFeeds fs -> do
      liftIO $ sequence_ $ fmap (queue . fst) fs
      continue s
    _ -> continue s
handleMenu _ s (EvKey (KChar 'q') _) =
  case s of
    LevelFeeds _ -> halt s
    _ -> continue $ stateUp s
handleMenu _ s (EvKey KEnter _) = continue $ stateDown s
handleMenu _ s e =
  case s of
    LevelFeeds fs -> do
      fs' <- handleListEventVi handleListEvent e fs
      continue $ LevelFeeds fs'
    LevelItems fs is -> do
      is' <- handleListEventVi handleListEvent e is
      continue $ LevelItems fs is'
    LevelContents _ _ -> continue s
