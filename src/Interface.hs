{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Data.Maybe
import Data.Text () -- Instances
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))

import Control.Lens
import Control.Arrow (second)

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

import Menu

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

renderFeed :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderFeed _ f = (txt " × ")
              <+> (hLimit 6 $ padRight Max $ markup $ unreadCount @? readStatus)
              <+> vLimit 1 vBorder
              <+> (padRight Max $ markup $ (" " <> caption) @? readStatus)
  where
    (unread, total, caption) = case f of
      (_, Just (gf, is)) -> ( length $ filter (not . snd) is
                            , length is
                            , gfTitle gf <> " (" <> gfURL gf <> ")")
      (u, Nothing) -> (0, 0, T.pack u)
    readStatus = if unread == 0 then "read-item" else "unread-item"
    unreadCount = T.pack $ show unread <> "/" <> show total

drawMenu :: MenuState -> Widget ()
drawMenu s =
    case s of
      LevelFeeds fs -> g $ renderList renderFeed True fs
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

handleMenu :: (FilePath -> IO ()) -> State -> Event -> EventM () (Next State)
handleMenu queue st@(State _ s) (EvKey (KChar 'r') _) = continue =<< fmap (\y -> st {_menuState = y}) x
  where
    x = do
      let (u, _) = selectedElement fs
      liftIO $ queue u
      pure s
    fs = case s of
      LevelFeeds fs' -> fs'
      LevelItems fs' _ -> fs'
      LevelContents fs' _ -> fs'
handleMenu queue st@(State c _) (EvKey (KChar 'R') _) = do
  liftIO $ sequence_ $ fmap (queue . fst) c
  continue st
handleMenu _ st@(State _ s) (EvKey (KChar 'q') _) =
  case s of
    LevelFeeds _ -> halt st
    _ -> continue $ st {_menuState = stateUp s}
handleMenu _ st (EvKey KEnter _) = continue $ over activeItem (second $ const True) $ over menuState stateDown st
handleMenu _ st@(State _ s) e = continue =<< fmap (\y -> st {_menuState = y}) x
  where
    x = case s of
      LevelFeeds fs -> do
        fs' <- handleListEventVi handleListEvent e fs
        pure $ LevelFeeds fs'
      LevelItems fs is -> do
        is' <- handleListEventVi handleListEvent e is
        pure $ LevelItems fs is'
      LevelContents _ _ -> pure s
