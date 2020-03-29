{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Data.Maybe
import Data.Text () -- Instances
import qualified Data.Text as T

import Control.Lens

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

import Menu
import Actions

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
   <> (fromMaybe "" giDate)
   <> "  "
   <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle)

renderFeed :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderFeed _ f = (txt " × ")
              <+> (hLimit 7 $ padRight Max $ markup $ unreadCount @? readStatus)
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

drawMenu :: State -> Widget ()
drawMenu s =
    case s ^. menuState of
      LevelFeeds fi -> g $ renderList renderFeed True $ (s ^. innerState, fi) ^. feedsListState (s ^. showUnreadFeeds)
      LevelItems fi is
        -> g $ renderList renderItem True
             $ (snd $ fromJust $ snd $ (s ^. innerState) !! fi, Just is) ^. itemsListState (s ^. showUnreadItems)
      LevelContents _ is -> f $ padBottom Max $ renderContents $ fromJust $ s ^? (selectedItem . _1)
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
        <+> str " u - toggle unread"
        <+> vBorder
        <+> str " h,j,k,l - navigation "]
    g x = vCenter $ f x

handleMenu :: (FilePath -> IO ()) -> State -> Event -> EventM () (Next State)
handleMenu queue st (EvKey (KChar 'r') _) = fetchOne queue st
handleMenu queue st (EvKey (KChar 'R') _) = fetchAll queue st
handleMenu _ st (EvKey (KChar 'q') _) = back st
handleMenu _ st (EvKey KEnter _) = enter st
handleMenu _ st (EvKey (KChar 'l') _) = toggleShowRead st
-- We let the list widget handle all the other keys
handleMenu _ st e = continue =<< fmap (\y -> set' menuState y st) x
  where
    s = st ^. menuState
    x = case s of
      LevelFeeds fs -> do
        (_, fs') <- feedsListState (st ^. showUnreadFeeds) (handleListEventVi handleListEvent e) (st ^. innerState, fs)
        pure $ LevelFeeds fs'
      LevelItems fs is -> do
        v <- itemsListState (st ^. showUnreadItems) (handleListEventVi handleListEvent e) (snd $ fromJust $ snd $ (st ^. innerState) !! fs, Just is)
        case v of
          (_, Just is') -> pure $ LevelItems fs is'
          (_, Nothing) -> error "Brick ate my index!"
      LevelContents _ _ -> pure s
