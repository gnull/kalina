{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Data.Maybe
import Data.Text () -- Instances
import qualified Data.Text as T

import Data.Time.LocalTime (TimeZone)

import Control.Lens

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

import State
import State.Menu
import State.Fetch
import Interface.Actions

renderItem :: TimeZone -> Bool -> (GenericItem , ItemStatus)-> Widget ()
renderItem z _ (GenericItem {..}, r) = padRight Max $ markup
  $ (@? if r then "read-item" else "unread-item") $
      (if r then "   " else " N ")
   <> (fromMaybe "" $ timeToText z <$> giDate)
   <> "  "
   <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle)

renderFeed :: FetchState -> Bool -> (String, Maybe CacheEntry) -> Widget ()
renderFeed fs _ (u, f) = statusIcon
              <+> (hLimit 7 $ padRight Max $ markup $ unreadCount @? readStatus)
              <+> vLimit 1 vBorder
              <+> (padRight Max $ markup $ (" " <> caption) @? readStatus)
  where
    (unread, total, caption) = case f of
      Just (gf, is) -> ( length $ filter (not . snd) is
                       , length is
                       , gfTitle gf <> " (" <> gfURL gf <> ")")
      Nothing -> (0, 0, T.pack u)
    readStatus = if unread == 0 then "read-item" else "unread-item"
    unreadCount = T.pack $ show unread <> "/" <> show total
    statusIcon = markup $ case fetchLookup u fs of
      FetchNothing -> " · " @? "FetchNothing"
      FetchStarted -> " ○ " @? "FetchStarted"
      FetchFailed -> " × " @? "FetchFailed"
      FetchOK -> " ● " @? "FetchOK"

helpWidget :: Widget ()
helpWidget = vBox
           [ withAttr "title" $ padRight Max $ str "Help"
           , hCenter $ markup $
                "j,k" @? hl <> " — navigation\n"
             <> "Enter" @? hl <> " — open a menu item\n"
             <> "q" @? hl <> " — back/quit\n"
             <> "A" @? hl <> " — mark current feed as read\n"
             <> "n" @? hl <> " — toggle read status of the selected item\n"
             <> "l" @? hl <> " — toggle display of read feed/items\n"
             <> "r/R" @? hl <> " — fetch current feed/all feeds\n"
             <> "o" @? hl <> " — open current item in browser\n"
           ]
  where
    hl = "hightlight"

drawMenu :: TimeZone -> State -> Widget ()
drawMenu tz s =
    if s ^. displayHelp then
      helpWidget
    else
      case s ^. menuState of
        MenuFeeds z -> g $ renderList (renderFeed $ s ^. fetchState) True $ z ^. listState . listStateFilter (feedsFilterPredicate $ s ^. filterPrefs)
        MenuItems is -> g $ renderList (renderItem tz) True $ is ^. liItems ^. listState ^. listStateFilter (itemsFilterPredicate $ s ^. filterPrefs)
  where
    f x = vBox
      [ withAttr "title" $ padRight Max $ str "Title"
      , x
      , helpLine
      ]
    g x = vCenter $ f x

helpLine :: Widget ()
helpLine = border $ vLimit 1 $ padRight Max $
      str " q - back/quit "
  <+> vBorder
  <+> str " ? - help "


draw :: TimeZone -> State -> [Widget ()]
draw z s = [drawMenu z s]

queueHelper :: (FilePath -> IO ()) -> FetchState -> FilePath -> IO ()
queueHelper q s u = if fetchLookup u s == FetchStarted
  then pure ()
  else q u

handleMenu :: (FilePath -> IO ()) -> State -> Event -> EventM () (Next State)
handleMenu _ st@(State {_displayHelp = True}) (EvKey _ _) = toggleHelp st
handleMenu queue st e@(EvKey k _) = case k of
  (KChar 'r') -> fetchOne (queueHelper queue $ st ^. fetchState) st
  (KChar 'R') -> fetchAll (queueHelper queue $ st ^. fetchState) st
  (KChar 'q') -> back st
  KEnter      -> fmap (touchListIdex . enter) <$> openCurrentInPager st
  (KChar 'l') -> fmap touchListIdex <$> toggleShowRead st
  (KChar 'n') -> toggleReadItem st
  (KChar 'A') -> markAsRead st
  (KChar 'o') -> openCurrentUrl st
  (KChar '?') -> toggleHelp st
  _           -> let -- let one of the lists handle the key
      f s = case s of
         MenuFeeds z -> MenuFeeds <$>
           (listState . listStateFilter (feedsFilterPredicate $ st ^. filterPrefs))
           (handleListEventVi handleListEvent e) z
         MenuItems i -> MenuItems <$>
           (liItems . listState . listStateFilter (itemsFilterPredicate $ st ^. filterPrefs))
           (handleListEventVi handleListEvent e) i
     in continue =<< menuState f st
handleMenu _ st _ = continue st -- ignore mouse, resize, etc. events
