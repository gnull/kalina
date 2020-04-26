{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Data.Maybe
import Data.Text () -- Instances
import qualified Data.Text as T

import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Control.Lens

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

import Text.Pandoc (runPure, writePlain, readHtml, def, WriterOptions(writerWrapText), WrapOption(..))

import State
import State.Menu
import State.Fetch
import Interface.Actions

timeToText :: TimeZone -> MyTime -> T.Text
timeToText z = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" . utcToLocalTime z . getMyTime

renderContents :: TimeZone -> GenericItem -> Widget ()
renderContents z (GenericItem {..}) =
    txtWrap $ T.unlines $ catMaybes
      [ ("Title: " <>) <$> giTitle
      , ("Link: " <>) <$> giURL
      , ("Author: " <>) <$> giAuthor
      , ("Date: " <>) <$> timeToText z <$> giDate
      , Just ""
      , f <$> giBody
      ]
  where
    settings = def { writerWrapText = WrapNone }
    f x = case runPure $ writePlain settings =<< readHtml def x of
      Left e -> T.pack $ show e
      Right b -> b

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
        MenuFeeds z -> g $ renderList (renderFeed $ s ^. fetchState) True $ z ^. listState . listStateFilter (s ^. forceShowFeed) (feedsFilterPredicate $ s ^. menuPrefs)
        MenuItems False is -> g $ renderList (renderItem tz) True $ is ^. liItems ^. listState ^. listStateFilter (s ^. forceShowItem) (itemsFilterPredicate $ s ^. menuPrefs)
        -- TODO: maybe I should split the True and False versions of MenuItems into different constructors to get
        -- rid of the fromJust on the next line. The True option must always have a non-empty zipper.
        MenuItems True is -> f $ padBottom Max $ renderContents tz $ fromJust $ is ^? (liItems . mFocus . _1)
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
handleMenu queue st (EvKey (KChar 'r') _) = fetchOne (queueHelper queue $ st ^. fetchState) st
handleMenu queue st (EvKey (KChar 'R') _) = fetchAll (queueHelper queue $ st ^. fetchState) st
handleMenu _ st (EvKey (KChar 'q') _) = fmap forceShowSelected <$> back st
handleMenu _ st (EvKey KEnter _) = fmap cancelForceShowSelected <$> enter (touchListIdex $ st)
handleMenu _ st (EvKey (KChar 'l') _) = fmap cancelForceShowSelected <$> toggleShowRead st
handleMenu _ st (EvKey (KChar 'n') _) = fmap forceShowSelected <$> toggleReadItem st
handleMenu _ st (EvKey (KChar 'A') _) = fmap cancelForceShowSelected <$> markAsRead st
handleMenu _ st (EvKey (KChar 'o') _) = fmap forceShowSelected <$> openCurrentUrl st
handleMenu _ st (EvKey (KChar '?') _) = toggleHelp st
-- We let the list widget handle all the other keys
handleMenu _ st e = (fmap . fmap) cancelForceShowSelected $ continue =<< menuState f st
  where
    f s = case s of
      MenuFeeds z -> MenuFeeds <$> (listState . listStateFilter (st ^. forceShowFeed) (feedsFilterPredicate $ st ^. menuPrefs)) (handleListEventVi handleListEvent e) z
      MenuItems False i -> MenuItems False <$> (liItems . listState . listStateFilter (st ^. forceShowItem) (itemsFilterPredicate $ st ^. menuPrefs)) (handleListEventVi handleListEvent e) i
      x@(MenuItems True _) -> pure x
