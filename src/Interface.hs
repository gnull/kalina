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

import Text.Pandoc (runPure, writePlain, readHtml, def, WriterOptions(writerWrapText), WrapOption(..))

import State
import State.Menu
import State.Fetch
import Interface.Actions

renderContents :: GenericItem -> Widget ()
renderContents (GenericItem {..}) =
    txtWrap $ T.unlines $ catMaybes
      [ ("Title: " <>) <$> giTitle
      , ("Link: " <>) <$> giURL
      , ("Author: " <>) <$> giAuthor
      , ("Date: " <>) <$> giDate
      , Just ""
      , f <$> giBody
      ]
  where
    settings = def { writerWrapText = WrapNone }
    f x = case runPure $ writePlain settings =<< readHtml def x of
      Left e -> T.pack $ show e
      Right b -> b

renderItem :: Bool -> (GenericItem , ItemStatus)-> Widget ()
renderItem _ (GenericItem {..}, r) = padRight Max $ markup
  $ (@? if r then "read-item" else "unread-item") $
      (if r then "   " else " N ")
   <> (fromMaybe "" giDate)
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
      FetchNothing -> " ? " @? "FetchNothing"
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
             <> "l" @? hl <> " — toggle display of read feed/items\n"
             <> "r/R" @? hl <> " — fetch current feed/all feeds\n"
             <> "o" @? hl <> " — open current item in browser\n"
           ]
  where
    hl = "hightlight"

drawMenu :: State -> Widget ()
drawMenu s =
    if s ^. displayHelp then
      helpWidget
    else
      case s ^. menuState of
        MenuFeeds z -> g $ renderList (renderFeed $ s ^. fetchState) True $ z ^. listState . listStateFilter (feedsFilterPredicate $ s ^. menuPrefs)
        MenuItems False is -> g $ renderList renderItem True $ is ^. liItems ^. listState ^. listStateFilter (itemsFilterPredicate $ s ^. menuPrefs)
        -- TODO: maybe I should split the True and False versions of MenuItems into different constructors to get
        -- rid of the fromJust on the next line. The True option must always have a non-empty zipper.
        MenuItems True is -> f $ padBottom Max $ renderContents $ fromJust $ is ^? (liItems . mFocus . _1)
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


draw :: State -> [Widget ()]
draw s = [drawMenu s]

handleMenu :: (FilePath -> IO ()) -> State -> Event -> EventM () (Next State)
handleMenu _ st@(State {_displayHelp = True}) (EvKey _ _) = toggleHelp st
handleMenu queue st (EvKey (KChar 'r') _) = fetchOne queue st
handleMenu queue st (EvKey (KChar 'R') _) = fetchAll queue st
handleMenu _ st (EvKey (KChar 'q') _) = back st
handleMenu _ st (EvKey KEnter _) = fmap touchListIdex <$> enter st
handleMenu _ st (EvKey (KChar 'l') _) = fmap touchListIdex <$> toggleShowRead st
handleMenu _ st (EvKey (KChar 'A') _) = markAsRead st
handleMenu _ st (EvKey (KChar 'o') _) = openCurrentUrl st
handleMenu _ st (EvKey (KChar '?') _) = toggleHelp st
-- We let the list widget handle all the other keys
handleMenu _ st e = continue =<< menuState f st
  where
    f s = case s of
      MenuFeeds z -> MenuFeeds <$> (listState . listStateFilter (feedsFilterPredicate $ st ^. menuPrefs)) (handleListEventVi handleListEvent e) z
      MenuItems False i -> MenuItems False <$> (liItems . listState . listStateFilter (itemsFilterPredicate $ st ^. menuPrefs)) (handleListEventVi handleListEvent e) i
      x@(MenuItems True _) -> pure x
