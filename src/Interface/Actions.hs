{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Interface.Actions
  ( Action
  , enter
  , back
  , fetchOne
  , fetchAll
  , toggleShowRead
  , toggleReadItem
  , markAsRead
  , openCurrentInPager
  , openCurrentUrl
  , toggleHelp
  , timeToText
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens
import Control.Arrow (second)
import Control.Monad (void)
import Data.Maybe (catMaybes)

import Data.Time.LocalTime (TimeZone, utcToLocalTime, getCurrentTimeZone)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Pandoc (runPure, writePlain, readHtml, def, WriterOptions(writerWrapText), WrapOption(..))

import Data.Text (unpack, Text, pack)
import Data.Text as T

import System.Process (rawSystem)
import System.IO.Unsafe (unsafePerformIO)

import Brick

import State
import State.Menu
import GenericFeed

type Action = State -> EventM () (Next State)

-- I'm not sure is this thing obeys Lens laws. I just couldn't compile my code
-- without it.
--
-- Basically, it says that inside each (outer) s there's an (inner) s, to which
-- you can apply an action which returns (). And after that, the outer s will
-- remain s.
preservingResult :: Lens s s s ()
preservingResult f x = const x <$> f x

enter :: State -> State
enter st =
  -- the order of the "over" and "set" here is important
    over menuState menuDown
  $ set (menuState . selectedItem . _2) True st

back :: Action
back st = case st ^. menuState of
  MenuFeeds _ -> halt st
  _ -> continue $ over menuState menuUp st

fetchOne :: (FilePath -> IO ()) -> Action
fetchOne queue s = (mFocus . _1 . preservingResult) (liftIO . queue) (s ^. menuState ^. allFeeds) >> continue s

fetchAll :: (FilePath -> IO ()) -> Action
fetchAll queue s = (traverse . _1) (liftIO . queue) (s ^. menuState ^. allFeeds) >> continue s

-- In addition to simply inverting the internal Bool fields, this function also
-- "touches" the list indices — it applies id function to list widget state
-- through a lens to make sure indexes point to a visible element.
toggleShowRead :: Action
toggleShowRead st = continue $ case st ^. menuState of
    MenuFeeds _ -> over (filterPrefs . showUnreadFeeds) not st
    MenuItems _ -> over (filterPrefs . showUnreadItems) not st

toggleReadItem :: Action
toggleReadItem st = continue $ case st ^. menuState of
  MenuItems i -> set menuState (MenuItems $ over (liItems . mFocus) (second not) i) st
  MenuFeeds _ -> st

toggleHelp :: Action
toggleHelp st = continue $ over displayHelp not st

markAsRead :: Action
markAsRead st = continue $ over (menuState . selectedFeedItems) (second $ const True) st

timeToText :: TimeZone -> MyTime -> Text
timeToText z = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" . utcToLocalTime z . getMyTime

renderContents :: TimeZone -> GenericItem -> String
renderContents z (GenericItem {..}) =
    unpack $ T.unlines $ catMaybes
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

openCurrentInPager :: Action
openCurrentInPager st =
  suspendAndResume $ (menuState . selectedItem . _1 . preservingResult) (\i -> void $ rawSystem "sh" ["-c", "echo \"$1\" | less", "--", renderContents (unsafePerformIO getCurrentTimeZone) i]) st

openCurrentUrl :: Action
openCurrentUrl st = suspendAndResume $ set (menuState . selectedItem . _2) True <$> (menuState . selectedItem . _1 . giURLL . _Just . preservingResult) f st
  where
    f u = rawSystem "xdg-open" [unpack u] >> pure ()
