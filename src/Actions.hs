module Actions
  ( Action
  , enter
  , back
  , fetchOne
  , fetchAll
  , toggleShowRead
  , markAsRead
  , openCurrentUrl
  , toggleHelp
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens
import Control.Arrow (second)

import Data.Text (unpack)

import System.Process (rawSystem)

import Brick
import Menu
import GenericFeed

import New

type Action = State -> EventM () (Next State)

-- I'm not sure is this thing obeys Lens laws. I just couldn't compile my code
-- without it.
--
-- Basically, it says that inside each (outer) s there's an (inner) s, to which
-- you can apply an action which returns (). And after that, the outer s will
-- remain s.
preservingResult :: Lens s s s ()
preservingResult f x = const x <$> f x

enter :: Action
enter st = continue
  -- the order of two "over"s here is important
  $ over menuState menuDown
  $ over (menuState . selectedItem) (second $ const True) st

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
toggleShowRead st = undefined
-- toggleShowRead st = continue $ case st ^. menuState of
--     LevelFeeds fs -> let
--         st' = over showUnreadFeeds not st
--         (_, fs') = over (feedsListState $ st' ^. showUnreadFeeds) id (st' ^. innerState, fs)
--       in st' & menuState .~ LevelFeeds fs'
--     LevelItems fs is -> let
--         st' = over showUnreadItems not st
--         (_, Just is') = over (itemsListState $ st' ^. showUnreadItems) id (fromJust $ st' ^? (selectedFeed . itemsOfFeed), Just is)
--       in st' & menuState .~ LevelItems fs is'
--     LevelContents _ _ -> st

toggleHelp :: Action
toggleHelp st = continue $ over displayHelp not st

markAsRead :: Action
markAsRead st = continue $ over (menuState . selectedFeedItems) (second $ const True) st

openCurrentUrl :: Action
openCurrentUrl st = suspendAndResume $ set (menuState . selectedItem . _2) True <$> (menuState . selectedItem . _1 . giURLL . _Just . preservingResult) f st
  where
    f u = rawSystem "xdg-open" [unpack u] >> pure ()
