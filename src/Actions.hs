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

import Data.Maybe (fromJust)
import Data.Text (unpack)

import System.Process (rawSystem)

import Brick
import Menu
import GenericFeed

type Action = State -> EventM () (Next State)

enter :: Action
enter st = continue
  $ over activeItem (second $ const True)
  $ stateDown st

back :: Action
back st =
    case s of
      LevelFeeds _ -> halt st
      _ -> continue $ stateUp st
  where
    s = st ^. menuState

fetchOne :: (FilePath -> IO ()) -> Action
fetchOne queue s = case s ^? selectedFeed of
  Nothing -> continue s
  Just (u, _) -> liftIO (queue u) >> continue s

fetchAll :: (FilePath -> IO ()) -> Action
fetchAll queue st = do
  liftIO $ sequence_ $ fmap (queue . fst) (st ^. innerState)
  continue st

-- In addition to simply inverting the internal Bool fields, this function also
-- "touches" the list indices — it applies id function to list widget state
-- through a lens to make sure indexes point to a visible element.
toggleShowRead :: Action
toggleShowRead st = continue $ case st ^. menuState of
    LevelFeeds fs -> let
        st' = over showUnreadFeeds not st
        (_, fs') = over (feedsListState $ st' ^. showUnreadFeeds) id (st' ^. innerState, fs)
      in st' & menuState .~ LevelFeeds fs'
    LevelItems fs is -> let
        st' = over showUnreadItems not st
        (_, Just is') = over (itemsListState $ st' ^. showUnreadItems) id (fromJust $ st' ^? (selectedFeed . itemsOfFeed), Just is)
      in st' & menuState .~ LevelItems fs is'
    LevelContents _ _ -> st

toggleHelp :: Action
toggleHelp st = continue $ over displayHelp not st

markAsRead :: Action
markAsRead st = continue $ over (selectedFeed . itemsOfFeed) (map $ second $ const True) st

openCurrentUrl :: Action
openCurrentUrl st = suspendAndResume (f >> pure (over selectedItem (second $ const True) st))
  where
    f = case st ^? selectedItem . _1 . giURLL . _Just of
      Just u -> rawSystem "xdg-open" [unpack u] >> pure ()
      Nothing -> pure ()
