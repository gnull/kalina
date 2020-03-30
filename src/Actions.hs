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

toggleShowRead :: Action
toggleShowRead st = continue $ case st ^. menuState of
  LevelFeeds _ -> over showUnreadFeeds not st
  LevelItems _ _ -> over showUnreadItems not st
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
