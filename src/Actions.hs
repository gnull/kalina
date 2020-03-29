module Actions
  ( Action
  , enter
  , back
  , fetchOne
  , fetchAll
  , toggleShowRead
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens
import Control.Arrow (second)

import Brick
import Menu

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
