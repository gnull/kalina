{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Menu where

import Data.Maybe
import Data.List
import Data.Ord (comparing)
import Data.Text () -- Instances

import Control.Lens

import GenericFeed

import Brick.Widgets.List

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

data MenuState -- The parameters are indices of selected items
  = LevelFeeds    (Maybe Int)
  | LevelItems    Int Int
  | LevelContents Int Int

data State = State { _innerState :: CacheFile
                   , _menuState :: MenuState
                   , _showUnreadFeeds :: Bool
                   , _showUnreadItems :: Bool
                   }

makeLenses ''State

toGenericList :: [a] -> L a
toGenericList x = list () x 1

-- This lens allows one to "look into" the generic list represented by a pair of
-- list and an index. All the changes made to the generic list are reflected in
-- the pair.
glist :: (a -> Bool) -> Bool -> Lens' ([a], Maybe Int) (L a)
glist p showAll f (l, i) = (f $ toGenericList newL & listSelectedL .~ newId) <&> \gl ->
    if null h then
      (l, Nothing)
    else let
        i' = fromMaybe 0 $ gl ^. listSelectedL
        i'' = fst $ h !! i'
      in (l, Just i'')
  where
    p' = if showAll then const True else p
    h = filter (p' . snd) $ zip [0..] l
    oldId = fromMaybe 0 i -- must be used only if h ≠ []
    newId = if null h then Nothing else Just $
      fst $ minimumBy (comparing $ \(_, x) -> abs (x - oldId)) $ zip [0..] $ map fst h
    newL = map snd h

feedsListState :: Bool -> Lens' (CacheFile, Maybe Int) (L (String, Maybe CacheEntry))
feedsListState = glist $ \(_, x) -> case x of
  Nothing -> False
  Just (_, []) -> False
  Just (_, _) -> True

itemsListState :: Bool -> Lens' ([(GenericItem, ItemStatus)], Maybe Int) (L (GenericItem, ItemStatus))
itemsListState = glist $ not . snd

listLens :: Int -> Lens' [a] a
listLens i f l = f x <&> \x' -> l1 ++ x' : l2
  where
    (l1, x:l2) = Data.List.splitAt i l

selectedFeed :: Traversal' State (String, Maybe (GenericFeed, [(GenericItem, ItemStatus)]))
selectedFeed f s = case s ^. menuState of
  LevelFeeds Nothing -> pure s
  LevelFeeds (Just i) -> innerState (listLens i f) s
  LevelItems    i _ -> innerState (listLens i f) s
  LevelContents i _ -> innerState (listLens i f) s

itemsOfFeed :: Traversal' (String, Maybe (GenericFeed, [(GenericItem, ItemStatus)])) [(GenericItem, ItemStatus)]
itemsOfFeed = _2 . _Just . _2

-- The item which is currently selected in the items menu (or open in the contents menu)
selectedItem :: Traversal' State (GenericItem, ItemStatus)
selectedItem f s = case s ^. menuState of
    LevelFeeds _ -> pure s
    LevelItems _ ii -> helper ii
    LevelContents _ ii -> helper ii
  where
    helper ii = let
        foo (u, Just (feed, items)) = listLens ii f items <&> \items' -> (u, Just (feed, items'))
        foo (_, Nothing) = error "Index ii points into a non-existent list"
      in selectedFeed foo s

-- As selectedItem, but works only if the item is open in the contents menu
activeItem :: Traversal' State (GenericItem, ItemStatus)
activeItem f s = case s ^. menuState of
  LevelContents _ _ -> selectedItem f s
  _ -> pure s

initialState :: CacheFile -> State
initialState c = State { _innerState = c
                       , _menuState = LevelFeeds $ if null c then Nothing else Just 0
                       , _showUnreadFeeds = True
                       , _showUnreadItems = True
                       }

-- TODO: Make these two functions work with State instead of MenuState

stateDown :: State -> State
stateDown s = case s ^. menuState of
  LevelFeeds Nothing -> s
  LevelFeeds (Just i) -> fromMaybe s $ do
    (_, gf) <- s ^? selectedFeed
    (_, is) <- gf
    _ <- listToMaybe is
    pure $ s & menuState .~ LevelItems i 0
  LevelItems i j -> s & menuState .~ LevelContents i j
  LevelContents _ _ -> s

stateUp :: State -> State
stateUp st = over menuState f st
  where
    f s@(LevelFeeds _) = s
    f (LevelItems fs _) = LevelFeeds (Just fs)
    f (LevelContents fs is) = LevelItems fs is
