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

data MenuState
  = LevelFeeds    (L (String, Maybe CacheEntry))
  | LevelItems    (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))
  | LevelContents (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))

data State = State { _innerState :: CacheFile
                   , _menuState :: MenuState
                   , _showUnreadFeeds :: Bool
                   , _showUnreadItems :: Bool
                   }

makeLenses ''State

toGenericList :: [a] -> L a
toGenericList x = list () x 1

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

feedListMenu :: Lens' MenuState (L (String, Maybe CacheEntry))
feedListMenu f (LevelFeeds fs) = LevelFeeds <$> f fs
feedListMenu f (LevelItems fs is) = (\x -> LevelItems x is) <$> f fs
feedListMenu f (LevelContents fs is) = (\x -> LevelContents x is) <$> f fs

-- This allows to modify the `is' field of MenuState. If the field is not
-- present, nothing happens. But it doesn't allow reading the field, since it
-- may not be present.
itemsListMenu :: Traversal' MenuState (L (GenericItem, ItemStatus))
itemsListMenu _ s@(LevelFeeds _) = pure s
itemsListMenu f (LevelItems fs is) = LevelItems fs <$> f is
itemsListMenu f (LevelContents fs is) = LevelContents fs <$> f is

-- The item which is currently selected in the items menu (or open in the contents menu)
selectedItem :: Traversal' State (GenericItem, ItemStatus)
selectedItem f s = case (s ^. menuState) ^? itemsListMenu of
    Just x -> let i = x ^. (selectedElementL . _1)
                  u = s ^. (menuState . feedListMenu . selectedElementL . _1)
                  fu feed@(_, Nothing) = pure feed
                  fu (u', Just (gf, is)) = do
                    if u == u' then do
                      is' <- flip traverse is $ \(i', readStatus) ->
                        if i' == i then f (i', readStatus) else pure (i', readStatus)
                      pure (u', Just (gf, is'))
                    else
                      pure (u', Just (gf, is))
               in do
                 st <- traverse fu $ s ^. innerState
                 pure $ syncMenuState $ set' innerState st s
    Nothing -> pure s

-- As selectedItem, but works only if the item is open in the contents menu
activeItem :: Traversal' State (GenericItem, ItemStatus)
activeItem f s = case s ^. menuState of
  LevelContents _ _ -> selectedItem f s
  _ -> pure s

initialState :: CacheFile -> State
initialState c = State { _innerState = c
                       , _menuState = LevelFeeds $ toGenericList c
                       , _showUnreadFeeds = True
                       , _showUnreadItems = True
                       }

-- TODO: Use listFilter here, after adding a bool field to State
syncMenuState :: State -> State
syncMenuState s = over menuState (updateMenuState $ s ^. innerState) s

updateMenuState :: CacheFile -> MenuState -> MenuState
updateMenuState c ms = over itemsListMenu (patchGenList urlEqual (snd $ fromJust $ snd $ selectedElement $ ms' ^. feedListMenu)) ms'
  where
    ms' = over feedListMenu (patchGenList fstEqual c) ms
    -- These two are utility functions
    patchGenList :: (a -> a -> Bool) -> [a] -> L a -> L a
    patchGenList eq l' l = listReplace l' (Just pos) l
      where
        sel = selectedElement l
        pos = fromJust $ findIndex (eq sel) l'
    fstEqual :: Eq a => (a, b) -> (a, b) -> Bool
    fstEqual (x, _) (y, _) = x == y
    urlEqual :: (GenericItem, a) -> (GenericItem, a) -> Bool
    urlEqual (x, _) (y, _) = giURL x == giURL y

selectedElement :: L x -> x
selectedElement = snd . fromJust . listSelectedElement

selectedElementL :: Lens' (L x) x
selectedElementL f l = (\y -> listModify (const y) l) <$> f x
  where
    x = selectedElement l

-- TODO: Make these two functions work with State instead of MenuState

stateDown :: MenuState -> MenuState
stateDown s@(LevelFeeds fs) =
  let f = selectedElement fs
  in case snd f of
    Nothing -> s
    Just (_, is) -> LevelItems fs $ toGenericList is
stateDown (LevelItems fs is) = LevelContents fs is
stateDown s@(LevelContents _ _) = s

stateUp :: MenuState -> MenuState
stateUp s@(LevelFeeds _) = s
stateUp (LevelItems fs _) = LevelFeeds fs
stateUp (LevelContents fs is) = LevelItems fs is
