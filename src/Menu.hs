{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}

module Menu where

import Data.Maybe
import Data.List
import Data.Text () -- Instances

import Control.Lens

import GenericFeed

import Brick.Widgets.List

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

toGenericList :: [a] -> L a
toGenericList x = list () x 1

data MenuState
  = LevelFeeds    (L (String, Maybe CacheEntry))
  | LevelItems    (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))
  | LevelContents (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))

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

data State = State { _innerState :: CacheFile, _menuState :: MenuState }

makeLenses ''State

activeItem :: Traversal' State (GenericItem, ItemStatus)
activeItem f s = case (s ^. menuState) ^? itemsListMenu of
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
                 pure $ set' innerState st $ over menuState (updateMenuState st) s
    Nothing -> pure s

initialState :: CacheFile -> State
initialState c = State { _innerState = c
                       , _menuState = LevelFeeds $ toGenericList c
                       }

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
