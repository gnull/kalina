module Menu where

import Data.Maybe
import Data.List
import Data.Text () -- Instances

import Data.Functor ((<&>))

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

data State = State { innerState :: CacheFile, menuState :: MenuState }

initialState :: CacheFile -> State
initialState c = State { innerState = c
                       , menuState = LevelFeeds $ toGenericList c
                       }

-- This allows to read or modify the `fs' field of MenuState.
fsListL :: Lens' MenuState (L (String, Maybe CacheEntry))
fsListL f (LevelFeeds fs) = LevelFeeds <$> f fs
fsListL f (LevelItems fs is) = (\x -> LevelItems x is) <$> f fs
fsListL f (LevelContents fs is) = (\x -> LevelContents x is) <$> f fs

-- This allows to modify the `is' field of MenuState. If the field is not
-- present, nothing happens. But it doesn't allow reading the field, since it
-- may not be present.
isListL :: Traversal' MenuState (L (GenericItem, ItemStatus))
isListL _ s@(LevelFeeds _) = pure s
isListL f (LevelItems fs is) = LevelItems fs <$> f is
isListL f (LevelContents fs is) = LevelContents fs <$> f is

updateMenuState :: CacheFile -> MenuState -> MenuState
updateMenuState c ms = over isListL (patchGenList fstEqual (snd $ fromJust $ snd $ selectedElement $ ms' ^. fsListL)) ms'
  where
    ms' = over fsListL (patchGenList fstEqual c) ms
    -- These two are utility functions
    patchGenList :: (a -> a -> Bool) -> [a] -> L a -> L a
    patchGenList eq l' l = listReplace l' (Just pos) l
      where
        sel = selectedElement l
        pos = fromJust $ findIndex (eq sel) l'
    fstEqual :: Eq a => (a, b) -> (a, b) -> Bool
    fstEqual (x, _) (y, _) = x == y

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
stateDown (LevelItems fs is) =
  let (i, _) = selectedElement is
      is' = listModify (const (i, True)) is
      fs' = listModify (\(u, c) -> (u, c <&> \(gf, _) -> (gf, listElements is'))) fs
      -- fs' = listModify (second $ (Just .) $ const $ listElements is') fs
  in LevelContents fs' is'
stateDown s@(LevelContents _ _) = s

stateUp :: MenuState -> MenuState
stateUp s@(LevelFeeds _) = s
stateUp (LevelItems fs _) = LevelFeeds fs
stateUp (LevelContents fs is) = LevelItems fs is
