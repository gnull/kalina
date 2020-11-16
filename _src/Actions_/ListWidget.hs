{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Actions_.ListWidget where

import Prelude hiding (filter, length)
import Data.Functor.Compose (Compose(..))
import Data.Foldable (Foldable(..))

import Control.Lens
-- import Control.Monad.Reader
import Control.Monad.State (get, lift)

import Brick (EventM, continue)
import Brick.Widgets.List (GenericList, list, listSelectedL)

import MZipper_
import Actions_
import GenericFeed
import State_

-- Zip left until the condition is satistied
-- (make sure such element exitst, otherwise this function will loop infinitely)
zipLeftUntil :: (a -> Bool) -> Zipper a -> Zipper a
zipLeftUntil p z | p $ z ^. focus = z'
                 | otherwise      = zipLeftUntil p z'
  where
    z' = zipLeft z

zipRightUntil :: (a -> Bool) -> Zipper a -> Zipper a
zipRightUntil p = zipReverse . zipLeftUntil p . zipReverse

-- TODO: Rewrite the next two functions to work with EventM actions

applyFitered :: (a -> Bool -> Bool)              -- filter expression
             -> (MZipper a -> MZipper a) -- modifying function, it's allowed only to move the cursor
             -> MZipper a -> MZipper a
applyFitered p f z = case compare delta 0 of
    LT -> iterate (over compose $ fmap $ zipLeftUntil $ flip p False) z !! (-delta)
    EQ -> z
    GT -> iterate (over compose $ fmap $ zipRightUntil $ flip p False) z !! delta
  where
    z' = zipFilter p z
    delta = case (zipIndexMaybe $ f z', zipIndexMaybe z') of
      (Just a, Just b) -> a - b
      (Nothing, Nothing) -> 0
      (_, _) -> error "Your function messed with Zipper contents!"

handleEvent :: ((String, Maybe CacheEntry) -> Bool -> Bool) -- which feeds should be visible?
            -> ((GenericItem, ItemStatus) -> Bool -> Bool)  -- which items should be visible?
            -> (forall e. GenericList () [] e -> GenericList () [] e) -- list event handler to apply
            -> MenuAction
handleEvent pFeed pItem act = do
    (State cs s) <- get
    let (FilterPrefs {..}) = view filterPrefs cs
    case s of
      Left fs -> do
        let fs' = applyFitered pFeed (glistOverZipper act) fs
        lift $ lift $ continue $ State cs $ Left fs'
      Right is -> do
        let is' = over liItems (applyFitered pItem $ glistOverZipper act) is
        lift $ lift $ continue $ State cs $ Right is'
  where
    glistOverZipper :: (GenericList () [] e -> GenericList () [] e) -> MZipper e -> MZipper e
    glistOverZipper _  (Compose Nothing) = Compose Nothing
    glistOverZipper ac (Compose (Just z)) = Compose $ Just $ let
        i = Just $ zipIndex z
        l = list () (toList z) 1 & listSelectedL .~ i
        Just i' = ac l ^. listSelectedL
      in zipSetIndex i' z
