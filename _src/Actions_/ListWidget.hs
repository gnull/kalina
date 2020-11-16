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

applyFitered :: (a -> Bool -> Bool)              -- filter expression
             -> (MZipper a -> EventM () (MZipper a)) -- modifying function, it's allowed only to move the cursor
             -> MZipper a -> EventM () (MZipper a)
applyFitered p f z = do
  d <- delta
  pure $ case compare d 0 of
    LT -> iterate (over compose $ fmap $ zipLeftUntil $ flip p False) z !! (-d)
    EQ -> z
    GT -> iterate (over compose $ fmap $ zipRightUntil $ flip p False) z !! d
  where
    z' = zipFilter p z
    delta = do
      fz' <- f z'
      pure $ case (zipIndexMaybe $ fz', zipIndexMaybe z') of
        (Just a, Just b) -> a - b
        (Nothing, Nothing) -> 0
        (_, _) -> error "Your function messed with Zipper contents!"

handleEvent :: ((String, Maybe CacheEntry) -> Bool -> Bool) -- which feeds should be visible?
            -> ((GenericItem, ItemStatus) -> Bool -> Bool)  -- which items should be visible?
            -> (forall e. GenericList () [] e -> EventM () (GenericList () [] e)) -- list event handler to apply
            -> MenuAction
handleEvent pFeed pItem act = do
    (State cs s) <- get
    let (FilterPrefs {..}) = view filterPrefs cs
    case s of
      Left fs -> do
        fs' <- lift $ lift $ applyFitered pFeed (glistOverZipper act) fs
        lift $ lift $ continue $ State cs $ Left fs'
      Right is -> do
        is' <- lift $ lift $ liItems (applyFitered pItem $ glistOverZipper act) is
        lift $ lift $ continue $ State cs $ Right is'
  where
    glistOverZipper :: (GenericList () [] e -> EventM () (GenericList () [] e)) -> MZipper e -> EventM () (MZipper e)
    glistOverZipper _  (Compose Nothing) = pure $ Compose Nothing
    glistOverZipper ac (Compose (Just z)) = do
        let i = Just $ zipIndex z
            l = list () (toList z) 1 & listSelectedL .~ i
        i' <- ac l <&> (^. listSelectedL)
        case i' of
          Just i'' -> pure $ Compose $ Just $ zipSetIndex i'' z
          Nothing -> error "Action ate the list index! (this should be impossible)"
