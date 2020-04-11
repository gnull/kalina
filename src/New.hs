{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module New where

import Data.List (splitAt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Data.Ord (comparing)

import Control.Applicative ((<|>))
import Control.Lens
import Data.Foldable (Foldable(..))

import GenericFeed

import Brick.Widgets.List

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

newListState :: Maybe Int -> [a] -> L a
newListState idx x = list () x 1 & listSelectedL .~ idx

data Zipper a = ZEmpty
              | Zip [a] a [a]

zipLeft :: Zipper a -> Zipper a
zipLeft ZEmpty = ZEmpty
zipLeft (Zip [] x r) = Zip [] x r
zipLeft (Zip (x':l) x r) = Zip l x' (x:r)

zipRight :: Zipper a -> Zipper a
zipRight ZEmpty = ZEmpty
zipRight (Zip l x []) = Zip l x []
zipRight (Zip l x (x':r)) = Zip (x:l) x' r

zipSetIndex :: Int -> Zipper a -> Zipper a
zipSetIndex _ ZEmpty = ZEmpty
zipSetIndex idx z@(Zip l _ _) = case compare idx le of
    LT -> repeatN (le - idx) zipLeft z
    EQ -> z
    GT -> repeatN (idx - le) zipRight z
  where
    le = length l
    repeatN 0 _ a = a
    repeatN i f a = repeatN (pred i) f $ f a

instance Foldable Zipper where
  foldr f a ZEmpty = foldr f a []
  foldr f a (Zip l x r) = foldr f a $ Prelude.reverse l ++ x : r

fromList :: [a] -> Zipper a
fromList [] = ZEmpty
fromList (x:xs) = Zip [] x xs

-- TODO: all the instances and functions on Zipper

data LevelItems = LevelItems
  { liBefore :: [(FilePath, Maybe CacheEntry)]
  , liAfter :: [(FilePath, Maybe CacheEntry)]
  , liUrl :: FilePath
  , liFeed :: GenericFeed
  , liItems :: Zipper (GenericItem, ItemStatus)
  }

data MenuState = MenuFeeds (Zipper (FilePath, Maybe CacheEntry))
               | MenuItems Bool       -- Is the selected item open?
                           LevelItems -- The Zipper state

-- Go one level up in the menu
menuUp :: MenuState -> MenuState
menuUp (MenuFeeds x) = MenuFeeds x
menuUp (MenuItems False (LevelItems {..})) = MenuFeeds $ Zip liBefore (liUrl, Just (liFeed, toList liItems)) liAfter
menuUp (MenuItems True x) = MenuItems False x

menuFromCache :: CacheFile -> MenuState
menuFromCache = MenuFeeds . fromList

menuToCache :: MenuState -> CacheFile
menuToCache s = case menuUp $ menuUp s of
  MenuFeeds z -> toList z
  _ -> error "`menuUp . menuUp` didn't lift to top level menu"

-- Inside each zipper there is a list with index
listState :: Lens' (Zipper a) (L a)
listState f ZEmpty = const ZEmpty <$> f (newListState Nothing [])
listState f z@(Zip l _ _) = fmap foo $ f $ newListState (Just idx) (toList z)
  where
    idx = length l
    foo st = zipSetIndex (fromMaybe idx $ st ^. listSelectedL) z

-- If we have a filter function, we can pretend there's a filtered version of
-- any list inside it
listStateFilter :: (a -> Bool) -> Lens' (L a) (L a)
listStateFilter p f st = fmap g $ f $ newListState idx' l'
  where
    l = st ^. listElementsL
    ixMaybe = st ^. listSelectedL -- This should be used only when h ≠ []
    idx = fromJust ixMaybe
    h = filter (p . snd) $ zip [0..] l
    l' = map snd h
    foo (_, x) = if x <= idx then Left $ idx - x else Right $ x - idx
    idx' = if null h then Nothing else Just $
      fst $ minimumBy (comparing foo) $ zip [0..] $ map fst h
    g st' = let origIx = fmap (\i -> fst $ h !! i) (st' ^. listSelectedL) <|> ixMaybe
             in newListState origIx l
