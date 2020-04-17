{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module New where

import Data.List (splitAt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Data.Ord (comparing)

import Data.Functor.Compose (Compose(..))

import Control.Applicative ((<|>))
import Control.Lens
import Data.Foldable (Foldable(..))

import GenericFeed

import Brick.Widgets.List hiding (reverse)

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

newListState :: Maybe Int -> [a] -> L a
newListState idx x = list () x 1 & listSelectedL .~ idx

data Zipper a = Zipper [a] a [a]

focus :: Lens' (Zipper a) a
focus f (Zipper before x after) = (\y -> Zipper before y after) <$> f x

type MZipper a = Compose Maybe Zipper a

compose :: Lens' (Compose a b c) (a (b c))
compose f (Compose x) = Compose <$> f x

mFocus :: Traversal' (MZipper a) a
mFocus = compose . _Just . focus

mZEmpty :: MZipper a
mZEmpty = Compose Nothing

mZipper :: Zipper a -> MZipper a
mZipper = Compose . Just

zipLeft :: Zipper a -> Zipper a
zipLeft (Zipper [] x r) = Zipper [] x r
zipLeft (Zipper (x':l) x r) = Zipper l x' (x:r)

zipRight :: Zipper a -> Zipper a
zipRight (Zipper l x []) = Zipper l x []
zipRight (Zipper l x (x':r)) = Zipper (x:l) x' r

zipSetIndex :: Int -> Zipper a -> Zipper a
zipSetIndex idx z@(Zipper l _ _) = case compare idx le of
    LT -> repeatN (le - idx) zipLeft z
    EQ -> z
    GT -> repeatN (idx - le) zipRight z
  where
    le = length l
    repeatN 0 _ a = a
    repeatN i f a = repeatN (pred i) f $ f a

instance Foldable Zipper where
  foldr f a (Zipper l x r) = foldr f a $ Prelude.reverse l ++ x : r

instance Functor Zipper where
  fmap f (Zipper bef x aft) = Zipper (f <$> bef) (f x) (f <$> aft)

instance Traversable Zipper where
  traverse f (Zipper bef x aft) = Zipper <$> traverse f (reverse bef) <*> f x <*> traverse f aft

fromList :: [a] -> MZipper a
fromList [] = mZEmpty
fromList (x:xs) = mZipper $ Zipper [] x xs

data LevelItems = LevelItems
  { _liBefore :: [(FilePath, Maybe CacheEntry)]
  , _liAfter :: [(FilePath, Maybe CacheEntry)]
  , _liUrl :: FilePath
  , _liFeed :: GenericFeed
  , _liItems :: MZipper (GenericItem, ItemStatus)
  }

makeLenses ''LevelItems

data MenuState = MenuFeeds (MZipper (FilePath, Maybe CacheEntry))
               | MenuItems Bool       -- Is the selected item open?
                           LevelItems -- The Zipper state

-- Go one level up in the menu
menuUp :: MenuState -> MenuState
menuUp (MenuFeeds x) = MenuFeeds x
menuUp (MenuItems False (LevelItems {..})) = MenuFeeds $ mZipper $ Zipper _liBefore (_liUrl, Just (_liFeed, toList _liItems)) _liAfter
menuUp (MenuItems True x) = MenuItems False x

-- Go one level down in the menu
menuDown :: MenuState -> MenuState
menuDown x@(MenuFeeds (Compose Nothing)) = x
menuDown x@(MenuFeeds (Compose (Just (Zipper _ (_, Nothing) _)))) = x
menuDown (MenuFeeds (Compose (Just (Zipper bef (url, Just (feed, is)) aft)))) = MenuItems False $ LevelItems bef aft url feed $ fromList is
menuDown (MenuItems _ is) = MenuItems True is

menuFromCache :: CacheFile -> MenuState
menuFromCache = MenuFeeds . fromList

menuToCache :: MenuState -> CacheFile
menuToCache s = case menuUp $ menuUp s of
  MenuFeeds z -> toList z
  _ -> error "`menuUp . menuUp` didn't lift to top level menu"

-- Inside each zipper there is a list with index
listState :: Lens' (MZipper a) (L a)
listState f (Compose Nothing) = const (Compose Nothing) <$> f (newListState Nothing [])
listState f (Compose (Just z@(Zipper l _ _))) = fmap (mZipper . foo) $ f $ newListState (Just idx) (toList z)
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

appendNewItems :: FilePath -> (GenericFeed, [GenericItem]) -> MenuState -> MenuState
appendNewItems = undefined
