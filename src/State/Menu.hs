{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module State.Menu where

import Data.List (splitAt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Data.Ord (comparing)

import Data.Functor.Compose (Compose(..))

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
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

maybePos :: MZipper a -> Maybe Int
maybePos (Compose (Just (Zipper bef _ _))) = Just $ length bef
maybePos (Compose Nothing) = Nothing

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

appendList :: [a] -> MZipper a -> MZipper a
appendList l (Compose Nothing) = fromList l
appendList l (Compose (Just (Zipper bef x aft))) = mZipper $ Zipper (bef ++ reverse l) x aft

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
listStateFilter :: Maybe Int -> (a -> Bool) -> Lens' (L a) (L a)
listStateFilter forceShow p f st = fmap g $ f $ newListState idx' l'
  where
    l = st ^. listElementsL
    ixMaybe = st ^. listSelectedL -- This should be used only when h ≠ []
    idx = fromJust ixMaybe
    h = filter (\(i, x) -> Just i == forceShow || p x) $ zip [0..] l
    l' = map snd h
    foo (_, x) = if x <= idx then Left $ idx - x else Right $ x - idx
    idx' = if null h then Nothing else Just $
      fst $ minimumBy (comparing foo) $ zip [0..] $ map fst h
    g st' = let origIx = fmap (\i -> fst $ h !! i) (st' ^. listSelectedL) <|> ixMaybe
             in newListState origIx l

-- A lens which looks at all the items of the currently selected feed
selectedFeedItems :: Traversal' MenuState (GenericItem, ItemStatus)
selectedFeedItems f (MenuFeeds z) = MenuFeeds <$> (mFocus . _2 . _Just . _2 . traverse) f z
selectedFeedItems f (MenuItems b is) = MenuItems b <$> (liItems . traverse) f is

-- This is similar to the previous one, but looks at only one selected item (if
-- one is selected).
selectedItem :: Traversal' MenuState (GenericItem, ItemStatus)
selectedItem _ (MenuFeeds z) = pure $ MenuFeeds z
selectedItem f (MenuItems b is) = MenuItems b <$> (liItems . mFocus) f is

type Getting' s a = Getting a s a

allFeeds :: Getting' MenuState (MZipper (FilePath, Maybe CacheEntry))
allFeeds f s = case menuUp $ menuUp s of
  MenuFeeds x -> Const $ getConst $ f x
  _ -> undefined

-- I'm 60% sure there should be a library function which does this. I should
-- check this
lensPair :: Lens' s a -> Lens' s b -> Lens' s (a, b)
lensPair la lb f s = f (a, b) <&> \(a', b') -> set la a' $ set lb b' s
  where
    a = s ^. la
    b = s ^. lb

-- TODO: This function is quite ugly, some code is duplicated. There should be
-- some simpler and shorter way to do this.
appendNewItems :: FilePath -> (GenericFeed, [GenericItem]) -> MenuState -> MenuState
appendNewItems u (f, is) s = case s of
    MenuFeeds z -> MenuFeeds $ fmap patchMaybe z
    MenuItems b i -> MenuItems b
      $ over liBefore (fmap patchMaybe)
      $ over liAfter (fmap patchMaybe)
      $ over (lensPair liUrl $ lensPair liFeed liItems) patch i
  where
    elemBy g l x = elem (g x) $ map g l
    remaining :: [GenericItem] -> [GenericItem] -> [GenericItem]
    remaining new old = filter (not . elemBy (giTitle &&& giURL) old) new

    patchMaybe :: (FilePath, Maybe (GenericFeed, [(GenericItem, ItemStatus)]))
               -> (FilePath, Maybe (GenericFeed, [(GenericItem, ItemStatus)]))
    patchMaybe (u', x) = if u' /= u then (u', x)
      else case x of
        Nothing -> (u', Just (f, zip is $ repeat False))
        Just (_, is') -> (u', Just (f, zip (remaining is $ map fst is') (repeat False) ++ is'))

    patch :: (FilePath, (GenericFeed, MZipper (GenericItem, ItemStatus)))
          -> (FilePath, (GenericFeed, MZipper (GenericItem, ItemStatus)))
    patch (u', (f', z)) = if u' /= u then (u', (f', z))
      else (u', (f, appendList (zip (remaining is $ map fst $ toList z) $ repeat False) z))
