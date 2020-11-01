module MZipper_ where

import Data.Functor.Compose (Compose(..))

import Control.Lens
import Data.Foldable (Foldable(..))

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
    LT -> iterate zipLeft z !! (le - idx)
    EQ -> z
    GT -> iterate zipRight z !! (idx - le)
  where
    le = length l

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
