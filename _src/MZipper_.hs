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

zipReverse :: Zipper a -> Zipper a
zipReverse (Zipper l x r) = Zipper r x l

zipLeft :: Zipper a -> Zipper a
zipLeft (Zipper [] x r) = Zipper [] x r
zipLeft (Zipper (x':l) x r) = Zipper l x' (x:r)

zipRight :: Zipper a -> Zipper a
zipRight = zipReverse . zipLeft . zipReverse

zipIndex :: Zipper a -> Int
zipIndex (Zipper l _ _) = length l

zipIndexMaybe :: MZipper a -> Maybe Int
zipIndexMaybe = fmap zipIndex . view compose

zipSetIndex :: Int -> Zipper a -> Zipper a
zipSetIndex idx z = case compare idx le of
    LT -> iterate zipLeft z !! (le - idx)
    EQ -> z
    GT -> iterate zipRight z !! (idx - le)
  where
    le = zipIndex z

instance Foldable Zipper where
  foldr f a (Zipper l x r) = foldr f a $ Prelude.reverse l ++ x : r

instance Functor Zipper where
  fmap f (Zipper bef x aft) = Zipper (f <$> bef) (f x) (f <$> aft)

instance Traversable Zipper where
  traverse f (Zipper bef x aft) = Zipper <$> traverse f (reverse bef) <*> f x <*> traverse f aft

zipFilter :: (a -> Bool -> Bool) -- which elements should we keep?
          -> MZipper a -> MZipper a
zipFilter _ (Compose Nothing) = mZEmpty
zipFilter p (Compose (Just (Zipper bef x aft))) =
    case (bef', p x True, aft') of
      (_, True, _) -> mZipper $ Zipper bef' x aft'
      ([], _, []) -> mZEmpty
      (y:rest, _, _) -> mZipper $ Zipper rest y aft'
      (_, _, y:rest) -> mZipper $ Zipper bef' y rest
  where
    bef' = filter (flip p False) bef
    aft' = filter (flip p False) aft

fromList :: [a] -> MZipper a
fromList [] = mZEmpty
fromList (x:xs) = mZipper $ Zipper [] x xs

appendList :: [a] -> MZipper a -> MZipper a
appendList l (Compose Nothing) = fromList l
appendList l (Compose (Just (Zipper bef x aft))) = mZipper $ Zipper (bef ++ reverse l) x aft
