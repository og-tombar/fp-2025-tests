{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Data.Either
import Data.List (foldl', sort, uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import MultiSet
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Foldable (foldMap, foldl, foldr), Fractional, Functor (fmap), Int, Maybe (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, error, filter, flip, fst, id, init, map, not, or, replicate, reverse, snd, take, takeWhile, uncurry, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (||), (/), fromIntegral, (<$>))

-- Section 1: Foldable functions
fold :: (Foldable t, Monoid a) => t a -> a
fold = undefined
toList :: Foldable t => t a -> [a]
toList = undefined
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = undefined
find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
find  = undefined
length :: Foldable t => t a -> Int
length = undefined
null :: Foldable t => t a -> Bool
null = undefined
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = undefined
maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maxBy = undefined
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = undefined
minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
minBy = undefined
sum :: (Foldable t, Num a) => t a -> a
sum  = undefined
product :: (Foldable t, Num a) => t a -> a
product = undefined
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap = undefined

-- Section 2: Composing folds
data Fold a b c = Fold (b -> a -> b) b (b -> c)

-- Instances + helpers

instance Functor (Fold a b) where

combine :: Fold a b c -> Fold a b' c' -> Fold a (b, b') (c, c')

combineWith :: (c -> c' -> d) -> Fold a b c -> Fold a b' c' -> Fold a (b, b') d

-- Execute a fold operation as a left fold.
runFold :: Foldable t => Fold a b c -> t a -> c

-- Will only fold over elements that satisfy a predicate
filterF :: (a -> Bool) -> Fold a b c -> Fold a b c

-- Applies a function to each elements before applying the step function
mapF :: (a -> a) -> Fold a b c -> Fold a b c -- Not to be confused with fmap!

nullF :: Fold a Bool Bool

findF :: (a -> Bool) -> Fold a (Maybe a) (Maybe a)

topKF :: Ord a => Int -> Fold a [a] [a]

-- Mathematical folds. Use combineWith to implement average from the basic ones!
sumF :: Num a => Fold a a a

productF :: Num a => Fold a a a

lengthF :: Fold a Int Int

averageF :: Fractional a => Fold a (a, Int) a



-- Section 3: Functor functions

fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)

fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)

strengthenL :: Functor f => b -> f a -> f (b, a)

strengthenR :: Functor f => b -> f a -> f (a, b)

unzip :: Functor f => f (a, b) -> (f a, f b)

coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)

-- Section 4: MultiSet Foldable instances
newtype FoldOccur a = FoldOccur {getFoldOccur :: MultiSet a}
instance Foldable FoldOccur

newtype MinToMax a = MinToMax {getMinToMax :: MultiSet a}
instance Foldable MinToMax

newtype MaxToMin a = MaxToMin {getMaxToMin :: MultiSet a}
instance Foldable MaxToMin

-- Bonus section

newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

instance Semigroup a => Semigroup (ZipList a)
instance Monoid a => Monoid (ZipList a)



-- Bonus (5 pt.): implement the varianceF Fold. This is slightly harder than average - the formula is
-- E[X^2] - E[x]^2, so you need to keep track of more than two quantities in your tuple. Use `combineWith` as needed.
varianceF :: Fractional a => Fold a ((a, a), Int) a
