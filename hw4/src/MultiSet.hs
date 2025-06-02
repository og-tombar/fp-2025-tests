{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MultiSet (MultiSet, empty, member, count, remove, insert, fromList, toList) where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import Data.Semigroup (Arg (..))
import Data.Set qualified as Set
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))

newtype MultiSet a = MultiSet {_getMultiset :: Set.Set (Arg a Int)}

empty :: MultiSet a

member :: Ord a => a -> MultiSet a -> Bool

-- | Returns the count of an element in the multiset, 0 if not present.
count :: Ord a => a -> MultiSet a -> Int

-- | Insert one occurrence of an element into the multiset.
insert :: Ord a => a -> MultiSet a -> MultiSet a

-- | Remove one occurrence of an element from the multiset.
remove :: Ord a => a -> MultiSet a -> MultiSet a

-- | Convert a list into a multiset.
fromList :: Ord a => [a] -> MultiSet a

-- | Convert a multiset into a list, including duplicates.
toList :: Ord a => MultiSet a -> [a]

instance Ord a => Eq (MultiSet a)
instance Show a => Show (MultiSet a)
instance Ord a => Semigroup (MultiSet a)
instance Ord a => Monoid (MultiSet a)
