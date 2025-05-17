{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW3.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, head, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, show, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (/), (^), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.Enum (Bounded)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Set qualified as Set

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

treeSize :: Tree a -> Int
treeHeight :: Tree a -> Int
preOrderTraversal :: Tree a -> [a]
inOrderTraversal :: Tree a -> [a]
postOrderTraversal :: Tree a -> [a]
data Classification = Full | Complete | FullAndComplete | Perfect | Degenerate | Other deriving (Show, Eq)
classify :: Tree a -> Classification
isBalanced :: Tree a -> Bool

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

-- As defined in the PDf
sample :: InfiniteList a -> [a]
sample = sampleN 10
smallSample :: InfiniteList a -> [a]
smallSample = sampleN 5
sampleN :: Int -> InfiniteList a -> [a]
sampleN n = take n . itoList

itoList :: InfiniteList a -> [a]
iiterate :: (a -> a) -> a -> InfiniteList a
irepeat :: a -> InfiniteList a
naturals :: InfiniteList Integer
imap :: (a -> b) -> InfiniteList a -> InfiniteList b
iconcat :: InfiniteList [a] -> InfiniteList a
grouped :: Integer -> InfiniteList a -> InfiniteList [a]
reverseN :: Integer -> InfiniteList a -> InfiniteList a
sqrtInf :: Rational -> InfiniteList Rational
type InfiniteString = InfiniteList Char
longDivision :: Rational -> InfiniteString
sqrtStrings :: Rational -> InfiniteList InfiniteString

-- Section 3: Maze
data Cell = Open | Blocked | Treasure deriving (Show, Eq, Ord)
data Maze = Maze {width :: Int, height :: Int, layout :: [[Cell]]} deriving Show
cellAt :: Maze -> CellPosition -> Maybe Cell
data CellPosition = CellPosition {row :: Int, col :: Int} deriving (Show, Eq, Ord)

data Error = OutOfBounds | InvalidCell | NoPath deriving (Show, Eq, Ord)
getAvailableMoves :: Maze -> CellPosition -> Either Error [CellPosition]

-- From the lectures
data Queue a = Queue [a] [a] deriving (Show, Eq)
emptyQueue :: Queue a
emptyQueue = Queue [] []
enqueue :: a -> Queue a -> Queue a
enqueue a (Queue l r) = Queue (a : l) r
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue l []) = dequeue $ Queue [] (reverse l)
dequeue (Queue l (r : rs)) = Just (r, Queue l rs)

shortestPath :: Maze -> CellPosition -> CellPosition -> Either Error [CellPosition]
-- Bonus (15 points)
treasureHunt :: Maze -> CellPosition -> Either Error [CellPosition]
