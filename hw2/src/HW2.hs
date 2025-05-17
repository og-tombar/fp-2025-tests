{-# LANGUAGE GHC2024 #-}
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, elem, error, even, filter, flip, foldl, foldr, fromIntegral, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------
-- Utilities from the lecture
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f = \case
  Nothing -> Nothing
  Just x -> Just $ f x

eitherMap :: (a -> b) -> Either e a -> Either e b
eitherMap f = \case
  Left x -> Left x
  Right y -> Right $ f y

-- Section 1.1: Basic Maybes
fromMaybe :: a -> Maybe a -> a
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
maybe :: b -> (a -> b) -> Maybe a -> b
maybeHead :: [a] -> Maybe a
maybeLast :: [a] -> Maybe a
maybeMaximum :: [Int] -> Maybe Int
maybeMinimum :: [Int] -> Maybe Int
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
sumMaybe :: Maybe Int -> Maybe Int -> Maybe Int
liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
catMaybes :: [Maybe a] -> [a]
mapMaybe :: (a -> Maybe b) -> [a] -> [b]

-- Section 1.2 Basic Eithers
fromEither :: b -> Either a b -> b
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
either :: (a -> c) -> (b -> c) -> Either a b -> c
mapLeft :: (a -> c) -> Either a b -> Either c b
catEithers :: [Either e a] -> Either e [a]
mapEither :: (a -> Either e b) -> [a] -> Either e [b]
partitionEithers :: [Either a b] -> ([a], [b])
eitherToMaybe :: Either a b -> Maybe b
productEither :: (Either a Int -> Either a Int -> Either a Int)
liftEither2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c

-- Section 2: Expressions
data Expr = Iden String | Lit Int | Plus Expr Expr | Minus Expr Expr | Mul Expr Expr | Div Expr Expr deriving (Show, Eq)

-- Adds parentheses around sub-expressions (the top level expression never has parentheses).
exprToString :: Expr -> String
-- Bonus (25 points): Same as the above, but without unnecessary parentheses
exprToString' :: Expr -> String
-- Returns Nothing on division by zero.
partialEvaluate :: [(String, Int)] -> Expr -> Maybe Expr
negateExpr :: Expr -> Expr
-- if the exponent is smaller than 0, this should return 0.
powerExpr :: Expr -> Int -> Expr
modExpr :: Expr -> Expr -> Expr

-- Section 3.1: zips and products
zip :: [a] -> [b] -> [(a, b)]
zipWithIndex :: [a] -> [(Int, a)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault :: a -> b -> [a] -> [b] -> [(a, b)] -- Zips two lists, filling missing elements with defaults
data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipEither :: [a] -> [b] -> Either ZipFail [(a, b)]
unzip :: [(a, b)] -> ([a], [b])
unzipFirst :: [(a, b)] -> [a]
unzipSecond :: [(a, b)] -> [b]
cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- Section 3.2: list functions
snoc :: [a] -> a -> [a] -- The opposite of cons!
take :: Int -> [a] -> [a]
-- The last element of the result (if non-empty) is the last element which satisfies the predicate
takeWhile :: (a -> Bool) -> [a] -> [a]
drop :: Int -> [a] -> [a]
-- The first element of the result (if non-empty) is the first element which doesn't satisfy the predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
slice :: Int -> Int -> [a] -> [a]
takeEvery :: Int -> [a] -> [a]
dropEvery :: Int -> [a] -> [a]
-- Removes *consecutive* repeats
nub :: [Int] -> [Int]
-- Removes *all* repeats, consecutive or not.
uniq :: [Int] -> [Int]

-- Section 3.3: base64
base64Chars :: String -- As defined in the PDF
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

toBase64 :: Integer -> String
fromBase64 :: String -> Maybe Integer
