module Main where

import Data.List (find, foldl')
import HW2
import Test.HUnit
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), IO, Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, elem, error, even, filter, flip, foldl, foldr, fromIntegral, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

infiniteRepeats :: [Int]
infiniteRepeats = [tripleX | x <- [1 ..], tripleX <- [x, x, x]]

tests :: Test
tests =
  TestList
    [ TestLabel "fromMaybe 1 Nothing" $
        TestCase $
          assertEqual "fromMaybe 1 Nothing" (1 :: Integer) (fromMaybe 1 Nothing),
      TestLabel "fromMaybe undefined (Just 2)" $
        TestCase $
          assertEqual "fromMaybe undefined (Just 2)" (2 :: Integer) (fromMaybe undefined (Just 2)),
      -- Tests for concatMaybeMap
      TestLabel "concatMaybeMap with Nothing input" $
        TestCase $
          assertEqual "concatMaybeMap f Nothing" (Nothing :: Maybe Integer) (concatMaybeMap (\x -> Just (x + 1)) Nothing),
      TestLabel "concatMaybeMap with Just input" $
        TestCase $
          assertEqual "concatMaybeMap f (Just 3)" (Just 4 :: Maybe Integer) (concatMaybeMap (\x -> Just (x + 1)) (Just 3)),
      TestLabel "maybe 1 Nothing" $
        TestCase $
          assertEqual "maybe 1 Nothing" (1 :: Integer) (maybe 1 undefined Nothing),
      TestLabel "maybe undefined length (Just \"foo\")" $
        TestCase $
          assertEqual "maybe undefined length (Just \"foo\")" (3 :: Int) (maybe undefined length (Just "foo")),
      TestLabel "maybeHead [1,2,3]" $
        TestCase $
          assertEqual "maybeHead [1,2,3]" (Just (1 :: Integer)) (maybeHead [1, 2, 3]),
      TestLabel "maybeHead []" $
        TestCase $
          assertEqual "maybeHead []" (Nothing :: Maybe Integer) (maybeHead []),
      TestLabel "maybeLast [1,2,3]" $
        TestCase $
          assertEqual "maybeLast [1,2,3]" (Just (3 :: Integer)) (maybeLast [1, 2, 3]),
      TestLabel "maybeLast []" $
        TestCase $
          assertEqual "maybeLast []" (Nothing :: Maybe Integer) (maybeLast []),
      TestLabel "maybeMaximum [1,2,3]" $
        TestCase $
          assertEqual "maybeMaximum [1,2,3]" (Just (3 :: Int)) (maybeMaximum [1, 2, 3]),
      TestLabel "maybeMaximum []" $
        TestCase $
          assertEqual "maybeMaximum []" (Nothing :: Maybe Int) (maybeMaximum []),
      TestLabel "maybeMinimum [1,2,3]" $
        TestCase $
          assertEqual "maybeMinimum [1,2,3]" (Just (1 :: Int)) (maybeMinimum [1, 2, 3]),
      TestLabel "maybeMinimum []" $
        TestCase $
          assertEqual "maybeMinimum []" (Nothing :: Maybe Int) (maybeMinimum []),
      TestLabel "filterMaybe even (Just 2)" $
        TestCase $
          assertEqual "filterMaybe even (Just 2)" (Just (2 :: Integer)) (filterMaybe even (Just 2)),
      TestLabel "filterMaybe even (Just 3)" $
        TestCase $
          assertEqual "filterMaybe even (Just 3)" (Nothing :: Maybe Integer) (filterMaybe even (Just 3)),
      TestLabel "sumMaybe (Just 1) (Just 2)" $
        TestCase $
          assertEqual "sumMaybe (Just 1) (Just 2)" (Just 3 :: Maybe Int) (sumMaybe (Just 1) (Just 2)),
      TestLabel "sumMaybe (Just 1) Nothing" $
        TestCase $
          assertEqual "sumMaybe (Just 1) Nothing" (Nothing :: Maybe Int) (sumMaybe (Just 1) Nothing),
      TestLabel "liftMaybe2 (*) (Just 2) (Just 3)" $
        TestCase $
          assertEqual "liftMaybe2 (*) (Just 2) (Just 3)" (Just (6 :: Integer)) (liftMaybe2 (*) (Just 2) (Just 3)),
      TestLabel "liftMaybe2 undefined Nothing (Just 3)" $
        TestCase $
          assertEqual "liftMaybe2 undefined Nothing (Just 3)" (Nothing :: Maybe Integer) (liftMaybe2 undefined Nothing (Just 3)),
      TestLabel "mapMaybe double evens" $
        TestCase $
          assertEqual "mapMaybe double evens" ([4] :: [Integer]) (mapMaybe (\e -> if even e then Just (e * 2) else Nothing) [1, 2, 3]),
      TestLabel "catMaybes [Just 1, Just 2, Nothing, Just 3]" $
        TestCase $
          assertEqual "catMaybes [Just 1, Just 2, Nothing, Just 3]" ([1, 2, 3] :: [Integer]) (catMaybes [Just 1, Just 2, Nothing, Just 3]),
      -- Tests for Either-based functions
      TestLabel "fromEither undefined (Right 1)" $
        TestCase $
          assertEqual "fromEither undefined (Right 1)" (1 :: Integer) (fromEither undefined (Right 1)),
      TestLabel "fromEither \" foo \" (Left 42)" $
        TestCase $
          assertEqual "fromEither \" foo \" (Left 42)" (" foo " :: String) (fromEither " foo " (Left 42)),
      TestLabel "concatEitherMap f (Left z)" $
        TestCase $
          assertEqual "concatEitherMap should propagate Left without calling f" (Left "oops" :: Either String Int) (concatEitherMap (\x -> Right (x + 1)) (Left "oops")),
      TestLabel "concatEitherMap (\\x -> Right (x+1)) (Right 3)" $
        TestCase $
          assertEqual "concatEitherMap should apply f to Right value" (Right 4 :: Either String Int) (concatEitherMap (\x -> Right (x + 1)) (Right 3)),
      TestLabel "either (*2) (+3) (Left 4)" $
        TestCase $
          assertEqual "either should use the first function on Left" (8 :: Int) (either (* 2) (+ 3) (Left 4)),
      TestLabel "either (*2) (+3) (Right 4)" $
        TestCase $
          assertEqual "either should use the second function on Right" (7 :: Int) (either (* 2) (+ 3) (Right 4)),
      TestLabel "mapLeft (++ \"!\") (Right 10)" $
        TestCase $
          assertEqual "mapLeft should not change a Right value" (Right 10 :: Either String Int) (mapLeft (++ "!") (Right 10)),
      TestLabel "mapLeft (+1) (Left 2)" $
        TestCase $
          assertEqual "mapLeft should apply the function to a Left value" (Left 3 :: Either Int Int) (mapLeft (+ 1) (Left 2)),
      TestLabel "catEithers all Rights" $
        TestCase $
          assertEqual "catEithers all Rights" (Right [10, 20] :: Either String [Int]) (catEithers [Right 10, Right 20] :: Either String [Int]),
      TestLabel "catEithers stops at first Left" $
        TestCase $
          assertEqual "catEithers with an early Left" (Left "oops" :: Either String [Int]) (catEithers [Right 10, Left "oops", Right 20] :: Either String [Int]),
      TestLabel "catEithers with Left" $
        TestCase $
          assertEqual "catEithers with Left" (Left (" foo " :: String)) (catEithers [Right 10, Left " foo ", Right 20, Left " bar "]),
      TestLabel "liftEither2 (*) (Right 2) (Right 3)" $
        TestCase $
          assertEqual "liftEither2 (*) (Right 2) (Right 3)" (Right 6 :: Either String Int) (liftEither2 (*) (Right 2) (Right 3)),
      TestLabel "liftEither2 undefined (Right 2) (Left \" foo \")" $
        TestCase $
          assertEqual "liftEither2 undefined (Right 2) (Left \" foo \")" (Left " foo " :: Either String Int) (liftEither2 undefined (Right 2) (Left " foo ")),
      TestLabel "mapEither all Rights" $
        TestCase $
          assertEqual "mapEither all Rights" (Right ([10, 20, 30] :: [Integer])) (mapEither (\x -> if x > 0 then Right (x * 10) else Left (x + 5)) [1, 2, 3]),
      TestLabel "mapEither stops at first Left" $
        TestCase $
          assertEqual "mapEither stops at first Left" (Left (4 :: Integer)) (mapEither (\x -> if x > 0 then Right (x * 10) else Left (x + 5)) [1, -1, 2, -2]),
      TestLabel "partitionEithers example" $
        TestCase $
          assertEqual "partitionEithers example" (([42, 54], ["foo ", " bar "]) :: ([Integer], [String])) (partitionEithers [Right "foo ", Left 42, Left 54, Right " bar "]),
      TestLabel "eitherToMaybe Left" $
        TestCase $
          assertEqual "eitherToMaybe should return Nothing for a Left value" (Nothing :: Maybe Int) (eitherToMaybe (Left "oops" :: Either String Int)),
      TestLabel "eitherToMaybe Right Int" $
        TestCase $
          assertEqual "eitherToMaybe should wrap a Right Int in Just" (Just (42 :: Int)) (eitherToMaybe (Right 42 :: Either String Int)),
      TestLabel "eitherToMaybe Right String" $
        TestCase $
          assertEqual "eitherToMaybe should wrap a Right String in Just" (Just ("hello" :: String)) (eitherToMaybe (Right "hello" :: Either Int String)),
      -- Tests for productEither
      TestLabel "productEither Left-Left" $
        TestCase $
          assertEqual "should return the first Left when both are Left" (Left "err1" :: Either String Int) (productEither (Left "err1") (Left "err2")),
      TestLabel "productEither Left-Right" $
        TestCase $
          assertEqual "should return Left when first argument is Left" (Left "oops" :: Either String Int) (productEither (Left "oops") (Right 5)),
      TestLabel "productEither Right-Left" $
        TestCase $
          assertEqual "should return Left when second argument is Left" (Left "fail" :: Either String Int) (productEither (Right 3) (Left "fail")),
      TestLabel "productEither Right-Right" $
        TestCase $
          assertEqual "should multiply both Right values" (Right 20 :: Either String Int) (productEither (Right 4) (Right 5)),
      -- Tests for exprToString
      TestLabel "exprToString Lit 1 `Plus` Lit 2" $
        TestCase $
          assertEqual "should render 1 + 2" ("1 + 2" :: String) (exprToString (Lit 1 `Plus` Lit 2)),
      TestLabel "exprToString Iden \"x\" `Plus` (Lit 2 `Div` Lit 3)" $
        TestCase $
          assertEqual "should render x + (2 / 3)" ("x + (2 / 3)" :: String) (exprToString (Iden "x" `Plus` (Lit 2 `Div` Lit 3))),
      -- exprToString'
      TestLabel "exprToString (x + 42 + y) * 2 + 3" $
        TestCase $
          assertEqual
            "exprToString' ((Iden \"x\" `Plus` Lit 42 `Plus` Iden \"y\") `Mul` Lit 2 `Plus` Lit 3)"
            "(x + 42 + y) * 2 + 3"
            (exprToString' ((Iden "x" `Plus` Lit 42 `Plus` Iden "y") `Mul` Lit 2 `Plus` Lit 3)),
      TestLabel "exprToString 1 - (2 - 3)" $
        TestCase $
          assertEqual
            "exprToString' (Lit 1 `Minus` (Lit 2 `Minus` Lit 3))"
            "1 - (2 - 3)"
            (exprToString' (Lit 1 `Minus` (Lit 2 `Minus` Lit 3))),
      TestLabel "exprToString 1 - 2 - 3" $
        TestCase $
          assertEqual
            "exprToString' ((Lit 1 `Minus` Lit 2) `Minus` Lit 3)"
            "1 - 2 - 3"
            (exprToString' ((Lit 1 `Minus` Lit 2) `Minus` Lit 3)),
      -- Tests for partialEvaluate
      TestLabel "partialEvaluate: evaluate known sub‐expr, keep rest" $
        TestCase $
          assertEqual
            "should fold (Lit 1 + Iden \"x\") to Lit 43 and leave (Iden \"y\" - Lit 2) intact"
            (Just (Mul (Lit 43) (Minus (Iden "y") (Lit 2))) :: Maybe Expr)
            ( partialEvaluate
                [("x", 42)]
                ((Lit 1 `Plus` Iden "x") `Mul` (Iden "y" `Minus` Lit 2))
            ),
      TestLabel "partialEvaluate: division by zero yields Nothing" $
        TestCase $
          assertEqual "division by zero anywhere should produce Nothing" (Nothing :: Maybe Expr) (partialEvaluate [("x", 42)] ((Lit 1 `Plus` Iden "x") `Mul` (Iden "y" `Div` Lit 0))),
      -- Added tests for powerExpr, modExpr and negateExpr
      TestLabel "partialEvaluate negateExpr (Lit 20 `Minus` Lit 62)" $
        TestCase $
          assertEqual "partialEvaluate [] $ negateExpr $ Lit 20 `Minus` Lit 62" (Just (Lit 42) :: Maybe Expr) (partialEvaluate [] $ negateExpr $ Lit 20 `Minus` Lit 62),
      TestLabel "partialEvaluate powerExpr exponent 0" $
        TestCase $
          assertEqual
            "partialEvaluate [] $ Lit 42 `powerExpr` 0"
            (Just (Lit 1) :: Maybe Expr)
            (partialEvaluate [] $ Lit 42 `powerExpr` 0),
      TestLabel "partialEvaluate powerExpr 0^0" $
        TestCase $
          assertEqual
            "partialEvaluate [] $ Lit 0 `powerExpr` 0"
            (Just (Lit 1) :: Maybe Expr)
            (partialEvaluate [] $ Lit 0 `powerExpr` 0),
      TestLabel "partialEvaluate powerExpr with variable base" $
        TestCase $
          assertEqual
            "partialEvaluate [(\"x\",2)] $ (Iden \"x\" `Plus` Lit 4) `powerExpr` 3"
            (Just (Lit 216) :: Maybe Expr)
            (partialEvaluate [("x", 2)] $ (Iden "x" `Plus` Lit 4) `powerExpr` 3),
      TestLabel "partialEvaluate negative exponent yields 0" $
        TestCase $
          assertEqual
            "partialEvaluate [] $ (Lit 2 `Plus` Lit 3) `powerExpr` (-1)"
            (Just (Lit 0) :: Maybe Expr)
            (partialEvaluate [] $ (Lit 2 `Plus` Lit 3) `powerExpr` (-1)),
      TestLabel "partialEvaluate powerExpr div-by-zero & negative exponent" $
        TestCase $
          assertEqual
            "partialEvaluate [] $ (Lit 2 `Div` Lit 0) `powerExpr` (-1)"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] $ (Lit 2 `Div` Lit 0) `powerExpr` (-1)),
      TestLabel "partialEvaluate powerExpr div-by-zero & zero exponent" $
        TestCase $
          assertEqual
            "partialEvaluate [] $ (Lit 2 `Div` Lit 0) `powerExpr` 0"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] $ (Lit 2 `Div` Lit 0) `powerExpr` 0),
      TestLabel "partialEvaluate modExpr with variable sum" $
        TestCase $
          assertEqual
            "partialEvaluate [(\"x\",2),(\"y\",3)] $ Lit 42 `modExpr` (Iden \"x\" `Plus` Iden \"y\")"
            (Just (Lit 2) :: Maybe Expr)
            (partialEvaluate [("x", 2), ("y", 3)] $ Lit 42 `modExpr` (Iden "x" `Plus` Iden "y")),
      TestLabel "partialEvaluate modExpr by zero yields Nothing" $
        TestCase $
          assertEqual
            "partialEvaluate [] $ Lit 42 `modExpr` Lit 0"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] $ Lit 42 `modExpr` Lit 0),
      -- Tests for negateExpr
      TestLabel "negateExpr literal" $
        TestCase $
          assertEqual "negateExpr Lit 5" (Lit 0 `Minus` Lit 5) (negateExpr (Lit 5)),
      TestLabel "negateExpr nested" $
        TestCase $
          assertEqual "negateExpr (Lit 2 `Mul` Lit 3)" (Lit 0 `Minus` (Lit 2 `Mul` Lit 3)) (negateExpr (Lit 2 `Mul` Lit 3)),
      -- Tests for powerExpr
      TestLabel "partialEvaluate [] $ Lit 42 `powerExpr` 0" $
        TestCase $
          assertEqual
            "42 ^ 0  ==> 1"
            (Just (Lit 1))
            (partialEvaluate [] (powerExpr (Lit 42) 0)),
      TestLabel "partialEvaluate [] $ Lit 0 `powerExpr` 0" $
        TestCase $
          assertEqual
            "0 ^ 0  ==> 1   (by definition)"
            (Just (Lit 1))
            (partialEvaluate [] (powerExpr (Lit 0) 0)),
      TestLabel "partialEvaluate [(\"x\",2)] $ (Iden \"x\" `Plus` Lit 4) `powerExpr` 3" $
        TestCase $
          assertEqual
            "(x + 4) ^ 3  with  x = 2  ==> 216"
            (Just (Lit 216))
            (partialEvaluate [("x", 2)] (powerExpr (Iden "x" `Plus` Lit 4) 3)),
      TestLabel "partialEvaluate [] $ (Lit 2 `Plus` Lit 3) `powerExpr` (-1)" $
        TestCase $
          assertEqual
            "(2 + 3) ^ (-1)  ==> 0"
            (Just (Lit 0))
            (partialEvaluate [] (powerExpr (Lit 2 `Plus` Lit 3) (-1))),
      TestLabel "partialEvaluate [] $ (Lit 2 `Div` Lit 0) `powerExpr` (-1)" $
        TestCase $
          assertEqual
            "division by zero inside base (n < 0)  ==> Nothing"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] (powerExpr (Lit 2 `Div` Lit 0) (-1))),
      TestLabel "partialEvaluate [] $ (Lit 2 `Div` Lit 0) `powerExpr` 0" $
        TestCase $
          assertEqual
            "division by zero inside base (n == 0) ==> Nothing"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] (powerExpr (Lit 2 `Div` Lit 0) 0)),
      -- Tests for modExpr
      TestLabel "partialEvaluate [(\"x\",2),(\"y\",3)] (Lit 42 `modExpr` (Iden \"x\" `Plus` Iden \"y\"))" $
        TestCase $
          assertEqual
            "42 mod (x + y) with x = 2, y = 3  ==> 2"
            (Just (Lit 2))
            (partialEvaluate [("x", 2), ("y", 3)] (Lit 42 `modExpr` (Iden "x" `Plus` Iden "y"))),
      TestLabel "partialEvaluate [] (Lit 42 `modExpr` Lit 0)" $
        TestCase $
          assertEqual
            "modExpr with divisor 0  ==> Nothing"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] (Lit 42 `modExpr` Lit 0)),
      TestLabel "partialEvaluate [] (Lit (-7) `modExpr` Lit 5)" $
        TestCase $
          assertEqual
            "(-7) mod 5  ==> 3"
            (Just (Lit 3))
            (partialEvaluate [] (Lit (-7) `modExpr` Lit 5)),
      TestLabel "partialEvaluate [] ((Lit 1 `Div` Lit 0) `modExpr` Lit 5)" $
        TestCase $
          assertEqual
            "error inside dividend propagates through modExpr  ==> Nothing"
            (Nothing :: Maybe Expr)
            (partialEvaluate [] ((Lit 1 `Div` Lit 0) `modExpr` Lit 5)),
      -- Tests for zip
      TestLabel "zip [1,2,3] \"foo\"" $
        TestCase $
          assertEqual "should pair each Int with the corresponding Char" ([(1, 'f'), (2, 'o'), (3, 'o')] :: [(Int, Char)]) (zip [1, 2, 3] "foo"),
      -- Tests for zipWith (+)
      TestLabel "zipWith (+) [1,2,3] [4,5,6]" $
        TestCase $
          assertEqual "should add elements pointwise" ([5, 7, 9] :: [Int]) (zipWith (+) [1, 2, 3] [4, 5, 6]),
      -- Tests for zipWithIndex
      TestLabel "zipWithIndex empty list" $
        TestCase $
          assertEqual "zipWithIndex [] should be []" ([] :: [(Int, Int)]) (zipWithIndex ([] :: [Int])),
      TestLabel "zipWithIndex on ints" $
        TestCase $
          assertEqual "zipWithIndex [10,20,30]" ([(0, 10), (1, 20), (2, 30)] :: [(Int, Int)]) (zipWithIndex [10, 20, 30]),
      TestLabel "zipWithIndex on string" $
        TestCase $
          assertEqual "zipWithIndex \"abc\"" ([(0, 'a'), (1, 'b'), (2, 'c')] :: [(Int, Char)]) (zipWithIndex "abc"),
      -- Tests for zipWithDefault
      TestLabel "zipWithDefault shorter list longer string" $
        TestCase $
          assertEqual
            "zipWithDefault 0 'a' [1,2,3] \"foobar\""
            ([(1, 'f'), (2, 'o'), (3, 'o'), (0, 'b'), (0, 'a'), (0, 'r')] :: [(Int, Char)])
            (zipWithDefault 0 'a' [1, 2, 3] "foobar"),
      TestLabel "zipWithDefault longer list shorter string" $
        TestCase $
          assertEqual
            "zipWithDefault 0 'a' [1..6] \"foo\""
            ([(1, 'f'), (2, 'o'), (3, 'o'), (4, 'a'), (5, 'a'), (6, 'a')] :: [(Int, Char)])
            (zipWithDefault 0 'a' [1 .. 6] "foo"),
      -- Tests for zipEither
      TestLabel "zipEither shorter first list" $
        TestCase $
          assertEqual
            "zipEither [1,2] \"foobar\" ==> Left ErrorFirst"
            (Left ErrorFirst :: Either ZipFail [(Int, Char)])
            (zipEither [1, 2] "foobar"),
      TestLabel "zipEither shorter string" $
        TestCase $
          assertEqual
            "zipEither [1..] \"foobar\" ==> Left ErrorSecond"
            (Left ErrorSecond :: Either ZipFail [(Int, Char)])
            (zipEither [1 ..] "foobar"),
      TestLabel "zipEither equal lengths" $
        TestCase $
          assertEqual
            "zipEither [1,2-- Tests for powerExpr,3] \"foo\" ==> Right [(1,'f'),(2,'o'),(3,'o')]"
            (Right [(1, 'f'), (2, 'o'), (3, 'o')] :: Either ZipFail [(Int, Char)])
            (zipEither [1, 2, 3] "foo"),
      -- Tests for unzip
      TestLabel "unzip [(1,2),(3,4)]" $
        TestCase $
          assertEqual
            "should split list of pairs into a pair of lists"
            (([1, 3], [2, 4]) :: ([Int], [Int]))
            (unzip [(1, 2), (3, 4)]),
      -- Tests for unzipFst
      TestLabel "unzipFst [(1,2),(3,4)]" $
        TestCase $
          assertEqual
            "should extract first elements"
            ([1, 3] :: [Int])
            (unzipFirst [(1, 2), (3, 4)]),
      -- Tests for unzipSnd
      TestLabel "unzipSnd [(1,2),(3,4)]" $
        TestCase $
          assertEqual "should extract second elements" ([2, 4] :: [Int]) (unzipSecond [(1, 2), (3, 4)]),
      -- Tests for cartesianWith
      TestLabel "cartesianWith (*) [10,20] [3,4,5,6]" $
        TestCase $
          assertEqual
            "should produce all products in the right order"
            ([30, 40, 50, 60, 60, 80, 100, 120] :: [Int])
            (cartesianWith (*) [10, 20] [3, 4, 5, 6]),
      -- Tests for snoc
      TestLabel "take 5 $ snoc [1..] 0" $
        TestCase $
          assertEqual "take 5 $ snoc [1..] 0" ([1, 2, 3, 4, 5] :: [Int]) (take 5 $ snoc [1 ..] 0),
      TestLabel "snoc [2,3] 1" $
        TestCase $
          assertEqual "snoc [2,3] 1" ([2, 3, 1] :: [Int]) (snoc [2, 3] 1),
      -- Tests for take and drop
      TestLabel "take 2 [1,2,3]" $
        TestCase $
          assertEqual "take 2 [1,2,3]" ([1, 2] :: [Int]) (take 2 [1, 2, 3]),
      TestLabel "take 4 [1..]" $
        TestCase $
          assertEqual "take 4 [1..]" ([1, 2, 3, 4] :: [Int]) (take 4 [1 ..]),
      TestLabel "takeWhile (<5) [1..]" $
        TestCase $
          assertEqual "takeWhile (<5) [1..]" ([1, 2, 3, 4] :: [Int]) (takeWhile (< 5) [1 ..]),
      TestLabel "drop 2 [1,2,3]" $
        TestCase $
          assertEqual "drop 2 [1,2,3]" ([3] :: [Int]) (drop 2 [1, 2, 3]),
      TestLabel "take 5 $ dropWhile (<5) [1..]" $
        TestCase $
          assertEqual "take 5 $ dropWhile (<5) [1..]" ([5, 6, 7, 8, 9] :: [Int]) (take 5 $ dropWhile (< 5) [1 ..]),
      -- Tests for slice
      TestLabel "slice 2 5 [1..]" $
        TestCase $
          assertEqual "slice 2 5 [1..]" ([3, 4, 5] :: [Int]) (slice 2 5 [1 ..]),
      -- Tests for takeEvery / dropEvery
      TestLabel "take 5 $ takeEvery 3 [1..]" $
        TestCase $
          assertEqual "takeEvery 3 [1..]" ([3, 6, 9, 12, 15] :: [Int]) (take 5 $ takeEvery 3 [1 ..]),
      TestLabel "take 5 $ dropEvery 3 [1..]" $
        TestCase $
          assertEqual "dropEvery 3 [1..]" ([1, 2, 4, 5, 7] :: [Int]) (take 5 $ dropEvery 3 [1 ..]),
      -- Tests for nub (consecutive-dup removal)
      TestLabel "nub [1,1,2,3,2,4]" $
        TestCase $
          assertEqual "nub only removes consecutive duplicates" ([1, 2, 3, 2, 4] :: [Int]) (nub [1, 1, 2, 3, 2, 4]),
      TestLabel "take 5 $ nub [1..]" $
        TestCase $
          assertEqual "take 5 $ nub [1..]" ([1, 2, 3, 4, 5] :: [Int]) (take 5 $ nub [1 ..]),
      -- Tests for infiniteRepeats and nub on it
      TestLabel "take 15 infiniteRepeats" $
        TestCase $
          assertEqual "infiniteRepeats = [1,1,1,2,2,2,...]" ([1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5] :: [Int]) (take 15 infiniteRepeats),
      TestLabel "take 15 $ nub infiniteRepeats" $
        TestCase $
          assertEqual "nub on infiniteRepeats removes only consecutive duplicates" ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] :: [Int]) (take 15 $ nub infiniteRepeats),
      -- Tests for uniq (removes all duplicates)
      TestLabel "uniq [1,1,2,3,2,4]" $
        TestCase $
          assertEqual "uniq removes every duplicate" ([1, 2, 3, 4] :: [Int]) (uniq [1, 1, 2, 3, 2, 4]),
      -- Edge cases for take/drop/takeWhile/dropWhile
      TestLabel "take 4 [1,2,3]" $
        TestCase $
          assertEqual
            "take more than list length"
            ([1, 2, 3] :: [Int])
            (take 4 [1, 2, 3]),
      TestLabel "take 0 undefined" $
        TestCase $
          assertEqual
            "take 0 from undefined yields []"
            ([] :: [Int])
            (take 0 (undefined :: [Int])),
      TestLabel "take -1 undefined" $
        TestCase $
          assertEqual
            "take negative count yields []"
            ([] :: [Int])
            (take (-1) (undefined :: [Int])),
      TestLabel "takeWhile (<1) [1..]" $
        TestCase $
          assertEqual
            "takeWhile predicate fails at first element"
            ([] :: [Int])
            (takeWhile (< 1) ([1 ..] :: [Int])),
      TestLabel "drop 4 [1,2,3]" $
        TestCase $
          assertEqual
            "drop more than list length yields []"
            ([] :: [Int])
            (drop 4 [1, 2, 3]),
      TestLabel "drop 0 [1,2,3]" $
        TestCase $
          assertEqual
            "drop 0 yields entire list"
            ([1, 2, 3] :: [Int])
            (drop 0 [1, 2, 3]),
      TestLabel "drop -1 [1,2,3]" $
        TestCase $
          assertEqual
            "drop negative count yields entire list"
            ([1, 2, 3] :: [Int])
            (drop (-1) [1, 2, 3]),
      TestLabel "take 5 $ dropWhile (<1) [1..]" $
        TestCase $
          assertEqual
            "dropWhile skips until predicate fails"
            ([1, 2, 3, 4, 5] :: [Int])
            (take 5 $ dropWhile (< 1) ([1 ..] :: [Int])),
      -- Tests for slice (start,end semantics: start<0→0; end<=start→[])
      TestLabel "slice 3 3 undefined" $
        TestCase $
          assertEqual
            "slice start==end yields [] without evaluating the list"
            ([] :: [Int])
            (slice 3 3 (undefined :: [Int])),
      TestLabel "slice 5 2 undefined" $
        TestCase $
          assertEqual
            "slice end<start yields []"
            ([] :: [Int])
            (slice 5 2 (undefined :: [Int])),
      TestLabel "slice (-2) 5 [1..]" $
        TestCase $
          assertEqual
            "negative start treated as 0, take 5 elements"
            ([1, 2, 3, 4, 5] :: [Int])
            (slice (-2) 5 ([1 ..] :: [Int])),
      -- Tests for takeEvery
      TestLabel "takeEvery 4 [1,2,3]" $
        TestCase $
          assertEqual
            "takeEvery beyond length → []"
            ([] :: [Int])
            (takeEvery 4 [1, 2, 3]),
      TestLabel "takeEvery 1 [1,2,3]" $
        TestCase $
          assertEqual
            "takeEvery 1 → original list"
            ([1, 2, 3] :: [Int])
            (takeEvery 1 [1, 2, 3]),
      TestLabel "takeEvery 0 [1,2,3]" $
        TestCase $
          assertEqual
            "takeEvery zero treated as 1 → original list"
            ([1, 2, 3] :: [Int])
            (takeEvery 0 [1, 2, 3]),
      TestLabel "takeEvery (-1) [1,2,3]" $
        TestCase $
          assertEqual
            "takeEvery negative treated as 1 → original list"
            ([1, 2, 3] :: [Int])
            (takeEvery (-1) [1, 2, 3]),
      -- Tests for dropEvery
      TestLabel "dropEvery 4 [1,2,3]" $
        TestCase $
          assertEqual
            "dropEvery beyond length → original list"
            ([1, 2, 3] :: [Int])
            (dropEvery 4 [1, 2, 3]),
      TestLabel "dropEvery 1 undefined" $
        TestCase $
          assertEqual
            "dropEvery 1 on undefined list → []"
            ([] :: [Int])
            (dropEvery 1 (undefined :: [Int])),
      TestLabel "dropEvery 0 undefined" $
        TestCase $
          assertEqual
            "dropEvery zero treated as 1 on undefined → []"
            ([] :: [Int])
            (dropEvery 0 (undefined :: [Int])),
      TestLabel "dropEvery -1 undefined" $
        TestCase $
          assertEqual
            "dropEvery negative treated as 1 on undefined → []"
            ([] :: [Int])
            (dropEvery (-1) (undefined :: [Int])),
      TestLabel "take 10 [1,1..]" $
        TestCase $
          assertEqual "take 10 [1,1..]" ([1, 1, 1, 1, 1, 1, 1, 1, 1, 1] :: [Int]) (take 10 [1, 1 ..]),
      -- Base-64 encoding/decoding tests
      TestLabel "toBase64 0" $
        TestCase $
          assertEqual "toBase64 0" "A" (toBase64 0),
      TestLabel "toBase64 509701384549" $
        TestCase $
          assertEqual "toBase64 509701384549" "Haskell" (toBase64 509701384549),
      TestLabel "fromBase64 \"Haskell\"" $
        TestCase $
          assertEqual "fromBase64 \"Haskell\"" (Just (509701384549 :: Integer)) (fromBase64 "Haskell"),
      TestLabel "toBase64 (-509701384549)" $
        TestCase $
          assertEqual "toBase64 (-509701384549)" "-Haskell" (toBase64 (-509701384549)),
      TestLabel "fromBase64 \"-Haskell\"" $
        TestCase $
          assertEqual "fromBase64 \"-Haskell\"" (Just (-509701384549 :: Integer)) (fromBase64 "-Haskell"),
      TestLabel "fromBase64 \"--Haskell\"" $
        TestCase $
          assertEqual "fromBase64 \"--Haskell\"" (Just (509701384549 :: Integer)) (fromBase64 "--Haskell"),
      TestLabel "fromBase64 \"Haskell?\"" $
        TestCase $
          assertEqual "fromBase64 \"Haskell?\"" Nothing (fromBase64 "Haskell?")
    ]

main :: IO Counts
main = runTestTT tests
