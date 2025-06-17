{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

-- avoid clashing with Prelude.fold
-- your implementation under test
import Data.Monoid (Product (..), Sum (..), getSum)
import HW5
import MultiSet (MultiSet, empty, foldOccur, fromList, insert)
import Test.HUnit
import Prelude hiding (concatMap, length, maximum, minimum, null, product, sum, unzip)

--------------------------------------------------------------------------------
-- Test list -------------------------------------------------------------------
--------------------------------------------------------------------------------

tests :: Test
tests =
  TestList
    [ --------------------------------------------------------------------------
      -- 1. Sanity check --------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "True is True" $
        TestCase $
          assertBool "True == True" True,
      --------------------------------------------------------------------------
      -- 2. fold – Sum monoid example (matches screenshot) ---------------------
      --------------------------------------------------------------------------
      TestLabel "fold: Sum over list" $
        TestCase $ do
          -- getSum $ fold $ map Sum [1,2,3]  ==>  6
          let result = getSum $ fold $ map Sum [1, 2, 3 :: Int]
          assertEqual
            "getSum (fold (map Sum [1,2,3])) == 6"
            6
            result,
      --------------------------------------------------------------------------
      -- 3. toList – Maybe example (matches screenshot) ------------------------
      --------------------------------------------------------------------------
      TestLabel "toList: Just 4" $
        TestCase $ do
          -- toList $ Just 4  ==>  [4]
          let result = toList (Just 4 :: Maybe Int)
          assertEqual
            "toList (Just 4) == [4]"
            [4 :: Int]
            result,
      --------------------------------------------------------------------------
      -- 4. toList – Nothing example (matches screenshot) ----------------------
      --------------------------------------------------------------------------
      TestLabel "toList: Nothing" $
        TestCase $ do
          -- toList $ Nothing  ==>  []
          let result = toList (Nothing :: Maybe Int)
          assertEqual
            "toList Nothing == []"
            ([] :: [Int])
            result,
      --------------------------------------------------------------------------
      -- 5. find – assorted Foldable cases -------------------------------------
      --------------------------------------------------------------------------
      -- 5.1 first even number in a list
      TestLabel
        "find: first even in list"
        $ TestCase
        $ do
          let result = find even ([1 .. 5] :: [Int])
          assertEqual "find even [1..5] == Just 2" (Just 2) result,
      -- 5.2 no matching element
      TestLabel "find: no match in list" $
        TestCase $ do
          let result = find (> 10) ([1 .. 5] :: [Int])
          assertEqual "find (>10) [1..5] == Nothing" Nothing result,
      -- 5.3 working on a Just value
      TestLabel "find: on Just" $
        TestCase $ do
          let result = find (> 0) (Just 3 :: Maybe Int)
          assertEqual "find (>0) (Just 3) == Just 3" (Just 3) result,
      -- 5.4 working on Nothing
      TestLabel "find: on Nothing" $
        TestCase $ do
          let result = find (> 0) (Nothing :: Maybe Int)
          assertEqual "find (>0) Nothing == Nothing" Nothing result,
      -- 5.5 searching in a String (list of Char)
      TestLabel "find: char in String" $
        TestCase $ do
          let result = find (== 'b') ("abc" :: String)
          assertEqual "find (=='b') \"abc\" == Just 'b'" (Just 'b') result,
      --------------------------------------------------------------------------
      -- 6. length & null -------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "length: empty list" $
        TestCase $
          assertEqual
            "length [] == 0"
            0
            (length ([] :: [Int])),
      TestLabel "length: non-empty list" $
        TestCase $
          assertEqual
            "length [1,2,3] == 3"
            3
            (length ([1, 2, 3] :: [Int])),
      TestLabel "length: Just x" $
        TestCase $
          assertEqual
            "length (Just 'a') == 1"
            1
            (length (Just 'a' :: Maybe Char)),
      TestLabel "length: Nothing" $
        TestCase $
          assertEqual
            "length Nothing == 0"
            0
            (length (Nothing :: Maybe Int)),
      TestLabel "null: empty list" $
        TestCase $
          assertBool
            "null [] == True"
            (null ([] :: [Int])),
      TestLabel "null: non-empty list" $
        TestCase $
          assertBool
            "null [1] == False"
            (not $ null [1 :: Int]),
      TestLabel "null: Nothing" $
        TestCase $
          assertBool
            "null Nothing == True"
            (null (Nothing :: Maybe Int)),
      --------------------------------------------------------------------------
      -- 7. maximum & minimum ---------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "maximum: empty list" $
        TestCase $
          assertEqual
            "maximum [] == Nothing"
            (Nothing :: Maybe Int)
            (maximum ([] :: [Int])),
      TestLabel "maximum: non-empty list" $
        TestCase $
          assertEqual
            "maximum [3,1,4,2] == Just 4"
            (Just 4)
            (maximum ([3, 1, 4, 2] :: [Int])),
      TestLabel "minimum: empty list" $
        TestCase $
          assertEqual
            "minimum [] == Nothing"
            (Nothing :: Maybe Int)
            (minimum ([] :: [Int])),
      TestLabel "minimum: non-empty list" $
        TestCase $
          assertEqual
            "minimum [3,1,4,2] == Just 1"
            (Just 1)
            (minimum ([3, 1, 4, 2] :: [Int])),
      --------------------------------------------------------------------------
      -- 8. maxBy & minBy --------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "maxBy: length on strings" $
        TestCase $
          assertEqual
            "maxBy length [\"foo\",\"bar\",\"bazz\"] == Just \"bazz\""
            (Just "bazz")
            (maxBy length ["foo", "bar", "bazz"]),
      TestLabel "maxBy: empty list" $
        TestCase $
          assertEqual
            "maxBy length [] == Nothing"
            (Nothing :: Maybe String)
            (maxBy length ([] :: [String])),
      TestLabel "minBy: length on strings" $
        TestCase $
          assertEqual
            "minBy length [\"bar\",\"bazz\"] == Just \"bar\""
            (Just "bar")
            (minBy length ["bar", "bazz"]),
      TestLabel "minBy: empty list" $
        TestCase $
          assertEqual
            "minBy length [] == Nothing"
            (Nothing :: Maybe String)
            (minBy length ([] :: [String])),
      --------------------------------------------------------------------------
      -- 9. sum & product -------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "sum: empty list" $
        TestCase $
          assertEqual
            "sum [] == 0"
            (0 :: Int)
            (sum ([] :: [Int])),
      TestLabel "sum: non-empty list" $
        TestCase $
          assertEqual
            "sum [1,2,3] == 6"
            6
            (sum ([1, 2, 3] :: [Int])),
      TestLabel "sum: Just 5" $
        TestCase $
          assertEqual
            "sum (Just 5) == 5"
            5
            (sum (Just 5 :: Maybe Int)),
      TestLabel "product: empty list" $
        TestCase $
          assertEqual
            "product [] == 1"
            (1 :: Int)
            (product ([] :: [Int])),
      TestLabel "product: non-empty list" $
        TestCase $
          assertEqual
            "product [2,3,4] == 24"
            24
            (product ([2, 3, 4] :: [Int])),
      TestLabel "product: Just 5" $
        TestCase $
          assertEqual
            "product (Just 5) == 5"
            5
            (product (Just 5 :: Maybe Int)),
      --------------------------------------------------------------------------
      -- 10. concatMap ----------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "concatMap: duplicate each" $
        TestCase $
          assertEqual
            "concatMap (\\x->[x,x]) [1,2,3] == [1,1,2,2,3,3]"
            [1, 1, 2, 2, 3, 3 :: Int]
            (concatMap (\x -> [x, x]) ([1, 2, 3] :: [Int])),
      TestLabel "concatMap: id on list of lists" $
        TestCase $
          assertEqual
            "concatMap id [[1,2],[3]] == [1,2,3]"
            ([1, 2, 3] :: [Int])
            (concatMap id ([[1, 2], [3]] :: [[Int]])),
      --------------------------------------------------------------------------
      -- filterF ---------------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "filterF: even numbers" $
        TestCase $
          assertEqual
            "filterF even on [1..5] yields [2,4]"
            ([2, 4] :: [Int])
            ( runFold
                (filterF even (Fold (\acc x -> acc ++ [x]) [] id))
                ([1 .. 5] :: [Int])
            ),
      --------------------------------------------------------------------------
      -- mapF ------------------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "mapF: doubling each element" $
        TestCase $
          assertEqual
            "mapF (*2) on [1,2,3] yields [2,4,6]"
            ([2, 4, 6] :: [Int])
            ( runFold
                (mapF (* 2) (Fold (\acc x -> acc ++ [x]) [] id))
                ([1, 2, 3] :: [Int])
            ),
      --------------------------------------------------------------------------
      -- nullF -----------------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "nullF: empty container" $
        TestCase $
          assertEqual
            "nullF on [] == True"
            True
            (runFold nullF ([] :: [Int])),
      TestLabel "nullF: non-empty container" $
        TestCase $
          assertEqual
            "nullF on [1] == False"
            False
            (runFold nullF ([1] :: [Int])),
      --------------------------------------------------------------------------
      -- findF -----------------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "findF: first greater than 3" $
        TestCase $
          assertEqual
            "findF (>3) on [1,4,2] == Just 4"
            (Just 4)
            (runFold (findF (> 3)) ([1, 4, 2] :: [Int])),
      TestLabel "findF: nothing found" $
        TestCase $
          assertEqual
            "findF (>10) on [1,2,3] == Nothing"
            (Nothing :: Maybe Int)
            (runFold (findF (> 10)) ([1, 2, 3] :: [Int])),
      --------------------------------------------------------------------------
      -- topKF -----------------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "topKF: top 2 elements" $
        TestCase $
          assertEqual
            "topKF 2 on [5,1,3,2] == [5,3]"
            ([5, 3] :: [Int])
            (runFold (topKF 2) ([5, 1, 3, 2] :: [Int])),
      TestLabel "topKF: k larger than list length" $
        TestCase $
          assertEqual
            "topKF 5 on [2,1] == [2,1]"
            ([2, 1] :: [Int])
            (runFold (topKF 5) ([2, 1] :: [Int])),
      --------------------------------------------------------------------------
      -- 11. sumF & productF ----------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "sumF: sum of [1,2,3]" $
        TestCase $
          assertEqual
            "runFold sumF [1,2,3] == 6"
            (6 :: Int)
            (runFold sumF ([1, 2, 3] :: [Int])),
      TestLabel "productF: product of [2,3,4]" $
        TestCase $
          assertEqual
            "runFold productF [2,3,4] == 24"
            (24 :: Int)
            (runFold productF ([2, 3, 4] :: [Int])),
      TestLabel "sumF: on Just 5" $
        TestCase $
          assertEqual
            "runFold sumF (Just 5) == 5"
            (5 :: Int)
            (runFold sumF (Just 5 :: Maybe Int)),
      TestLabel "productF: on Just 5" $
        TestCase $
          assertEqual
            "runFold productF (Just 5) == 5"
            (5 :: Int)
            (runFold productF (Just 5 :: Maybe Int)),
      --------------------------------------------------------------------------
      -- 12. lengthF & averageF -------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "lengthF: length of [1,2,3]" $
        TestCase $
          assertEqual
            "runFold lengthF [1,2,3] == 3"
            (3 :: Int)
            (runFold lengthF ([1, 2, 3] :: [Int])),
      TestLabel "lengthF: length of []" $
        TestCase $
          assertEqual
            "runFold lengthF [] == 0"
            (0 :: Int)
            (runFold lengthF ([] :: [Int])),
      TestLabel "averageF: avg of [1,2,3,4]" $
        TestCase $
          assertEqual
            "runFold averageF [1,2,3,4] == 2.5"
            (2.5 :: Double)
            (runFold averageF ([1, 2, 3, 4] :: [Double])),
      TestLabel "averageF: avg of [5]" $
        TestCase $
          assertEqual
            "runFold averageF [5] == 5"
            (5 :: Double)
            (runFold averageF ([5] :: [Double])),
      --------------------------------------------------------------------------
      -- 13. Functor-derived helpers -------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "fmapToFst: length [\"foo\",\"bar\"]" $
        TestCase $
          assertEqual
            "fmapToFst length [\"foo\",\"bar\"] == [(3,\"foo\"),(3,\"bar\")]"
            [(3, "foo"), (3, "bar")]
            (fmapToFst length ["foo", "bar"]),
      TestLabel "fmapToSnd: length Just \"foo\"" $
        TestCase $
          assertEqual
            "fmapToSnd length (Just \"foo\") == Just (\"foo\",3)"
            (Just ("foo", 3))
            (fmapToSnd length (Just "foo")),
      TestLabel "strengthenL: 42 $ Right \"foo\"" $
        TestCase $
          assertEqual
            "strengthenL 42 (Right \"foo\" :: Either () String) == Right (42,\"foo\")"
            (Right (42, "foo") :: Either () (Int, String))
            (strengthenL 42 (Right "foo" :: Either () String)),
      TestLabel "strengthenR: \"x\" [1,2,3]" $
        TestCase $
          assertEqual
            "strengthenR \"x\" [1,2,3] == [(1,\"x\"),(2,\"x\"),(3,\"x\")]"
            [(1 :: Integer, "x"), (2 :: Integer, "x"), (3 :: Integer, "x")]
            (strengthenR "x" [1, 2, 3]),
      TestLabel "unzip: Just (1,2)" $
        TestCase $
          assertEqual
            "unzip (Just (1,2)) == (Just 1,Just 2)"
            (Just 1, Just 2)
            (unzip (Just (1, 2) :: Maybe (Int, Int))),
      TestLabel "coUnzip: Right [1,2,3]" $
        TestCase $
          assertEqual
            "coUnzip (Right [1,2,3] :: Either String [Int]) == [Right 1,Right 2,Right 3]"
            [Right 1, Right 2, Right 3]
            (coUnzip (Right [1, 2, 3] :: Either String [Int])),
      TestLabel "coUnzip: Left \"foo\"" $
        TestCase $
          assertEqual
            "coUnzip (Left \"foo\" :: Either String [Int]) == [Left 'f',Left 'o',Left 'o']"
            [Left 'f', Left 'o', Left 'o']
            (coUnzip (Left "foo" :: Either String [Int])),
      --------------------------------------------------------------------------
      -- 14. Foldable MultiSet.foldr --------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "Foldable MultiSet: sum with foldr (+) 0" $
        TestCase $
          assertEqual
            "foldr (+) 0 (fromList [1,2,2]) == 5"
            5
            (foldr (+) 0 (fromList ([1, 2, 2] :: [Int]) :: MultiSet Int)),
      TestLabel "Foldable MultiSet: toList via foldr (:) []" $
        TestCase $
          assertEqual
            "foldr (:) [] (fromList [1,2,2]) == [1,2,2]"
            ([1, 2, 2] :: [Int])
            (foldr (:) [] (fromList ([1, 2, 2] :: [Int]) :: MultiSet Int)),
      TestLabel "Foldable MultiSet: all True on empty Bool" $
        TestCase $
          assertBool
            "foldr (&&) True (empty :: MultiSet Bool) == True"
            (foldr (&&) True (empty :: MultiSet Bool)),
      TestLabel "and over [True,False,True] == False" $
        TestCase $
          assertEqual
            "foldr (&&) True (fromList [True,False,True]) should be False"
            False
            (foldr (&&) True (fromList ([True, False, True] :: [Bool]) :: MultiSet Bool)),
      --------------------------------------------------------------------------
      -- foldOccur: distinct elements ------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "foldOccur: distinct elements" $
        TestCase $
          assertEqual
            "toList $ foldOccur (\\x _ acc -> insert x acc) empty (insert 2 $ insert 1 $ insert 2 empty) == [1,2]"
            ([1, 2] :: [Int])
            ( toList $
                foldOccur
                  (\x _ acc -> insert x acc)
                  empty
                  (insert 2 $ insert 1 $ insert 2 empty)
            ),
      --------------------------------------------------------------------------
      -- 15. Foldable wrappers: FoldOccur, MinToMax, MaxToMin ------------------
      --------------------------------------------------------------------------
      TestLabel "FoldOccur: distinct elements" $
        TestCase $
          assertEqual
            "toList (FoldOccur (insert 2 $ insert 1 $ insert 2 empty)) == [1,2]"
            ([1, 2] :: [Int])
            (toList (FoldOccur (insert 2 $ insert 1 $ insert 2 empty))),
      TestLabel "MinToMax: ascending with duplicates" $
        TestCase $
          assertEqual
            "toList (MinToMax (insert 2 $ insert 1 $ insert 2 empty)) == [1,2,2]"
            ([1, 2, 2] :: [Int])
            (toList (MinToMax (insert 2 $ insert 1 $ insert 2 empty))),
      TestLabel "MaxToMin: descending with duplicates" $
        TestCase $
          assertEqual
            "toList (MaxToMin (insert 2 $ insert 1 $ insert 2 empty)) == [2,2,1]"
            ([2, 2, 1] :: [Int])
            (toList (MaxToMin (insert 2 $ insert 1 $ insert 2 empty))),
      --------------------------------------------------------------------------
      -- 16. Semigroup ZipList -------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "ZipList semigroup: sum" $
        TestCase $
          assertEqual
            "map getSum $ getZipList (ZipList (map Sum [1,2,3]) <> ZipList (map Sum [4,5])) == [5,7]"
            [5, 7 :: Int]
            ( map getSum $
                getZipList $
                  (ZipList (map Sum [1, 2, 3]) <> ZipList (map Sum [4, 5]))
            ),
      TestLabel "ZipList semigroup: product" $
        TestCase $
          assertEqual
            "take 5 $ map getProduct $ getZipList (ZipList (map Product [1..]) <> ZipList (map Product [0..])) == [0,2,6,12,20]"
            [0, 2, 6, 12, 20 :: Int]
            ( take 5 $
                map getProduct $
                  getZipList $
                    (ZipList (map Product [1 ..]) <> ZipList (map Product [0 ..]))
            ),
      --------------------------------------------------------------------------
      -- 17. Monoid ZipList: identity laws -------------------------------------
      --------------------------------------------------------------------------
      TestLabel "ZipList Monoid: left identity" $
        TestCase $
          let xs = ZipList (map Sum ([1, 2, 3] :: [Integer]))
              left = mempty <> xs
              want = xs
              -- compare only the prefix we care about
              got = take 3 $ map getSum $ getZipList left
              expct = take 3 $ map getSum $ getZipList want
           in assertEqual
                "mempty <> xs == xs"
                expct
                got,
      TestLabel "ZipList Monoid: right identity" $
        TestCase $
          let xs = ZipList (map Sum ([1, 2, 3] :: [Integer]))
              right = xs <> mempty
              want = xs
              got = take 3 $ map getSum $ getZipList right
              expct = take 3 $ map getSum $ getZipList want
           in assertEqual
                "xs <> mempty == xs"
                expct
                got,
      --------------------------------------------------------------------------
      -- 18. varianceF ----------------------------------------------------------
      --------------------------------------------------------------------------
      TestLabel "varianceF: variance of [1,2,3,4]" $
        TestCase $
          assertEqual
            "runFold varianceF [1,2,3,4] == 1.25"
            (1.25 :: Double)
            (runFold varianceF ([1, 2, 3, 4] :: [Double])),
      TestLabel "varianceF: variance of single element == 0" $
        TestCase $
          assertEqual
            "runFold varianceF [5] == 0"
            (0.0 :: Double)
            (runFold varianceF ([5] :: [Double])),
      TestLabel "varianceF: variance of [2,4]" $
        TestCase $
          assertEqual
            "runFold varianceF [2,4] == 1"
            (1.0 :: Double)
            (runFold varianceF ([2, 4] :: [Double])),
      --------------------------------------------------------------------------
      -- varianceF: empty input produces NaN -----------------------------------
      --------------------------------------------------------------------------
      TestLabel "varianceF: variance of [] is NaN" $
        TestCase $
          let result = runFold varianceF ([] :: [Double])
           in assertBool
                "runFold varianceF [] should be NaN"
                (isNaN result)
    ]

--------------------------------------------------------------------------------
-- Test runner -----------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT tests
