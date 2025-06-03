{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- Stand-alone test-suite for the MultiSet module
module Main where

import Data.List (sort)
import Data.Map qualified as Map
import HW4
import MultiSet
import Test.HUnit

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Build a multiset from a list by repeated 'insert'.
mkMS :: (Ord a) => [a] -> MultiSet a
mkMS = foldl' (flip insert) empty

-- | Assert that two multisets have the same counts for all elements in a list.
assertSameCounts :: (Ord a, Show a) => [a] -> MultiSet a -> MultiSet a -> Assertion
assertSameCounts xs ms1 ms2 = mapM_ check xs
  where
    check x =
      assertEqual
        ("count " ++ show x)
        (count x ms1)
        (count x ms2)

--------------------------------------------------------------------------------
-- 1. Tests for 'empty' ---------------------------------------------------------
--------------------------------------------------------------------------------

emptyTests :: Test
emptyTests = TestLabel "empty multiset" $ TestCase $ do
  assertBool
    "member on empty is False"
    (not $ member 'x' (empty :: MultiSet Char))
  assertEqual
    "count on empty is 0"
    0
    (count (42 :: Int) (empty :: MultiSet Int))

--------------------------------------------------------------------------------
-- 2. Tests for 'member' --------------------------------------------------------
--    (original tests kept, grouped here) ---------------------------------------
--------------------------------------------------------------------------------

memberTests :: Test
memberTests =
  TestList
    [ TestLabel "member: empty" $
        TestCase $
          assertBool
            "should be False on empty"
            (not $ member (42 :: Int) empty),
      TestLabel "member: present element" $
        TestCase $
          let ms = insert 'a' empty
           in assertBool "member 'a' should be True" (member 'a' ms),
      TestLabel "member: absent element" $
        TestCase $
          let ms = insert 'a' empty
           in assertBool "member 'b' should be False" (not $ member 'b' ms),
      TestLabel "member: several distinct elements" $
        TestCase $ do
          let ms = mkMS ['x', 'y', 'z']
          mapM_
            (\c -> assertBool ("member '" ++ [c] ++ "'") (member c ms))
            ['x', 'y', 'z']
          assertBool
            "member 'q' should be False"
            (not $ member 'q' ms),
      TestLabel "member: many duplicates" $
        TestCase $
          let ms = foldl' (\m _ -> insert 'k' m) empty ([1 .. 5] :: [Int])
           in assertBool "member 'k' after many inserts" (member 'k' ms)
    ]

--------------------------------------------------------------------------------
-- 3. Tests for 'insert' --------------------------------------------------------
--------------------------------------------------------------------------------

insertTests :: Test
insertTests = TestLabel "insert" $ TestCase $ do
  let ms0 = empty :: MultiSet Char
      ms1 = insert 'p' ms0 -- {p×1}
      ms2 = insert 'p' ms1 -- {p×2}
      ms3 = insert 'q' ms2 -- {p×2, q×1}
  assertBool "insert adds membership" (member 'p' ms1)
  assertEqual "count after first insert == 1" 1 (count 'p' ms1)
  assertEqual "count after duplicate == 2" 2 (count 'p' ms2)
  assertEqual "other element unchanged" 2 (count 'p' ms3)
  assertEqual "new element count == 1" 1 (count 'q' ms3)

--------------------------------------------------------------------------------
-- 4. Tests for 'count' ---------------------------------------------------------
--------------------------------------------------------------------------------

countTests :: Test
countTests = TestLabel "count" $ TestCase $ do
  let ms = mkMS [1, 2, 1, 3, 1, 2] :: MultiSet Int -- {1×3, 2×2, 3×1}
  assertEqual "count 1 == 3" 3 (count 1 ms)
  assertEqual "count 2 == 2" 2 (count 2 ms)
  assertEqual "count 3 == 1" 1 (count 3 ms)
  assertEqual "count 4 == 0" 0 (count 4 ms)

--------------------------------------------------------------------------------
-- 5. Tests for 'remove' --------------------------------------------------------
--------------------------------------------------------------------------------

removeTests :: Test
removeTests = TestLabel "remove" $ TestCase $ do
  -- 6.1 removing from an empty multiset is a no-op
  let msEmpty = remove 'x' (empty :: MultiSet Char)
  assertBool
    "remove on empty leaves it empty"
    (not $ member 'x' msEmpty)

  -- 6.2 removing the only occurrence eliminates membership
  let ms1 = insert 'a' empty -- {a×1}
      ms1' = remove 'a' ms1 -- {}
  assertBool "element no longer member after remove" (not $ member 'a' ms1')
  assertEqual "count becomes 0" 0 (count 'a' ms1')

  -- 6.3 removing one occurrence from many decrements multiplicity by 1
  let ms2 = mkMS (replicate 3 'b') -- {b×3}
      ms2' = remove 'b' ms2 -- {b×2}
  assertEqual "count decremented" 2 (count 'b' ms2')

  -- 6.4 removing an absent element keeps all existing counts unchanged
  let baseElems = [1, 2, 3, 2] -- {1×1, 2×2, 3×1}
      ms3 = mkMS baseElems :: MultiSet Int
      ms3' = remove 99 ms3 -- 99 not present
      check e =
        assertEqual
          ("count of " ++ show e ++ " unchanged")
          (count e ms3)
          (count e ms3')
  mapM_ check [1, 2, 3] -- verify each original element
  assertEqual "still absent element" 0 (count 99 ms3')

--------------------------------------------------------------------------------
-- 6. Tests for 'fromList' ------------------------------------------------------
--------------------------------------------------------------------------------

fromListTests :: Test
fromListTests =
  TestList
    [ TestLabel "fromList: empty list" $
        TestCase $ do
          let ms = fromList ([] :: [Int])
          assertBool "no members in empty multiset" (not $ member 1 ms)
          assertEqual "count of any element == 0" 0 (count 1 ms),
      TestLabel "fromList: unique elements" $
        TestCase $ do
          let elems = ['a', 'b', 'c']
              ms = fromList elems
          mapM_
            ( \c -> do
                assertBool ("member '" ++ [c] ++ "'") (member c ms)
                assertEqual ("count '" ++ [c] ++ "' == 1") 1 (count c ms)
            )
            elems
          assertBool "absent element" (not $ member 'z' ms),
      TestLabel "fromList: duplicates respected" $
        TestCase $ do
          let xs = [1, 1, 2, 3, 3, 3] :: [Int]
              ms = fromList xs
          assertEqual "count 1 == 2" 2 (count 1 ms)
          assertEqual "count 2 == 1" 1 (count 2 ms)
          assertEqual "count 3 == 3" 3 (count 3 ms)
          assertEqual "count 4 == 0" 0 (count 4 ms),
      TestLabel "fromList behaves like fold-insert" $
        TestCase $ do
          let xs = "multiset test!"
              msA = fromList xs
              msB = mkMS xs
          assertSameCounts xs msA msB
    ]

--------------------------------------------------------------------------------
-- 7. Tests for 'toList' --------------------------------------------------------
--------------------------------------------------------------------------------

toListTests :: Test
toListTests =
  TestList
    [ TestLabel "toList: empty" $
        TestCase $
          assertEqual
            "empty multiset -> []"
            ([] :: [Int])
            (toList (empty :: MultiSet Int)),
      TestLabel "toList: unique elements" $
        TestCase $ do
          let xs = ['a', 'b', 'c']
              ms = fromList xs
          assertEqual
            "same elements (order-insensitive)"
            (sort xs)
            (sort (toList ms)),
      TestLabel "toList: duplicates preserved" $
        TestCase $ do
          let xs = [1, 1, 2, 3, 3, 3] :: [Int]
              ms = fromList xs
          assertEqual
            "exact multiplicities"
            (sort xs)
            (sort (toList ms)),
      TestLabel "toList ∘ fromList round-trip" $
        TestCase $ do
          let xs = "multiset roundtrip!"
              ms = fromList xs
          -- Verify counts survive the round-trip without relying on Eq MultiSet
          assertSameCounts xs ms (fromList (toList ms))
    ]

-- --------------------------------------------------------------------------------
-- -- 8. Tests for Eq / Show / Semigroup / Monoid instances -----------------------
-- --------------------------------------------------------------------------------

eqShowSemigroupMonoidTests :: Test
eqShowSemigroupMonoidTests =
  TestList
    [ eqEqualTest,
      eqNotEqualTest,
      showConsistentTest,
      showEmptyTest,
      semigroupCombineTest,
      semigroupAssocTest,
      monoidIdentityTest
    ]
  where
    -- Eq ----------------------------------------------------------------------
    eqEqualTest =
      TestLabel "Eq: equal multisets" $
        TestCase $ do
          let msA = fromList "abcc" -- {a×1,b×1,c×2}
              msB = foldl' (flip insert) empty "ccba"
          assertBool "msA == msB" (msA == msB)

    eqNotEqualTest =
      TestLabel "Eq: unequal multisets" $
        TestCase $ do
          let msA = mkMS [1, 2] :: MultiSet Int
              msB = mkMS [1, 1] :: MultiSet Int
          assertBool "msA /= msB" (msA /= msB)

    -- Show --------------------------------------------------------------------
    -- Property: equal multisets render to identical strings.
    showConsistentTest =
      TestLabel "Show: identical for equal multisets" $
        TestCase $ do
          let xs = [3, 2, 2, 1] :: [Int]
              ms1 = fromList xs
              ms2 = mkMS xs
          assertEqual "show ms1 == show ms2" (show ms1) (show ms2)

    -- Concrete check on an easy case (empty).
    showEmptyTest =
      TestLabel "Show: empty multiset is printed like Set.empty" $
        TestCase $
          assertEqual "show mempty" "{}" (show (mempty :: MultiSet Int))

    -- Semigroup ---------------------------------------------------------------
    semigroupCombineTest =
      TestLabel "Semigroup: combines counts" $
        TestCase $ do
          let ms1 = fromList [1, 1, 2] :: MultiSet Int
              ms2 = fromList [2, 3] :: MultiSet Int
              ms = ms1 <> ms2 -- expected {1×2,2×2,3×1}
          assertEqual "count 1 == 2" 2 (count 1 ms)
          assertEqual "count 2 == 2" 2 (count 2 ms)
          assertEqual "count 3 == 1" 1 (count 3 ms)

    semigroupAssocTest =
      TestLabel "Semigroup: associativity" $
        TestCase $ do
          let a = mkMS "a" :: MultiSet Char
              b = mkMS "bb"
              c = mkMS "ccc"
          assertEqual
            "associativity"
            ((a <> b) <> c)
            (a <> (b <> c))

    -- Monoid ------------------------------------------------------------------
    monoidIdentityTest =
      TestLabel "Monoid: mempty identity" $
        TestCase $ do
          let m = mkMS [7, 8] :: MultiSet Int
          assertBool "m <> mempty == m" (m <> mempty == m)
          assertBool "mempty <> m == m" (mempty <> m == m)

--------------------------------------------------------------------------------
-- 9. Show instance – simple literal check -------------------------------------
--------------------------------------------------------------------------------

showLiteralTest :: Test
showLiteralTest =
  TestLabel "Show: concrete rendering" $
    TestCase $ do
      -- build the multiset {1,1,2}
      let ms = insert 1 . insert 1 . insert 2 $ (empty :: MultiSet Int)
      assertEqual
        "show should produce \"{1,1,2}\""
        "{1,1,2}"
        (show ms)

-- 10. Show instance – Semigroup example ---------------------------------------

semigroupShowLiteralTest :: Test
semigroupShowLiteralTest =
  TestLabel "Show: concrete rendering after (<> )" $
    TestCase $ do
      let ms1 = fromList [1, 1, 2] :: MultiSet Int
          ms2 = fromList [1, 3] :: MultiSet Int
          res = ms1 <> ms2 -- expected multiset {1,1,1,2,3}
      assertEqual
        "show should produce \"{1,1,1,2,3}\""
        "{1,1,1,2,3}"
        (show res)

--------------------------------------------------------------------------------
-- 11. Jsonable Bool round-trip -------------------------------------------------
--------------------------------------------------------------------------------

jsonBoolTests :: Test
jsonBoolTests =
  TestLabel "Jsonable Bool: round-trip" $
    TestCase $ do
      let samples = [True, False]
      mapM_
        ( \b ->
            assertEqual
              ("round-trip " ++ show b)
              (Just b)
              (fromJson . toJson $ b)
        )
        samples

--------------------------------------------------------------------------------
-- 12. Jsonable JString round-trip ---------------------------------------------
--------------------------------------------------------------------------------

jsonJStringTests :: Test
jsonJStringTests =
  TestLabel "Jsonable JString: round-trip" $
    TestCase $ do
      let samples =
            [ JString "", -- empty string
              JString "hello", -- simple ASCII
              JString "line\nbreak" -- embedded newline
            ]
      mapM_
        ( \s ->
            assertEqual
              ("round-trip " ++ show s)
              (Just s)
              (fromJson . toJson $ s)
        )
        samples

--------------------------------------------------------------------------------
-- 13. Jsonable Integer round-trip ---------------------------------------------
--------------------------------------------------------------------------------

jsonIntegerTests :: Test
jsonIntegerTests =
  TestLabel "Jsonable Integer: round-trip" $
    TestCase $ do
      let samples =
            [ 0,
              42,
              -123456,
              123456789012345678901234567890 -- big Integer
            ] ::
              [Integer]
      mapM_
        ( \n ->
            assertEqual
              ("round-trip " ++ show n)
              (Just n)
              (fromJson . toJson $ n)
        )
        samples

--------------------------------------------------------------------------------
-- 14. Jsonable Double round-trip ----------------------------------------------
--------------------------------------------------------------------------------

jsonDoubleTests :: Test
jsonDoubleTests =
  TestLabel "Jsonable Double: round-trip" $
    TestCase $ do
      let samples =
            [ 0.0,
              -0.0,
              3.141592653589793,
              -12345.6789,
              1.0e100,
              2.5e-12
            ] ::
              [Double]
      mapM_
        ( \d ->
            assertEqual
              ("round-trip " ++ show d)
              (Just d)
              (fromJson . toJson $ d)
        )
        samples

--------------------------------------------------------------------------------
-- 15. Jsonable (a, b) round-trip ----------------------------------------------
--------------------------------------------------------------------------------

jsonTupleTests :: Test
jsonTupleTests =
  TestLabel "Jsonable (a,b): round-trip" $
    TestCase $ do
      let samples =
            [ (0 :: Integer, True),
              (42 :: Integer, False),
              (-7 :: Integer, True)
            ] ::
              [(Integer, Bool)]
      mapM_
        ( \t ->
            assertEqual
              ("round-trip " ++ show t)
              (Just t)
              (fromJson . toJson $ t)
        )
        samples

--------------------------------------------------------------------------------
-- 16. Jsonable (a, b, c) round-trip -------------------------------------------
--------------------------------------------------------------------------------

jsonTripleTests :: Test
jsonTripleTests =
  TestLabel "Jsonable (a,b,c): round-trip" $
    TestCase $ do
      let samples =
            [ (0 :: Integer, True, JString ""),
              (42 :: Integer, False, JString "hi"),
              (-7 :: Integer, True, JString "שלום")
            ] ::
              [(Integer, Bool, JString)]
      mapM_
        ( \t ->
            assertEqual
              ("round-trip " ++ show t)
              (Just t)
              (fromJson . toJson $ t)
        )
        samples

--------------------------------------------------------------------------------
-- 17. Jsonable Maybe a round-trip ---------------------------------------------
--------------------------------------------------------------------------------

jsonMaybeTests :: Test
jsonMaybeTests =
  TestLabel "Jsonable Maybe a: round-trip" $
    TestCase $ do
      let samples =
            [ Nothing,
              Just (0 :: Integer),
              Just (-42 :: Integer)
            ] ::
              [Maybe Integer]
      mapM_
        ( \m ->
            assertEqual
              ("round-trip " ++ show m)
              (Just m)
              (fromJson . toJson $ m)
        )
        samples

--------------------------------------------------------------------------------
-- 18. Jsonable Either l r round-trip ------------------------------------------
--------------------------------------------------------------------------------

jsonEitherTests :: Test
jsonEitherTests =
  TestLabel "Jsonable Either l r: round-trip" $
    TestCase $ do
      let samples =
            [ Left (0 :: Integer),
              Left (-99 :: Integer),
              Right (JString ""),
              Right (JString "hello")
            ] ::
              [Either Integer JString]
      mapM_
        ( \e ->
            assertEqual
              ("round-trip " ++ show e)
              (Just e)
              (fromJson . toJson $ e)
        )
        samples

--------------------------------------------------------------------------------
-- 19. Jsonable [a] round-trip --------------------------------------------------
--------------------------------------------------------------------------------

jsonListTests :: Test
jsonListTests =
  TestLabel "Jsonable [a]: round-trip" $
    TestCase $ do
      let samples =
            [ [],
              [0],
              [1, 2, 3],
              [-5, 0, 9999]
            ] ::
              [[Integer]]
      mapM_
        ( \xs ->
            assertEqual
              ("round-trip " ++ show xs)
              (Just xs)
              (fromJson . toJson $ xs)
        )
        samples

--------------------------------------------------------------------------------
-- 20. Jsonable MultiSet a round-trip ------------------------------------------
--------------------------------------------------------------------------------

jsonMultiSetTests :: Test
jsonMultiSetTests =
  TestLabel "Jsonable MultiSet a: round-trip" $
    TestCase $ do
      let samples =
            [ empty,
              fromList [1 :: Integer],
              fromList [1, 1, 2],
              fromList [3, 2, 2, 1]
            ] ::
              [MultiSet Integer]
      mapM_
        ( \ms ->
            assertEqual
              ("round-trip " ++ show ms)
              (Just ms)
              (fromJson . toJson $ ms)
        )
        samples

--------------------------------------------------------------------------------
-- 21. Jsonable Matrix a round-trip --------------------------------------------
--------------------------------------------------------------------------------

jsonMatrixTests :: Test
jsonMatrixTests =
  TestLabel "Jsonable Matrix a: round-trip" $
    TestCase $ do
      let samples =
            [ Matrix [],
              Matrix [[1]],
              Matrix [[1, 2], [3, 4]],
              Matrix [[-1, 0, 2], [5, 6, 7], [8, 9, 10]]
            ] ::
              [Matrix Integer]
      mapM_
        ( \m ->
            assertEqual
              ("round-trip " ++ show m)
              (Just m)
              (fromJson . toJson $ m)
        )
        samples

--------------------------------------------------------------------------------
-- 22. Jsonable SparseMatrix a round-trip --------------------------------------
--------------------------------------------------------------------------------

jsonSparseMatrixTests :: Test
jsonSparseMatrixTests =
  TestLabel "Jsonable SparseMatrix a: round-trip" $
    TestCase $ do
      let samples =
            [ SparseMatrix 0 0 Map.empty,
              SparseMatrix 2 2 (Map.fromList [((0, 0), 1 :: Integer)]),
              SparseMatrix
                3
                3
                ( Map.fromList
                    [ ((0, 2), 5 :: Integer),
                      ((2, 1), -7 :: Integer)
                    ]
                )
            ] ::
              [SparseMatrix Integer]
      mapM_
        ( \sm ->
            assertEqual
              ("round-trip " ++ show sm)
              (Just sm)
              (fromJson . toJson $ sm)
        )
        samples

--------------------------------------------------------------------------------
-- 23. Jsonable Tree a round-trip ----------------------------------------------
--------------------------------------------------------------------------------

jsonTreeTests :: Test
jsonTreeTests =
  TestLabel "Jsonable Tree a: round-trip" $
    TestCase $ do
      let samples =
            [ Empty,
              Tree Empty (1 :: Integer) Empty,
              Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty),
              Tree (Tree (Tree Empty 1 Empty) 2 Empty) 3 Empty
            ] ::
              [Tree Integer]
      mapM_
        ( \t ->
            assertEqual
              ("round-trip " ++ show t)
              (Just t)
              (fromJson . toJson $ t)
        )
        samples

--------------------------------------------------------------------------------
-- 24. Complex nested Jsonables round-trip -------------------------------------
--------------------------------------------------------------------------------

-- | A nested type that exercises *every* Json constructor:
--   JsonNull (Nothing), JsonObject (Either & Tree), JsonArray (3-tuple),
--   JsonInt / JsonString / JsonBool / JsonDouble for leaves.
type Complex =
  Maybe -- JsonNull for Nothing
    ( Either -- JsonObject with "left"/"right"
        (Integer, JString, Bool) -- JsonArray of 3 different scalar Jsons
        (Tree Double) -- JsonObject of a Tree with Double values
    )

jsonComplexNestedTests :: Test
jsonComplexNestedTests =
  TestLabel "Jsonable complex nested structure: round-trip" $
    TestCase $ do
      let samples ::
            [Complex]
          samples =
            [ Nothing,
              Just (Left (42, JString "hello", True)),
              Just (Left (-1, JString "", False)),
              Just (Right (Tree Empty 3.14 Empty)),
              Just (Right (Empty :: Tree Double))
            ]
      mapM_
        ( \c ->
            assertEqual
              ("round-trip " ++ show c)
              (Just c)
              (fromJson . toJson $ c)
        )
        samples

--------------------------------------------------------------------------------
-- 24. Complex mega-nested Jsonables round-trip (all constructors) -------------
--------------------------------------------------------------------------------

-- | Triple that still covers every Json constructor.
--
--   1st component  = pair      (Maybe (Either …), [Maybe Bool])
--   2nd component  = Tree      (Either Integer Double)
--   3rd component  = SparseMatrix (Maybe Int)
type ComplexMega =
  ( ( Maybe (Either Integer JString), -- JsonNull / JsonObject / JsonString / JsonInt
      [Maybe Bool] -- JsonArray of JsonBool / JsonNull
    ),
    Tree (Either Integer Double), -- JsonObject (Tree) with JsonInt / JsonDouble
    SparseMatrix (Maybe Integer) -- JsonObject with Maybe Int entries
  )

jsonComplexMegaTests :: Test
jsonComplexMegaTests =
  TestLabel "Jsonable mega nested structure: round-trip" $
    TestCase $ do
      let sm1 = SparseMatrix 1 1 (Map.fromList [((0, 0), Just 7)])
          sm2 = SparseMatrix 0 0 Map.empty
          samples :: [ComplexMega]
          samples =
            [ ( ( Nothing,
                  [Just True, Nothing]
                ),
                Tree
                  Empty
                  (Left 42)
                  (Tree Empty (Right 3.14) Empty),
                sm1
              ),
              ( ( Just (Right (JString "hi")),
                  []
                ),
                Empty,
                sm2
              )
            ]
      mapM_
        ( \x ->
            assertEqual
              ("round-trip " ++ show x)
              (Just x)
              (fromJson . toJson $ x)
        )
        samples

--------------------------------------------------------------------------------
-- 25. “PDF” examples round-trip ------------------------------------------------
--------------------------------------------------------------------------------

-- | Reproduce the interactive examples shown in the PDF/screenshot.
pdfExampleTests :: Test
pdfExampleTests =
  TestLabel "Jsonable PDF examples: round-trip" $
    TestCase $ do
      -- 1. Explicit tuple round-trip
      let tup =
            (JString "abc", 3 :: Integer) :: (JString, Integer)
      assertEqual
        "tuple round-trip"
        (Just tup)
        (fromJson . toJson $ tup)

      -- 2. thereAndBackAgain helper
      let thereAndBackAgain :: (Jsonable a) => a -> Maybe a
          thereAndBackAgain = fromJson . toJson

      -- a) scalar Integer
      assertEqual
        "thereAndBackAgain 1"
        (Just (1 :: Integer))
        (thereAndBackAgain (1 :: Integer))

      -- b) Matrix value
      let mat = Matrix [[1, 2], [3, 4]] :: Matrix Integer
      assertEqual
        "thereAndBackAgain Matrix"
        (Just mat)
        (thereAndBackAgain mat)

--------------------------------------------------------------------------------
-- 26. Num Bool (F₂) behaviour --------------------------------------------------
--------------------------------------------------------------------------------

numBoolTests :: Test
numBoolTests =
  TestLabel "Num Bool behaves like F2" $
    TestCase $ do
      -- Addition (⊕  / XOR)
      assertEqual "T + F = T" True (True + False)
      assertEqual "F + T = T" True (False + True)
      assertEqual "T + T = F" False (True + True)
      assertEqual "F + F = F" False (False + False)

      -- Subtraction ≡ addition in F₂
      assertEqual "T - T = F" False (True - True)
      assertEqual "T - F = T" True (True - False)

      -- Multiplication (∧ / AND)
      assertEqual "T * T = T" True (True * True)
      assertEqual "T * F = F" False (True * False)
      assertEqual "F * F = F" False (False * False)

      -- abs and signum are identity
      mapM_
        ( \b -> do
            assertEqual ("abs " ++ show b) b (abs b)
            assertEqual ("signum " ++ show b) b (signum b)
        )
        [False, True]

      -- fromInteger is parity (odd → True, even → False)
      assertEqual "fromInteger 0" False (fromInteger 0 :: Bool)
      assertEqual "fromInteger 2" False (fromInteger 2 :: Bool)
      assertEqual "fromInteger 5" True (fromInteger 5 :: Bool)
      assertEqual "fromInteger -3" True (fromInteger (-3) :: Bool)

--------------------------------------------------------------------------------
-- 27. Num Bool – PDF examples --------------------------------------------------
--------------------------------------------------------------------------------

numBoolPdfTests :: Test
numBoolPdfTests =
  TestLabel "Num Bool PDF examples" $
    TestCase $ do
      -- Taken verbatim from the slide / screenshot.
      assertEqual "True + True  = False" False (True + True)
      assertEqual "True - False = True" True (True - False)
      assertEqual "9   :: Bool  = True" True (9 :: Bool)
      assertEqual "42  :: Bool  = False" False (42 :: Bool)

--------------------------------------------------------------------------------
-- 28. Semigroup MatrixSum ------------------------------------------------------
--------------------------------------------------------------------------------

matrixSumTests :: Test
matrixSumTests =
  TestLabel "Semigroup MatrixSum" $
    TestCase $ do
      -- helpers
      let m :: [[Integer]] -> Matrix Integer
          m = Matrix

          ms :: [[Integer]] -> MatrixSum Integer
          ms = MatrixSum . m

      -- concrete matrices
      let ms1 = ms [[1, 2], [3, 4]]
          ms2 = ms [[5, 6], [7, 8]]
          ms3 = ms [[1, 1], [1, 1]]

      -- element-wise sum expectation
      let expected = ms [[6, 8], [10, 12]]

      -- 1. basic combination behaves like element-wise addition
      assertEqual "ms1 <> ms2" expected (ms1 <> ms2)

      -- 2. associativity law  (x <> y) <> z == x <> (y <> z)
      assertEqual
        "associativity"
        ((ms1 <> ms2) <> ms3)
        (ms1 <> (ms2 <> ms3))

      -- 3. Edge-case: empty matrices
      let emptyMS = ms [] -- Matrix []
      -- empty <> empty = empty
      assertEqual "empty <> empty = empty" emptyMS (emptyMS <> emptyMS)

--------------------------------------------------------------------------------
-- 29. Semigroup MatrixMult (standard matrix-matrix multiplication) ------------
--------------------------------------------------------------------------------

matrixMultTests :: Test
matrixMultTests =
  TestLabel "Semigroup MatrixMult" $
    TestCase $ do
      -- Helpers
      let m :: [[Integer]] -> Matrix Integer
          m = Matrix

          mm :: [[Integer]] -> MatrixMult Integer
          mm = MatrixMult . m

      -- Two concrete 2×2 matrices
      let a = mm [[1, 2], [3, 4]]
          b = mm [[5, 6], [7, 8]]

      -- Expected product  a · b
      --  [[1*5+2*7 , 1*6+2*8],
      --   [3*5+4*7 , 3*6+4*8]]
      let expected = mm [[19, 22], [43, 50]]

      -- 1. Basic multiplication
      assertEqual "a <> b (matrix product)" expected (a <> b)

      -- 2. Associativity law  (a·b)·c == a·(b·c)
      let c =
            mm
              [ [2, 0],
                [1, 2]
              ]
      assertEqual
        "associativity"
        ((a <> b) <> c)
        (a <> (b <> c))

      -- 3. Edge-case: empty matrices
      let emptyMM = mm [] -- Matrix []
      -- empty <> empty = empty
      assertEqual "empty <> empty = empty" emptyMM (emptyMM <> emptyMM)

--------------------------------------------------------------------------------
-- 30. Semigroup SparseMatrixSum (entry-wise addition) --------------------------
--------------------------------------------------------------------------------

sparseMatrixSumTests :: Test
sparseMatrixSumTests =
  TestLabel "Semigroup SparseMatrixSum" $
    TestCase $ do
      -- helper to build a SparseMatrix quickly
      let sm ::
            Integer ->
            -- \^ rows
            Integer ->
            -- \^ cols
            [((Integer, Integer), Integer)] ->
            -- \^ non-zero entries
            SparseMatrix Integer
          sm r c es = SparseMatrix r c (Map.fromList es)

          sms ::
            Integer ->
            Integer ->
            [((Integer, Integer), Integer)] ->
            SparseMatrixSum Integer
          sms r c es = SparseMatrixSum (sm r c es)

      -- two 2×2 sparse matrices with overlapping & distinct entries
      let s1 = sms 2 2 [((0, 0), 3), ((1, 1), 4)] -- ⎡3 0⎤
      let s2 = sms 2 2 [((0, 0), 5), ((0, 1), 1)] -- ⎡5 1⎤
      -- ⎣0 0⎦
      -- expected element-wise sum
      let expected = sms 2 2 [((0, 0), 8), ((0, 1), 1), ((1, 1), 4)]

      -- 1. basic combination
      assertEqual "s1 <> s2" expected (s1 <> s2)

      -- 2. associativity (s1 <> s2) <> s3 == s1 <> (s2 <> s3)
      let s3 = sms 2 2 [((1, 0), 2)]
      assertEqual
        "associativity"
        ((s1 <> s2) <> s3)
        (s1 <> (s2 <> s3))

      -- 3. edge case: empty sparse matrices (rows = cols = 0)
      let emptySMS = sms 0 0 []
      assertEqual
        "empty <> empty = empty"
        emptySMS
        (emptySMS <> emptySMS)

--------------------------------------------------------------------------------
-- 31. Semigroup SparseMatrixMult (matrix × matrix) ----------------------------
--------------------------------------------------------------------------------

sparseMatrixMultTests :: Test
sparseMatrixMultTests =
  TestLabel "Semigroup SparseMatrixMult" $
    TestCase $ do
      -- helper to build SparseMatrix and wrapper
      let sm ::
            Integer -> -- rows
            Integer -> -- cols
            [((Integer, Integer), Integer)] -> -- non-zero entries
            SparseMatrix Integer
          sm r c es = SparseMatrix r c (Map.fromList es)

          smm ::
            Integer ->
            Integer ->
            [((Integer, Integer), Integer)] ->
            SparseMatrixMult Integer
          smm r c es = SparseMatrixMult (sm r c es)

      -- Two 2×2 matrices
      -- A = ⎡1 0⎤
      --     ⎣2 3⎦
      let a = smm 2 2 [((0, 0), 1), ((1, 0), 2), ((1, 1), 3)]
      -- B = ⎡0 4⎤
      --     ⎣5 6⎦
      let b = smm 2 2 [((0, 1), 4), ((1, 0), 5), ((1, 1), 6)]

      -- Expected A·B
      -- C = ⎡0  4 ⎤
      --     ⎣15 26⎦
      let expected = smm 2 2 [((0, 1), 4), ((1, 0), 15), ((1, 1), 26)]

      -- 1. Basic multiplication correctness
      assertEqual "a <> b" expected (a <> b)

      -- 2. Associativity  (a·b)·c == a·(b·c)
      -- Choose c = identity 2×2
      let c = smm 2 2 [((0, 0), 1), ((1, 1), 1)]
      assertEqual
        "associativity"
        ((a <> b) <> c)
        (a <> (b <> c))

      -- 3. Edge case: empty 0×0 sparse matrices
      let emptySMM = smm 0 0 []
      assertEqual
        "empty <> empty = empty"
        emptySMM
        (emptySMM <> emptySMM)

--------------------------------------------------------------------------------
-- 32. PDF-shown Semigroup cases ------------------------------------------------
--------------------------------------------------------------------------------

pdfSemigroupCasesTests :: Test
pdfSemigroupCasesTests =
  TestLabel "Semigroup cases from PDF" $
    TestCase $ do
      --------------------------------------------
      -- MatrixSum (Integer)  ms <> ms
      --------------------------------------------
      let m :: [[Integer]] -> Matrix Integer
          m = Matrix
          ms :: [[Integer]] -> MatrixSum Integer
          ms = MatrixSum . m

      let msBase = ms [[1, 2], [3, 4]]
          msExpected = ms [[2, 4], [6, 8]]
      assertEqual "MatrixSum integer" msExpected (msBase <> msBase)

      --------------------------------------------
      -- MatrixSum (Bool)  msb <> msb
      -- (Bool in F₂, so x ⊕ x = False)
      --------------------------------------------
      let msb :: [[Bool]] -> MatrixSum Bool
          msb = MatrixSum . Matrix
          msbBase = msb [[True, False], [False, True]]
          msbExpected = msb [[False, False], [False, False]]
      assertEqual "MatrixSum Bool (F2)" msbExpected (msbBase <> msbBase)

      --------------------------------------------
      -- MatrixMult (Integer)  mm1 <> mm2
      --------------------------------------------
      let mm :: [[Integer]] -> MatrixMult Integer
          mm = MatrixMult . m

      let mm1 = mm [[1, 0], [2, 1], [0, 1]] -- 3×2
          mm2 = mm [[1, 1, 2], [0, 2, 1]] -- 2×3
          mmExpected = mm [[1, 1, 2], [2, 4, 5], [0, 2, 1]] -- 3×3
      assertEqual "MatrixMult integer" mmExpected (mm1 <> mm2)

      --------------------------------------------
      -- SparseMatrixSum  sms1 <> sms2
      --------------------------------------------
      let sm r c es = SparseMatrix r c (Map.fromList es)
          sms r c es = SparseMatrixSum (sm r c es)

      let sms1 = sms 3 3 [((0, 0), 1), ((1, 1), 2)]
          sms2 = sms 3 3 [((0, 0), 3), ((2, 2), 4)]
          smsExpected =
            sms 3 3 ([((0, 0), 4), ((1, 1), 2), ((2, 2), 4)] :: [((Integer, Integer), Integer)])
      assertEqual "SparseMatrixSum" smsExpected (sms1 <> sms2)

      --------------------------------------------
      -- SparseMatrixMult  smm1 <> smm2
      --------------------------------------------
      let smm r c es = SparseMatrixMult (sm r c es)

      let smm1 = smm 2 2 [((0, 0), 1), ((0, 1), 2), ((1, 0), 3), ((1, 1), 4)]
          smm2 = smm 2 2 [((0, 0), 5), ((0, 1), 6), ((1, 0), 7), ((1, 1), 8)]
          smmExpected =
            smm 2 2 ([((0, 0), 19), ((0, 1), 22), ((1, 0), 43), ((1, 1), 50)] :: [((Integer, Integer), Integer)])
      assertEqual "SparseMatrixMult" smmExpected (smm1 <> smm2)

--------------------------------------------------------------------------------
-- 33. evalPoly examples from PDF ----------------------------------------------
--------------------------------------------------------------------------------

evalPolyTests :: Test
evalPolyTests =
  TestLabel "evalPoly (PDF examples)" $
    TestCase $ do
      -------------------------------------------------
      -- Numeric examples (Integer)
      -------------------------------------------------
      assertEqual
        "evalPoly [1,2,3] 2"
        (17 :: Integer)
        (evalPoly ([1, 2, 3] :: [Integer]) 2)

      assertEqual
        "evalPoly [] 5 == 0"
        (0 :: Integer)
        (evalPoly ([] :: [Integer]) 5)

      assertEqual
        "evalPoly [1,0,0,3] 2"
        (25 :: Integer)
        (evalPoly ([1, 0, 0, 3] :: [Integer]) 2)

      -------------------------------------------------
      -- Finite-field Bool examples
      -------------------------------------------------
      assertEqual
        "evalPoly [True,False] True"
        True
        (evalPoly [True, False] True)

      assertEqual
        "evalPoly [False] True"
        False
        (evalPoly [False] True)

      -------------------------------------------------
      -- Expression example
      -------------------------------------------------
      -- Expression helpers
      let lit = Lit :: Integer -> Expression Integer
          xVal = Lit 5

          expectedExpr1 =
            Plus
              (Mult (lit 2) (lit 1))
              ( Plus
                  (Mult (lit 3) xVal)
                  (lit 0)
              )

          expectedExpr2 =
            Plus
              (Mult (Lit 2) (Lit 1))
              ( Plus
                  (Mult (Lit 3) (Mult (Lit 5) (Lit 1)))
                  (Lit 0)
              )

      -- Test passes if result matches either expected expression
      let result = evalPoly [lit 2, lit 3] xVal
      assertBool
        "evalPoly [Lit 2, Lit 3] (Lit 5)"
        (result == expectedExpr1 || result == expectedExpr2)

--------------------------------------------------------------------------------
-- 34. pathsOfLengthK – PDF examples -------------------------------------------
--------------------------------------------------------------------------------

pathsOfLengthKTests :: Test
pathsOfLengthKTests =
  TestLabel "pathsOfLengthK (PDF examples)" $
    TestCase $ do
      -- adjacency matrix for graph  A→B, A→C, B→C, B→D, C→D
      -- nodes ordered A(0), B(1), C(2), D(3)
      let adj :: Matrix Int
          adj =
            Matrix
              [ [0, 1, 1, 0], -- A row
                [0, 0, 1, 1], -- B row
                [0, 0, 0, 1], -- C row
                [0, 0, 0, 0] -- D row
              ]

      -------------------------------------------------
      -- Example 1: length-1 path from B (1) to C (2)
      -------------------------------------------------
      assertEqual
        "pathsOfLengthK 1 1 2 adj"
        1
        (pathsOfLengthK 1 1 2 adj)

      -------------------------------------------------
      -- Example 2: length-2 paths from A (0) to D (3)
      -- Paths: A→B→D  and  A→C→D
      -------------------------------------------------
      assertEqual
        "pathsOfLengthK 2 0 3 adj"
        2
        (pathsOfLengthK 2 0 3 adj)

--------------------------------------------------------------------------------
-- 35. pathsOfLengthK – additional edge & sanity checks ------------------------
--------------------------------------------------------------------------------

pathsOfLengthKExtraTests :: Test
pathsOfLengthKExtraTests =
  TestLabel "pathsOfLengthK (extra cases)" $
    TestCase $ do
      -- Same adjacency matrix used earlier
      let adj :: Matrix Int
          adj =
            Matrix
              [ [0, 1, 1, 0], -- A(0)
                [0, 0, 1, 1], -- B(1)
                [0, 0, 0, 1], -- C(2)
                [0, 0, 0, 0] -- D(3)
              ]

      -- 1. Length-0 paths  (identity matrix behaviour)
      assertEqual "len-0, same node (C→C)" 1 (pathsOfLengthK 0 2 2 adj)
      assertEqual "len-0, different nodes (A→B)" 0 (pathsOfLengthK 0 0 1 adj)

      -- 2. Longer path count  (A→B→C→D) is the only length-3 path A→D
      assertEqual "len-3, A→D" 1 (pathsOfLengthK 3 0 3 adj)

      -- 3. Multiple hops starting inside the graph (B→D, length-2 via C)
      assertEqual "len-2, B→D" 1 (pathsOfLengthK 2 1 3 adj)

      -- 4. Node with no outgoing edges: D can’t reach anyone with len>0
      mapM_
        ( \dst ->
            assertEqual
              ("D→" ++ show dst ++ ", len-1")
              0
              (pathsOfLengthK 1 3 dst adj)
        )
        [0 .. 3 :: Int]

      -- 5. Degenerate “all-zero” adjacency matrix
      let zeroAdj = Matrix (replicate 3 (replicate 3 0)) :: Matrix Int
      assertEqual
        "zero graph, any len>0 gives 0"
        0
        (pathsOfLengthK 4 0 2 zeroAdj)

      -- 6. Truly empty adjacency matrix (0×0 graph)
      let emptyAdj = Matrix [] :: Matrix Int
      -- Even with k = 0 or k > 0, there are no paths at all.
      assertEqual "empty matrix, k = 0" 0 (pathsOfLengthK 0 0 0 emptyAdj)
      assertEqual "empty matrix, k = 3" 0 (pathsOfLengthK 3 0 0 emptyAdj)

--------------------------------------------------------------------------------
-- 36. hasPath – PDF examples ---------------------------------------------------
--------------------------------------------------------------------------------

hasPathTests :: Test
hasPathTests =
  TestLabel "hasPath (PDF examples)" $
    TestCase $ do
      -- Same adjacency matrix used earlier
      -- A(0)→B, A→C, B→C, B→D, C→D
      let adj :: Matrix Int
          adj =
            Matrix
              [ [0, 1, 1, 0],
                [0, 0, 1, 1],
                [0, 0, 0, 1],
                [0, 0, 0, 0]
              ]
      -- Example cases from the slide
      assertBool "hasPath 0 3 adj == True" (hasPath 0 3 adj)
      assertBool "hasPath 3 0 adj == False" (not $ hasPath 3 0 adj)

--------------------------------------------------------------------------------
-- 37. hasPath – additional edge & sanity checks -------------------------------
--------------------------------------------------------------------------------

hasPathExtraTests :: Test
hasPathExtraTests =
  TestLabel "hasPath (extra cases)" $
    TestCase $ do
      -- Reference adjacency matrix (A→B, A→C, B→C, B→D, C→D)
      let adj :: Matrix Int
          adj =
            Matrix
              [ [0, 1, 1, 0],
                [0, 0, 1, 1],
                [0, 0, 0, 1],
                [0, 0, 0, 0]
              ]

      -------------------------------------------------
      -- 1. Trivial self-paths (length 0) must exist
      -------------------------------------------------
      assertBool "A→A (len 0)" (hasPath 0 0 adj)
      assertBool "D→D (len 0 even for sink)" (hasPath 3 3 adj)

      -------------------------------------------------
      -- 2. More reachability checks
      -------------------------------------------------
      assertBool "A→C reachable" (hasPath 0 2 adj) -- via length-1
      assertBool "B→D reachable" (hasPath 1 3 adj) -- via length-1
      assertBool "C↛B unreachable" (not $ hasPath 2 1 adj)

      -------------------------------------------------
      -- 3. All-zero edges matrix (3×3)
      -------------------------------------------------
      let zero3 = Matrix (replicate 3 (replicate 3 0)) :: Matrix Int
      assertBool "zero matrix, no cross paths" (not $ hasPath 0 1 zero3)
      assertBool "zero matrix, self path still True" (hasPath 1 1 zero3)

      -------------------------------------------------
      -- 4. Truly empty matrix (0×0) – nothing reachable
      -------------------------------------------------
      let emptyM = Matrix [] :: Matrix Int
      assertBool "empty matrix has no nodes -> False" (not $ hasPath 0 0 emptyM)

--------------------------------------------------------------------------------
-- 38. simplify – PDF examples --------------------------------------------------
--------------------------------------------------------------------------------

simplifyPdfTests :: Test
simplifyPdfTests =
  TestLabel "simplify (PDF examples)" $
    TestCase $ do
      -----------------------------------------------------------------
      --  0 + x  →  x
      -----------------------------------------------------------------
      assertEqual
        "0 + x"
        (Iden "x")
        (simplify (Plus (Lit 0) (Iden "x")))

      -----------------------------------------------------------------
      --  y − 0  →  y
      -----------------------------------------------------------------
      assertEqual
        "y - 0"
        (Iden "y")
        (simplify (Minus (Iden "y") (Lit 0)))

      -----------------------------------------------------------------
      -- 1 * (3 + z)  →  3 + z
      -----------------------------------------------------------------
      assertEqual
        "1 * (3 + z)"
        (Plus (Lit 3) (Iden "z"))
        (simplify (Mult (Lit 1) (Plus (Lit 3) (Iden "z"))))

      -----------------------------------------------------------------
      -- 10 / 5  →  2
      -----------------------------------------------------------------
      assertEqual
        "10 / 5"
        (Lit 2)
        (simplify (Div (Lit 10) (Lit 5)))

      -----------------------------------------------------------------
      -- 10 / 0  →  remains symbolic
      -----------------------------------------------------------------
      assertEqual
        "10 / 0"
        (Div (Lit 10) (Lit 0))
        (simplify (Div (Lit 10) (Lit 0)))

      -----------------------------------------------------------------
      -- signum (-7)  →  -1
      -----------------------------------------------------------------
      assertEqual
        "signum (-7)"
        (Lit (-1))
        (simplify (Signum (Lit (-7))))

      -----------------------------------------------------------------
      -- signum (-3 * (-5 / w))
      --   →  (-1) * ( -1 / signum w )
      -----------------------------------------------------------------
      let expr =
            Signum
              ( Mult
                  (Lit (-3))
                  (Div (Lit (-5)) (Iden "w"))
              )
          expected =
            Mult
              (Lit (-1))
              (Div (Lit (-1)) (Signum (Iden "w")))
      assertEqual "signum product example" expected (simplify expr)

--------------------------------------------------------------------------------
-- 39. inlineExpressions – PDF examples ----------------------------------------
--------------------------------------------------------------------------------

inlineExpressionsPdfTests :: Test
inlineExpressionsPdfTests =
  TestLabel "inlineExpressions (PDF examples)" $
    TestCase $ do
      -------------------------------------------------
      -- Example 1
      -------------------------------------------------
      let ex1Input =
            [(Plus (Lit 2) (Lit 3), "a")]
          ex1Expect =
            [(Lit 5, "a")]
      assertEqual
        "example 1"
        ex1Expect
        (inlineExpressions ex1Input)

      -------------------------------------------------
      -- Example 2
      -------------------------------------------------
      let x = Iden "x"

          ex2Input =
            [ (Plus x (Lit 0), "e1"),
              (Mult (Plus x (Lit 0)) (Lit 2), "e2")
            ]
          ex2Expect =
            [ (Iden "x", "e1"),
              (Mult (Iden "e1") (Lit 2), "e2")
            ]
      assertEqual
        "example 2"
        ex2Expect
        (inlineExpressions ex2Input)

      -------------------------------------------------
      -- Example 3
      -------------------------------------------------
      let ex3Input =
            [ (Plus (Iden "x") (Iden "y"), "p"),
              (Plus (Iden "z") (Iden "w"), "q"),
              ( Mult
                  (Plus (Iden "x") (Iden "y"))
                  (Plus (Iden "z") (Iden "w")),
                "r"
              )
            ]
          ex3Expect =
            [ (Plus (Iden "x") (Iden "y"), "p"),
              (Plus (Iden "z") (Iden "w"), "q"),
              (Mult (Iden "p") (Iden "q"), "r")
            ]
      assertEqual
        "example 3"
        ex3Expect
        (inlineExpressions ex3Input)

--------------------------------------------------------------------------------
-- Master test list & runner ----------------------------------------------------
--------------------------------------------------------------------------------

tests :: Test
tests =
  TestList
    [ emptyTests,
      memberTests,
      insertTests,
      countTests,
      fromListTests,
      toListTests,
      eqShowSemigroupMonoidTests,
      showLiteralTest,
      semigroupShowLiteralTest,
      jsonBoolTests,
      jsonJStringTests,
      jsonIntegerTests,
      jsonDoubleTests,
      jsonTupleTests,
      jsonTripleTests,
      jsonMaybeTests,
      jsonEitherTests,
      jsonListTests,
      jsonMultiSetTests,
      jsonMatrixTests,
      jsonTreeTests,
      jsonComplexNestedTests,
      jsonComplexMegaTests,
      pdfExampleTests,
      numBoolTests,
      numBoolPdfTests,
      matrixSumTests,
      matrixMultTests,
      sparseMatrixSumTests,
      sparseMatrixMultTests,
      pdfSemigroupCasesTests,
      evalPolyTests,
      pathsOfLengthKTests,
      pathsOfLengthKExtraTests,
      hasPathTests,
      hasPathExtraTests,
      simplifyPdfTests,
      inlineExpressionsPdfTests
    ]

main :: IO Counts
main = runTestTT tests
