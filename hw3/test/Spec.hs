{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW3.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.Enum (Bounded)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Set qualified as Set
import HW3
import Test.HUnit (Counts, Test (..), assertBool, assertEqual, runTestTT)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), IO, Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, head, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, show, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (/), (||))

singleTree :: a -> Tree a
singleTree x = Node Empty x Empty

tree :: Tree Int
tree =
  Node
    ( Node
        ( Node
            (singleTree 8)
            4
            (singleTree 9)
        )
        2
        ( Node
            (singleTree 10)
            5
            Empty
        )
    )
    1
    ( Node
        (singleTree 6)
        3
        (singleTree 7)
    )

pdfTree :: Tree Int
pdfTree =
  Node
    ( Node
        (Node (singleTree 8) 4 (singleTree 9))
        2
        (Node (singleTree 10) 5 Empty)
    )
    1
    (Node (singleTree 6) 3 (singleTree 7))

-- Full tree from PDF page 5 image (used for 'Full' classification)
fullTreeImg :: Tree Int
fullTreeImg =
  Node
    ( Node
        (Node (singleTree 8) 4 (singleTree 9))
        2
        (singleTree 5)
    )
    1
    (singleTree 3)

-- Perfect tree (3 levels full) (used for 'Perfect' classification)
perfectTree3Levels :: Tree Int
perfectTree3Levels =
  Node
    (Node (singleTree 4) 2 (singleTree 5))
    1
    (Node (singleTree 6) 3 (singleTree 7))

-- Degenerate tree from PDF page 6 image (right skewed) (used for 'Degenerate')
degenerateTreePDF :: Tree Int
degenerateTreePDF = Node Empty (1 :: Int) (Node Empty 2 (singleTree (3 :: Int)))

-- Left skewed degenerate tree (another 'Degenerate' example)
degenerateTreeLeft :: Tree Int
degenerateTreeLeft = Node (Node (singleTree (3 :: Int)) 2 Empty) 1 Empty

-- Tree for FullAndComplete (but not Perfect)
fullAndCompleteTreeExample :: Tree Int
fullAndCompleteTreeExample =
  Node
    (Node (singleTree (4 :: Int)) 2 (singleTree (5 :: Int)))
    1
    (Node Empty 3 Empty)

-- Other tree (neither full, complete, perfect, nor degenerate)
otherTreeExample :: Tree Int
otherTreeExample =
  Node
    (Node (singleTree (4 :: Int)) 2 Empty)
    1
    (Node (singleTree (5 :: Int)) 3 Empty)

-- Example InfiniteLists for testing
natsInf :: InfiniteList Integer
natsInf = iiterate (+ 1) 0

onesInf :: InfiniteList Integer
onesInf = irepeat 1

iterateExampleInf :: InfiniteList Integer
iterateExampleInf = iiterate (\x -> x * x + x) 1

maze :: Maze
maze =
  Maze
    { width = 3,
      height = 3,
      layout =
        [ [Open, Open, Open],
          [Open, Blocked, Open],
          [Treasure, Open, Treasure]
        ]
    }

blockedMaze :: Maze
blockedMaze =
  Maze
    { width = 3,
      height = 3,
      layout =
        [ [Open, Open, Open],
          [Blocked, Blocked, Blocked],
          [Open, Open, Open]
        ]
    }

tests :: Test
tests =
  TestList
    [ TestLabel "treeSize: PDF example tree" $
        TestCase $
          assertEqual "treeSize pdfTree" 10 (treeSize pdfTree),
      TestLabel "treeSize: Empty tree" $
        TestCase $
          assertEqual "treeSize Empty" 0 (treeSize (Empty :: Tree Int)),
      TestLabel "treeSize: Single node tree" $
        TestCase $
          assertEqual "treeSize singleNode" 1 (treeSize (singleTree (1 :: Int))),
      TestLabel "treeSize: Left-skewed tree (3 nodes)" $
        TestCase $
          assertEqual "treeSize leftSkewed" 3 (treeSize (Node (Node (singleTree (3 :: Int)) 2 Empty) 1 Empty)),
      TestLabel "treeSize: Right-skewed tree (3 nodes)" $
        TestCase $
          assertEqual "treeSize rightSkewed" 3 (treeSize (Node Empty (1 :: Int) (Node Empty 2 (singleTree (3 :: Int))))),
      TestLabel "treeHeight: PDF example Empty" $
        TestCase $
          assertEqual "treeHeight Empty" 0 (treeHeight (Empty :: Tree Int)),
      TestLabel "treeHeight: PDF example tree" $
        TestCase $
          assertEqual "treeHeight pdfTree" 4 (treeHeight pdfTree),
      TestLabel "treeHeight: Single node tree" $
        TestCase $
          assertEqual "treeHeight singleNode" 1 (treeHeight (singleTree (1 :: Int))),
      TestLabel "treeHeight: Left-skewed tree (3 nodes)" $
        TestCase $
          assertEqual "treeHeight leftSkewed" 3 (treeHeight (Node (Node (singleTree (3 :: Int)) 2 Empty) 1 Empty)),
      TestLabel "treeHeight: Right-skewed tree (3 nodes)" $
        TestCase $
          assertEqual "treeHeight rightSkewed" 3 (treeHeight (Node Empty (1 :: Int) (Node Empty 2 (singleTree (3 :: Int))))),
      TestLabel "preOrder traversal" $
        TestCase $
          assertEqual
            "preOrder tree"
            [1, 2, 4, 8, 9, 5, 10, 3, 6, 7]
            (preOrderTraversal tree),
      TestLabel "inOrder traversal" $
        TestCase $
          assertEqual
            "inOrder tree"
            [8, 4, 9, 2, 10, 5, 1, 6, 3, 7]
            (inOrderTraversal tree),
      TestLabel "postOrder traversal" $
        TestCase $
          assertEqual
            "postOrder tree"
            [8, 9, 4, 10, 5, 2, 6, 7, 3, 1]
            (postOrderTraversal tree),
      TestLabel "classify: Empty tree is Perfect" $
        TestCase $
          assertEqual "classify Empty" Perfect (classify (Empty :: Tree Int)),
      TestLabel "classify: Single node tree is Perfect" $
        TestCase $
          assertEqual "classify singleNode" Perfect (classify (singleTree (1 :: Int))),
      TestLabel "classify: PDF Full tree example (fullTreeImg)" $
        TestCase $
          assertEqual "classify fullTreeImg" Full (classify fullTreeImg),
      TestLabel "classify: PDF Complete tree example (pdfTree)" $
        TestCase $
          assertEqual "classify pdfTree (Complete)" Complete (classify pdfTree),
      TestLabel "classify: Perfect tree example (perfectTree3Levels)" $
        TestCase $
          assertEqual "classify perfectTree3Levels" Perfect (classify perfectTree3Levels),
      TestLabel "classify: PDF Degenerate tree example (degenerateTreePDF)" $
        TestCase $
          assertEqual "classify degenerateTreePDF" Degenerate (classify degenerateTreePDF),
      TestLabel "classify: Degenerate tree example (degenerateTreeLeft)" $
        TestCase $
          assertEqual "classify degenerateTreeLeft" Degenerate (classify degenerateTreeLeft),
      TestLabel "classify: FullAndComplete example (fullAndCompleteTreeExample)" $
        TestCase $
          assertEqual "classify fullAndCompleteTreeExample" FullAndComplete (classify fullAndCompleteTreeExample),
      TestLabel "classify: Other tree example (otherTreeExample)" $
        TestCase $
          assertEqual "classify otherTreeExample" Other (classify otherTreeExample),
      TestLabel "isBalanced: Empty tree" $
        TestCase $
          assertEqual "isBalanced Empty" True (isBalanced (Empty :: Tree Int)),
      TestLabel "isBalanced: Single-node tree" $
        TestCase $
          assertEqual "isBalanced singleNode" True (isBalanced (singleTree (1 :: Int))),
      TestLabel "isBalanced: Complete tree example (pdfTree)" $
        TestCase $
          assertEqual "isBalanced pdfTree" True (isBalanced pdfTree),
      TestLabel "isBalanced: Full tree image example (fullTreeImg)" $
        TestCase $
          assertEqual "isBalanced fullTreeImg" False (isBalanced fullTreeImg),
      TestLabel "isBalanced: Perfect tree (perfectTree3Levels)" $
        TestCase $
          assertEqual "isBalanced perfectTree3Levels" True (isBalanced perfectTree3Levels),
      TestLabel "isBalanced: Right-skewed degenerate (degenerateTreePDF)" $
        TestCase $
          assertEqual "isBalanced degenerateTreePDF" False (isBalanced degenerateTreePDF),
      TestLabel "isBalanced: Left-skewed degenerate (degenerateTreeLeft)" $
        TestCase $
          assertEqual "isBalanced degenerateTreeLeft" False (isBalanced degenerateTreeLeft),
      TestLabel "isBalanced: FullAndComplete example (fullAndCompleteTreeExample)" $
        TestCase $
          assertEqual "isBalanced fullAndCompleteTreeExample" True (isBalanced fullAndCompleteTreeExample),
      TestLabel "isBalanced: Other tree example (otherTreeExample)" $
        TestCase $
          assertEqual "isBalanced otherTreeExample" True (isBalanced otherTreeExample),
      -- iiterate (implicitly tests :>)
      TestLabel "iiterate: PDF example" $
        TestCase $
          assertEqual "iiterate from PDF" ([1, 2, 6, 42, 1806] :: [Integer]) (sampleN 5 iterateExampleInf), -- Use sampleN 5 (smallSample)
      TestLabel "iiterate: Incrementing by 2 (natsInf style)" $
        TestCase $
          assertEqual "iiterate (+2) 0" ([0, 2, 4, 6, 8] :: [Integer]) (sampleN 5 (iiterate (+ 2) 0)),
      -- irepeat (implicitly tests :>)
      TestLabel "irepeat: PDF example" $
        TestCase $
          assertEqual "irepeat 1 from PDF" ([1, 1, 1, 1, 1] :: [Integer]) (sampleN 5 onesInf), -- Use sampleN 5 (smallSample)
      TestLabel "irepeat: Repeating a char" $
        TestCase $
          assertEqual "irepeat 'a'" (['a', 'a', 'a'] :: [Char]) (sampleN 3 (irepeat 'a')),
      -- itoList (tested via sampleN)
      -- No direct test needed if sampleN works, but we can add one for clarity
      TestLabel "itoList: first 5 naturals" $
        TestCase $
          assertEqual "itoList natsInf" [0, 1, 2, 3, 4] (take 5 (itoList natsInf)),
      -- sampleN
      TestLabel "sampleN: Take 3 from natsInf" $
        TestCase $
          assertEqual "sampleN 3 natsInf" [0, 1, 2] (sampleN 3 natsInf),
      TestLabel "sampleN: Take 0 from natsInf" $
        TestCase $
          assertEqual "sampleN 0 natsInf" [] (sampleN 0 natsInf),
      TestLabel "sampleN: Take 7 from onesInf" $
        TestCase $
          assertEqual "sampleN 7 onesInf" [1, 1, 1, 1, 1, 1, 1] (sampleN 7 onesInf),
      TestLabel "sampleN: Take negative (behaves like take)" $
        TestCase $
          assertEqual "sampleN -1 natsInf" [] (sampleN (-1) natsInf),
      -- sample (uses sampleN 10)
      TestLabel "sample: natsInf" $
        TestCase $
          assertEqual "sample natsInf" [0 .. 9] (sample natsInf),
      TestLabel "smallSample: natsInf" $
        TestCase $
          assertEqual "smallSample natsInf" [0 .. 4] (smallSample natsInf),
      TestLabel "smallSample: PDF iiterate example" $
        TestCase $
          assertEqual "smallSample iterateExampleInf" [1, 2, 6, 42, 1806] (smallSample iterateExampleInf),
      -- sample and smallSample examples from the hand-out
      TestLabel "sample $ irepeat 3" $
        TestCase $
          assertEqual
            "sample $ irepeat 3"
            ([3, 3, 3, 3, 3, 3, 3, 3, 3, 3] :: [Integer])
            (sample $ irepeat 3),
      TestLabel "smallSample $ irepeat 1" $
        TestCase $
          assertEqual
            "smallSample $ irepeat 1"
            ([1, 1, 1, 1, 1] :: [Integer])
            (smallSample $ irepeat 1),
      TestLabel "smallSample $ iiterate (\\x -> x * x + x) 1" $
        TestCase $
          assertEqual
            "smallSample $ iiterate (x*x+x) 1"
            ([1, 2, 6, 42, 1806] :: [Integer])
            (smallSample $ iiterate (\x -> x * x + x) 1),
      TestLabel "sample $ imap (* 3) natsInf" $
        TestCase $
          assertEqual
            "sample $ imap (* 3) naturals"
            ([0, 3, 6, 9, 12, 15, 18, 21, 24, 27] :: [Integer])
            (sample $ imap (* 3) natsInf),
      TestLabel "iconcat: PDF example iiterate map" $
        TestCase $
          assertEqual "iconcat iiterate map" ([1, 2, 3, 2, 3, 4, 3, 4, 5, 4] :: [Integer]) (sample (iconcat (iiterate (map (+ 1)) [1, 2, 3]))),
      TestLabel "iconcat: PDF example smallConcat" $
        TestCase $
          assertEqual "iconcat smallConcat" ([1, 2, 3, 4, 5] :: [Integer]) (sampleN 5 (iconcat ([1] :> [2] :> [3] :> [4] :> [5] :> irepeat []))),
      TestLabel "iconcat: Repeating list" $
        TestCase $
          assertEqual "iconcat repeating list" ([8, 9, 8, 9, 8, 9, 8] :: [Integer]) (sampleN 7 (iconcat (irepeat [8, 9]))),
      TestLabel "iconcat: Mixed lists" $
        TestCase $
          assertEqual "iconcat mixed lists" ([1, 2, 3, 4, 5, 4] :: [Integer]) (sampleN 6 (iconcat ([1, 2] :> [3] :> irepeat [4, 5]))),
      TestLabel "iconcat: Empty lists interspersed" $
        TestCase $
          assertEqual "iconcat empty lists interspersed" ([1, 2, 3, 4] :: [Integer]) (sampleN 4 (iconcat ([1] :> [] :> [2] :> [] :> [3] :> irepeat [4]))),
      TestLabel "grouped: smallSample $ grouped 3 natsInf" $
        TestCase $
          assertEqual "smallSample $ grouped 3 natsInf" ([[0, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11], [12, 13, 14]] :: [[Integer]]) (smallSample $ grouped 3 natsInf),
      TestLabel "reverseN: PDF example" $
        TestCase $
          assertEqual "reverseN 3 naturals" ([2, 1, 0, 5, 4, 3, 8, 7, 6, 11, 10, 9] :: [Integer]) (sampleN 12 (reverseN 3 naturals)),
      TestLabel "reverseN: Reverse by 1" $
        TestCase $
          assertEqual "reverseN 1 naturals" ([0, 1, 2, 3, 4] :: [Integer]) (sampleN 5 (reverseN 1 naturals)),
      TestLabel "reverseN: Reverse by 4" $
        TestCase $
          assertEqual "reverseN 4 naturals" ([3, 2, 1, 0, 7, 6, 5, 4] :: [Integer]) (sampleN 8 (reverseN 4 naturals)),
      TestLabel "reverseN: Reverse by 0 (identity)" $
        TestCase $
          assertEqual "reverseN 0 naturals" ([0, 1, 2, 3, 4] :: [Integer]) (sampleN 5 (reverseN 0 naturals)),
      TestLabel "smallSample $ sqrtInf 2" $
        TestCase $
          assertEqual "smallSample $ sqrtInf 2" ([2 % 1, 3 % 2, 17 % 12, 577 % 408, 665857 % 470832] :: [Rational]) (smallSample $ sqrtInf 2),
      TestLabel "longDivision: PDF 2 % 7" $
        TestCase $
          assertEqual "longDivision 2 % 7" "0.28571428" (sample $ longDivision (2 % 7)),
      TestLabel "longDivision: PDF 1 % 3 (smallSample in PDF, using sample here)" $
        TestCase $
          assertEqual "longDivision 1 % 3" "0.33333333" (sample (longDivision (1 % 3))), -- PDF used smallSample (5 chars: "0.333"), sample is 10
      TestLabel "longDivision: PDF 1 % 2 (smallSample in PDF, using sample here)" $
        TestCase $
          assertEqual "longDivision 1 % 2" "0.50000000" (sample (longDivision (1 % 2))), -- PDF used smallSample (5 chars: "0.500"), sample is 10
      TestLabel "longDivision: PDF 3 % 2 (smallSample in PDF, using sample here)" $
        TestCase $
          assertEqual "longDivision 3 % 2" "1.50000000" (sample (longDivision (3 % 2))), -- PDF used smallSample (5 chars: "1.500"), sample is 10
      TestLabel "longDivision: PDF (-3) % 2 (smallSample in PDF, using sample here)" $
        TestCase $
          assertEqual "longDivision (-3) % 2" "-1.5000000" (sample (longDivision ((-3) % 2))), -- PDF used smallSample (6 chars: "-1.50"), sample is 10
      TestLabel "longDivision: Zero (0 % 1)" $
        TestCase $
          assertEqual "longDivision 0 % 1" "0.00000000" (sample (longDivision (0 % 1))),
      TestLabel "longDivision: Integer (5 % 1)" $
        TestCase $
          assertEqual "longDivision 5 % 1" "5.00000000" (sample (longDivision (5 % 1))),
      TestLabel "longDivision: Negative Integer (-7 % 1)" $
        TestCase $
          assertEqual "longDivision -7 % 1" "-7.0000000" (sample (longDivision ((-7) % 1))),
      TestLabel "longDivision: Fraction 1 % 4 (terminating)" $
        TestCase $
          assertEqual "longDivision 1 % 4" "0.25000000" (sample (longDivision (1 % 4))),
      TestLabel "longDivision: Fraction 1 % 8 (terminating)" $
        TestCase $
          assertEqual "longDivision 1 % 8" "0.12500000" (sample (longDivision (1 % 8))),
      TestLabel "longDivision: Fraction 1 % 6 (repeating)" $
        TestCase $
          assertEqual "longDivision 1 % 6" "0.16666666" (sample (longDivision (1 % 6))),
      TestLabel "longDivision: Fraction 22 % 7 (Pi approximation)" $
        TestCase $
          assertEqual "longDivision 22 % 7" "3.14285714" (sample (longDivision (22 % 7))),
      TestLabel "longDivision: Fraction 1 % 9 (repeating)" $
        TestCase $
          assertEqual "longDivision 1 % 9" "0.11111111" (sample (longDivision (1 % 9))),
      TestLabel "longDivision: Fraction -1 % 3" $
        TestCase $
          assertEqual "longDivision -1 % 3" "-0.3333333" (sample (longDivision ((-1) % 3))),
      TestLabel "longDivision: Large numerator (12345 % 3)" $
        TestCase $
          assertEqual "longDivision 12345 % 3" "4115.00000" (sample (longDivision (12345 % 3))),
      TestLabel "longDivision: Large denominator (1 % 123)" $
        TestCase $
          assertEqual "longDivision 1 % 123" "0.00813008" (sample (longDivision (1 % 123))),
      -- Large 16-digit integer only
      TestLabel "longDivision: Large integer only (9999999999999999 % 1)" $
        TestCase $
          assertEqual
            "longDivision 9999999999999999 % 1"
            "9999999999"
            (sample $ longDivision (9999999999999999 % 1)),
      -- Large 16-digit denominator only
      TestLabel "longDivision: Large denominator only (1 % 9999999999999999)" $
        TestCase $
          assertEqual
            "longDivision 1 % 9999999999999999"
            "0.00000000"
            (sample $ longDivision (1 % 9999999999999999)),
      -- Nearly equal 19-digit numerator/denominator
      TestLabel "longDivision: Near-one fraction (1000000000000000001 % 1000000000000000000)" $
        TestCase $
          assertEqual
            "longDivision 1000000000000000001 % 1000000000000000000"
            "1.00000000"
            (sample $ longDivision (1000000000000000001 % 1000000000000000000)),
      -- Two 10-digit primes (repeating ≈ 0.999…)
      TestLabel "longDivision: Two 10-digit primes (1000000007 % 1000000009)" $
        TestCase $
          assertEqual
            "longDivision 1000000007 % 1000000009"
            "0.99999999"
            (sample $ longDivision (1000000007 % 1000000009)),
      -- 12-digit repeating sequence
      TestLabel "longDivision: 12-digit repeating (142857142857 % 999999999999)" $
        TestCase $
          assertEqual
            "longDivision 142857142857 % 999999999999"
            "0.14285714"
            (sample $ longDivision (142857142857 % 999999999999)),
      TestLabel "smallSample $ imap sample $ sqrtStrings 2" $
        TestCase $
          assertEqual
            "smallSample $ imap sample $ sqrtStrings 2"
            ["2.00000000", "1.50000000", "1.41666666", "1.41421568", "1.41421356" :: String]
            (smallSample $ imap sample $ sqrtStrings 2),
      TestLabel "cellAt (row0,col0)" $
        TestCase $
          assertEqual "cellAt maze (0,0)" (Just Open) (cellAt maze (CellPosition 0 0)),
      TestLabel "cellAt (row1,col1)" $
        TestCase $
          assertEqual "cellAt maze (1,1)" (Just Blocked) (cellAt maze (CellPosition 1 1)),
      TestLabel "cellAt (row2,col2)" $
        TestCase $
          assertEqual "cellAt maze (2,2)" (Just Treasure) (cellAt maze (CellPosition 2 2)),
      TestLabel "cellAt (row2,col0)" $
        TestCase $
          assertEqual "cellAt maze (2,0)" (Just Treasure) (cellAt maze (CellPosition 2 0)),
      TestLabel "cellAt (row0,col2)" $
        TestCase $
          assertEqual "cellAt maze (0,2)" (Just Open) (cellAt maze (CellPosition 0 2)),
      TestLabel "cellAt out-of-bounds (row3,col0)" $
        TestCase $
          assertEqual "cellAt maze (3,0)" Nothing (cellAt maze (CellPosition 3 0)),
      TestLabel "cellAt out-of-bounds (row0,col-1)" $
        TestCase $
          assertEqual "cellAt maze (0,-1)" Nothing (cellAt maze (CellPosition 0 (-1))),
      TestLabel "getAvailableMoves (row0,col0)" $
        TestCase $
          assertEqual "getAvailableMoves maze (0,0)" (Right [CellPosition 0 1, CellPosition 1 0]) (getAvailableMoves maze (CellPosition 0 0)),
      TestLabel "getAvailableMoves (row0,col1)" $
        TestCase $
          assertEqual "getAvailableMoves maze (0,1)" (Right [CellPosition 0 0, CellPosition 0 2]) (getAvailableMoves maze (CellPosition 0 1)),
      TestLabel "getAvailableMoves blocked cell (row1,col1)" $
        TestCase $
          assertEqual "getAvailableMoves maze (1,1)" (Left InvalidCell) (getAvailableMoves maze (CellPosition 1 1)),
      TestLabel "getAvailableMoves out-of-bounds (row-1,col42)" $
        TestCase $
          assertEqual "getAvailableMoves maze (-1,42)" (Left OutOfBounds) (getAvailableMoves maze (CellPosition (-1) 42)),
      -- Tests for shortestPath
      TestLabel "shortestPath maze (0,1) to (2,2)" $
        TestCase $
          assertEqual
            "shortestPath maze (0,1) (2,2)"
            ( Right
                [ CellPosition {row = 0, col = 2},
                  CellPosition {row = 1, col = 2}
                ]
            )
            (shortestPath maze (CellPosition 0 1) (CellPosition 2 2)),
      TestLabel "shortestPath maze (0,1) to (0,0)" $
        TestCase $
          assertEqual
            "shortestPath maze (0,1) (0,0)"
            (Right [])
            (shortestPath maze (CellPosition 0 1) (CellPosition 0 0)),
      TestLabel "shortestPath maze (0,1) to blocked cell (1,1)" $
        TestCase $
          assertEqual
            "shortestPath maze (0,1) (1,1)"
            (Left InvalidCell)
            (shortestPath maze (CellPosition 0 1) (CellPosition 1 1)),
      TestLabel "shortestPath maze (0,1) to out-of-bounds (3,2)" $
        TestCase $
          assertEqual
            "shortestPath maze (0,1) (3,2)"
            (Left OutOfBounds)
            (shortestPath maze (CellPosition 0 1) (CellPosition 3 2)),
      TestLabel "shortestPath blockedMaze (0,0) to (2,2) – no path" $
        TestCase $
          assertEqual
            "shortestPath blockedMaze (0,0) (2,2)"
            (Left NoPath)
            (shortestPath blockedMaze (CellPosition 0 0) (CellPosition 2 2)),
      TestLabel "treasureHunt: simple 3×3 example" $
        TestCase $
          let actual = treasureHunt maze (CellPosition {row = 0, col = 0})
              expected1 =
                Right
                  [ CellPosition {row = 0, col = 1},
                    CellPosition {row = 0, col = 2},
                    CellPosition {row = 1, col = 2},
                    CellPosition {row = 2, col = 2},
                    CellPosition {row = 2, col = 1},
                    CellPosition {row = 2, col = 0}
                  ]
              expected2 =
                Right
                  [ CellPosition {row = 1, col = 0},
                    CellPosition {row = 2, col = 0},
                    CellPosition {row = 2, col = 1},
                    CellPosition {row = 2, col = 2}
                  ]
           in assertBool
                "treasureHunt should collect all treasures (not necessarily optimally)"
                (actual == expected1 || actual == expected2)
    ]

main :: IO Counts
main = runTestTT tests
