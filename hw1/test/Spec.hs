{-# OPTIONS_GHC -Wall -Werror -Wno-unused-imports #-}

module Main where

import HW1
  ( Generator,
    andAlso,
    anyGen,
    collatzLength,
    constGen,
    countDigits,
    divisors,
    emptyGen,
    foreverGen,
    hasLengthOfAtLeast,
    hasNext,
    integers,
    isArmstrong,
    isHappy,
    isPalindromicPrime,
    isPrime,
    lengthGen,
    nextGen,
    nextPrime,
    nthGen,
    primes,
    reverseDigits,
    sumDigits,
    sumGen,
    thd3,
  )
import Test.HUnit

-- A helper for quickly building a generator tuple
mkGen :: (a -> a) -> (a -> Bool) -> a -> Generator a
mkGen f g seed = (f, g, seed)

-- "positives" as described in the PDF: generates 1,2,3,... from seed=0
positives :: Generator Integer
positives = ((+ 1), const True, 0)

tests :: Test
tests =
  TestList
    [ ------------------------------------------------------------------------
      -- Section 2: Basic integer functions
      ------------------------------------------------------------------------
      -- TESTS 0-4
      TestLabel "countDigits 0 => 1" $
        TestCase $
          assertEqual "countDigits 0" (1 :: Integer) (countDigits 0),
      TestLabel "countDigits 1024 => 4" $
        TestCase $
          assertEqual "countDigits 1024" (4 :: Integer) (countDigits 1024),
      TestLabel "countDigits (-42) => 2" $
        TestCase $
          assertEqual "countDigits -42" (2 :: Integer) (countDigits (-42)),
      TestLabel "sumDigits 0 => 0" $
        TestCase $
          assertEqual "sumDigits 0" (0 :: Integer) (sumDigits 0),
      TestLabel "sumDigits 1024 => 7" $
        TestCase $
          assertEqual "sumDigits 1024" (7 :: Integer) (sumDigits 1024),
      -- TESTS 5-9
      TestLabel "sumDigits (-42) => 6" $
        TestCase $
          assertEqual "sumDigits -42" (6 :: Integer) (sumDigits (-42)),
      TestLabel "reverseDigits 1234 => 4321" $
        TestCase $
          assertEqual "reverseDigits 1234" (4321 :: Integer) (reverseDigits 1234),
      TestLabel "reverseDigits -42 => -24" $
        TestCase $
          assertEqual "reverseDigits -42" (-24 :: Integer) (reverseDigits (-42)),
      TestLabel "reverseDigits 120 => 21" $
        TestCase $
          assertEqual "reverseDigits 120" (21 :: Integer) (reverseDigits 120),
      TestLabel "collatzLength 1 => 0" $
        TestCase $
          assertEqual "collatzLength 1" (0 :: Integer) (collatzLength 1),
      -- TESTS 10-14
      TestLabel "collatzLength 2 => 1" $
        TestCase $
          assertEqual "collatzLength 2" (1 :: Integer) (collatzLength 2),
      TestLabel "collatzLength 3 => 7" $
        TestCase $
          assertEqual "collatzLength 3" (7 :: Integer) (collatzLength 3),
      TestLabel "collatzLength 4 => 2" $
        TestCase $
          assertEqual "collatzLength 4" (2 :: Integer) (collatzLength 4),
      TestLabel "collatzLength 1024 => 10" $
        TestCase $
          assertEqual "collatzLength 1024" (10 :: Integer) (collatzLength 1024),
      TestLabel "collatzLength 1025 => 36" $
        TestCase $
          assertEqual "collatzLength 1025" (36 :: Integer) (collatzLength 1025),
      --   ------------------------------------------------------------------------
      --   -- Section 3: Generators
      --   ------------------------------------------------------------------------

      -- TESTS 15-19
      TestLabel "nthGen 0 positives => 1" $
        TestCase $
          assertEqual "nthGen 0 positives" (1 :: Integer) (nthGen 0 positives),
      TestLabel "nthGen 2 positives => 3" $
        TestCase $
          assertEqual "nthGen 2 positives" (3 :: Integer) (nthGen 2 positives),
      TestLabel "nthGen -1 positives => 0" $
        TestCase $
          assertEqual "nthGen (-1) positives" (0 :: Integer) (nthGen (-1) positives),
      TestLabel "nthGen 42 ((+1), <10, 0) => 10" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 10) (0 :: Integer)
           in assertEqual "nthGen 42 g" (10 :: Integer) (nthGen 42 g),
      TestLabel "hasNext ((+1),(<=1),0) => True" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (<= 1) (0 :: Integer)
           in assertEqual "hasNext g" True (hasNext g),
      -- TESTS 20-24
      TestLabel "hasNext ((+1),(<=0),0) => True" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (<= 0) (0 :: Integer)
           in assertEqual "hasNext g" True (hasNext g),
      TestLabel "hasNext ((+1),(<=0),1) => False" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (<= 0) (1 :: Integer)
           in assertEqual "hasNext g" False (hasNext g),
      TestLabel "nextGen ((+1), (<0), 0) => seed becomes 1" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 0) (0 :: Integer)
              gNext = nextGen g
           in assertEqual "thd3 (nextGen g)" (1 :: Integer) (thd3 gNext),
      TestLabel "lengthGen ((+1), (<0), 0) => 0" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 0) (0 :: Integer)
           in assertEqual "lengthGen g" (0 :: Integer) (lengthGen g),
      TestLabel "lengthGen ((+1),(<=0),0) => 1" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (<= 0) (0 :: Integer)
           in assertEqual "lengthGen g" (1 :: Integer) (lengthGen g),
      -- TESTS 25-29
      TestLabel "lengthGen ((+1),(<10),0) => 10" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 10) (0 :: Integer)
           in assertEqual "lengthGen g" (10 :: Integer) (lengthGen g),
      TestLabel "hasLengthOfAtLeast 10 ((+1), (<10), 0) => True" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 10) (0 :: Integer)
           in assertEqual "hasLengthOfAtLeast 10 g" True (hasLengthOfAtLeast 10 g),
      TestLabel "hasLengthOfAtLeast 10 ((+1), (<9), 0) => False" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 9) (0 :: Integer)
           in assertEqual "hasLengthOfAtLeast 10 g" False (hasLengthOfAtLeast 10 g),
      TestLabel "hasLengthOfAtLeast 42 positives => True" $
        TestCase $
          assertEqual "hasLengthOfAtLeast 42 positives" True (hasLengthOfAtLeast 42 positives),
      TestLabel "nthGen 42 (constGen \"foobar\") => \"foobar\"" $
        TestCase $
          let g = constGen "foobar"
           in assertEqual "nthGen 42 constGen \"foobar\"" "foobar" (nthGen 42 g),
      -- TESTS 30-34
      TestLabel "foreverGen creates infinite generator" $
        TestCase $
          let g = foreverGen (+ 1) 0 -- equivalent to positives
           in assertEqual "nthGen 42 (foreverGen (+1) 0)" (42 :: Integer) (nthGen 41 g),
      TestLabel "emptyGen has length 0 (Int)" $
        TestCase $
          assertEqual "lengthGen (emptyGen :: Generator Int)" (0 :: Integer) (lengthGen (emptyGen :: Generator Int)),
      TestLabel "emptyGen has length 0 (Int,Int)" $
        TestCase $
          assertEqual "lengthGen (emptyGen :: Generator (Int,Int))" (0 :: Integer) (lengthGen (emptyGen :: Generator (Int, Int))),
      TestLabel "emptyGen has length 0 (Generator Int)" $
        TestCase $
          assertEqual "lengthGen (emptyGen :: Generator (Generator Int))" (0 :: Integer) (lengthGen (emptyGen :: Generator (Generator Int))),
      TestLabel "integers generates all non-zero integers" $
        TestCase $
          assertEqual "anyGen (==42) integers" True (anyGen (== 42) integers),
      -- TESTS 35-39
      TestLabel
        "integers generates negative numbers"
        $ TestCase
        $ assertEqual "anyGen (==(-42)) integers" True (anyGen (== (-42)) integers),
      TestLabel "sumGen ((+1),(<=1),0) => 3" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (<= 1) (0 :: Integer)
           in assertEqual "sumGen g" (3 :: Integer) (sumGen g),
      TestLabel "sumGen ((+1),(<=1),1) => 2" $
        TestCase $
          let g = mkGen (+ 1) (<= 1) (1 :: Integer)
           in assertEqual "sumGen g" (2 :: Integer) (sumGen g),
      TestLabel "sumGen ((+1), (<10), 0) => 55" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 10) (0 :: Integer)
           in assertEqual "sumGen g" (55 :: Integer) (sumGen g),
      TestLabel "sumGen ((+1),(<10),1) => 54" $
        TestCase $
          let g = mkGen (+ 1) (< 10) (1 :: Integer)
           in assertEqual "sumGen g" (54 :: Integer) (sumGen g),
      -- TESTS 40-44
      TestLabel "sumGen emptyGen => 0" $
        TestCase $
          let g = emptyGen :: Generator Integer
           in assertEqual "sumGen emptyGen" (0 :: Integer) (sumGen g),
      TestLabel "anyGen (>0) ((+1),(<=1),0) => True" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (<= 1) (0 :: Integer)
           in assertEqual "anyGen g" True (anyGen (> 0) g),
      TestLabel "anyGen (const True) ((+1), (<1), 1) => False" $
        TestCase $
          let g :: Generator Integer
              g = mkGen (+ 1) (< 1) (1 :: Integer)
           in assertEqual "anyGen g" False (anyGen (const True) g),
      TestLabel "anyGen (>42) positives => True (eventually >42)" $
        TestCase $
          assertEqual "anyGen (>42) positives" True (anyGen (> 42) positives),
      TestLabel "andAlso (<10) positives => lengthGen == 10" $
        TestCase $
          let g = andAlso (< 10) positives
           in assertEqual "lengthGen g" (10 :: Integer) (lengthGen g),
      -- TESTS 45-49
      TestLabel "andAlso (>20) (andAlso (<10) positives) => length 0" $
        TestCase $
          let g = andAlso (> 20) (andAlso (< 10) positives)
           in assertEqual "lengthGen g" (0 :: Integer) (lengthGen g),
      TestLabel "andAlso (<20) (andAlso (<10) positives) => length 10" $
        TestCase $
          let g = andAlso (< 20) (andAlso (< 10) positives)
           in assertEqual "lengthGen g" (10 :: Integer) (lengthGen g),
      --   ------------------------------------------------------------------------
      --   -- Section 4: Number properties
      --   ------------------------------------------------------------------------

      TestLabel "isPrime 1 => False" $
        TestCase $
          assertEqual "isPrime 1" False (isPrime 1),
      TestLabel "isPrime 2 => True" $
        TestCase $
          assertEqual "isPrime 2" True (isPrime 2),
      TestLabel "isPrime (-2) => False" $
        TestCase $
          assertEqual "isPrime -2" False (isPrime (-2)),
      -- TESTS 50-54
      TestLabel "isPrime 29 => True" $
        TestCase $
          assertEqual "isPrime 29" True (isPrime 29),
      TestLabel "nextPrime (-42) => 2" $
        TestCase $
          assertEqual "nextPrime -42" (2 :: Integer) (nextPrime (-42)),
      TestLabel "nextPrime 1 => 2" $
        TestCase $
          assertEqual "nextPrime 1" (2 :: Integer) (nextPrime 1),
      TestLabel "nextPrime 2 => 3" $
        TestCase $
          assertEqual "nextPrime 2" (3 :: Integer) (nextPrime 2),
      TestLabel "nextPrime 100 => 101" $
        TestCase $
          assertEqual "nextPrime 100" (101 :: Integer) (nextPrime 100),
      -- TESTS 55-59
      TestLabel "nthGen 0 primes => 2" $
        TestCase $
          assertEqual "nthGen 0 primes" (2 :: Integer) (nthGen 0 primes),
      TestLabel "nthGen 100 primes => 547" $
        TestCase $
          assertEqual "nthGen 100 primes" (547 :: Integer) (nthGen 100 primes),
      TestLabel "isHappy 7 => True" $
        TestCase $
          assertEqual "isHappy 7" True (isHappy 7),
      TestLabel "isHappy 42 => False" $
        TestCase $
          assertEqual "isHappy 42" False (isHappy 42),
      TestLabel "isHappy 130 => True" $
        TestCase $
          assertEqual "isHappy 130" True (isHappy 130),
      -- TESTS 60-64
      TestLabel "isHappy (-130) => True" $
        TestCase $
          assertEqual "isHappy -130" True (isHappy (-130)),
      TestLabel "isArmstrong 0 => True (assuming from the PDF examples)" $
        TestCase $
          assertEqual "isArmstrong 0" True (isArmstrong 0),
      TestLabel "isArmstrong 1 => True" $
        TestCase $
          assertEqual "isArmstrong 1" True (isArmstrong 1),
      TestLabel "isArmstrong 42 => False" $
        TestCase $
          assertEqual "isArmstrong 42" False (isArmstrong 42),
      TestLabel "isArmstrong 153 => True" $
        TestCase $
          assertEqual "isArmstrong 153" True (isArmstrong 153),
      -- TESTS 65-69
      TestLabel "isPalindromicPrime 2 => True" $
        TestCase $
          assertEqual "isPalindromicPrime 2" True (isPalindromicPrime 2),
      TestLabel "isPalindromicPrime 11 => True" $
        TestCase $
          assertEqual "isPalindromicPrime 11" True (isPalindromicPrime 11),
      TestLabel "isPalindromicPrime 13 => False" $
        TestCase $
          assertEqual "isPalindromicPrime 13" False (isPalindromicPrime 13),
      TestLabel "isPalindromicPrime 101 => True" $
        TestCase $
          assertEqual "isPalindromicPrime 101" True (isPalindromicPrime 101),
      TestLabel "sumGen (divisors 12) => 12" $
        TestCase $
          assertEqual "sumGen (divisors 12)" 16 (sumGen (divisors 12))
    ]

main :: IO Counts
main = runTestTT tests
