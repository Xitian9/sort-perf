{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sort" #-}
module Main where

import Test.Tasty.Bench ( bench, bgroup, nf, Benchmark, bcompare, defaultMain, locateBenchmark )
import Test.Tasty.QuickCheck
import System.Random (randomRIO)

import qualified Sorts.New as New
import qualified Sorts.Old as Old


import Control.Monad (replicateM)
import Control.DeepSeq (NFData)

import Test.Tasty.Providers (TestTree, singleTest)
import Data.List hiding (sort, sortBy, sortOn)
import Test.Tasty (testGroup)
import Data.Semigroup (Arg(..))

import ComparisonProvider (ComparisonTest(..))
import Data.Data (Typeable)
import Test.Tasty.Patterns.Printer (printAwkExpr)

baseline :: String
baseline = "Old"

type ComparisonFunction a b = a -> b
type SortOn a b = ComparisonFunction a b -> [a] -> [a]

sizes :: [Int]
-- sizes = replicate 20 3
-- sizes = replicate 10 1_000_000
sizes = [ 1, 5, 25, 100, 1000, 10_000, 100_000, 1_000_000 ]

sorts :: Ord b => Show a => [(String, SortOn a b, SortOn a b)]
sorts =
  [ ("Old", Old.sortWith, Old.sortOn)
  , ("New", New.sortWith, New.sortOn)
  ]

main :: IO ()
main = do
  -- sizes <- readLn
  tData <- mapM benchmark sizes
  defaultMain (testAll : tData)

testAll :: TestTree
testAll = testGroup "List tests"
  [ makeTest "correctness" (isCorrect @Int)
  , makeTest "stability" (isStable @Int) ]

makeTest :: (Ord a, Arbitrary b, Show b, Show a) => String -> (([a] -> [a]) -> [b] -> Property) -> TestTree
makeTest name f = testGroup name $ map (\(n, sortWith, _) -> testProperty n $ f (sortWith id)) sorts

isStable :: Ord a => ([Arg a Int] -> [Arg a Int]) -> [a] -> Property
isStable sort xs = let result = sort (zipWith Arg xs [0..])
  in property $ isAscending result

isAscending :: (Eq a, Ord b) => [Arg a b] -> Bool
isAscending [] = True
isAscending [_] = True
isAscending ((Arg x1 i1) : a@((Arg x2 i2) : _))
  | x1 == x2  = i1 < i2 && isAscending a
  | otherwise = isAscending a

-- isCorrect :: Ord a => ([a] -> [a]) -> [a] -> Bool
isCorrect :: Ord a => ([a] -> [a]) -> [a] -> Property
isCorrect sort xs = let res = sort xs in isSorted res .&&. sameElems res xs
  where sameElems x y = null (x \\ y) && null (y \\ x)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)


benchmark :: Int -> IO Benchmark
benchmark size = do
  _data <- randoms size
  let name = show size ++ " Elements"
      randomSort  = bgroup' "sortOn" name snd _data id
      randomSort2  = bgroup'' "sortWith" name snd _data id
      minimumElem = bgroup' "min by sortOn" name snd _data (take 1)
      minimumElem2 = bgroup'' "min by sortWith" name snd _data (take 1)
      -- comparisons = testGroup "comparisons" (makeComps _data)
  pure $ bgroup name [randomSort, randomSort2, minimumElem, minimumElem2] -- comparisons


bgroup' :: (NFData b, Ord o, Show a) => String -> String -> (a -> o) -> [a] -> ([a] -> b) -> Benchmark
bgroup' str prev proj _data f = bgroup str $ makeBenchSortOn [str, prev] proj _data f

bgroup'' :: (NFData b, Ord o, Show a) => String -> String -> (a -> o) -> [a] -> ([a] -> b) -> Benchmark
bgroup'' str prev proj _data f = bgroup str $ makeBenchSortWith [str, prev] proj _data f

makeBenchSortOn :: (NFData b, Ord o, Show a) => [String] -> (a -> o) -> [a] -> ([a] -> b) -> [Benchmark]
makeBenchSortOn strs proj _data f = forSorts (\name _ sortOn -> compBench strs name $ bench name (nf (f . sortOn proj) _data))

makeBenchSortWith :: (NFData b, Ord o, Show a) => [String] -> (a -> o) -> [a] -> ([a] -> b) -> [Benchmark]
makeBenchSortWith strs proj _data f = forSorts (\name sortWith _ -> compBench strs name $ bench name (nf (f . sortWith proj) _data))

makeComps :: (Ord a, NFData a, Typeable a, Show a) => [a] -> [TestTree]
makeComps _data = forSorts (\name sortWith _ -> singleTest name (ComparisonTest _data sortWith id))

forSorts :: Ord o => Show a => (String -> SortOn a o -> SortOn a o -> b) -> [b]
forSorts f = map (\(n, x, y) -> f n x y) sorts

compBench :: [String] -> String -> Benchmark -> Benchmark
compBench strs name
  | name == baseline = id
  | otherwise        = bcompare $ printAwkExpr (locateBenchmark $ baseline : strs)

randoms :: Int -> IO [(Integer, Integer)]
randoms n = do
    fsts <- replicateM n $ randomRIO (0, 10_000)
    snds <- replicateM n $ randomRIO (0, 10_000)
    pure $ zip fsts snds
