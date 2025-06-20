module Main where

import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

readInputFile :: FilePath -> IO [String]
readInputFile filename = fmap lines (readFile filename)

longestDecreasing :: [Int] -> [Int]
longestDecreasing [] = []
longestDecreasing xs = buildSequence xs (fst maxIndex) [xs !! fst maxIndex]
  where
    n = length xs
    dp = array ((0,0), (n-1,n-1)) 
         [((i,j), compute i j) | i <- [0..n-1], j <- [0..n-1]]
    compute i j 
      | i >= j = 0
      | xs !! i > xs !! j = 1 + maximum [dp ! (j,k) | k <- [j+1..n-1]]
      | otherwise = maximum [dp ! (j,k) | k <- [j+1..n-1]]
    maxIndex = maximumBy (comparing snd) 
              [(i, maximum [dp ! (i,j) | j <- [i+1..n-1]]) | i <- [0..n-1]]
    buildSequence xs i acc
      | i >= n-1 = acc
      | otherwise = let next = snd $ maximumBy (comparing snd) 
                              [(j, dp ! (i,j)) | j <- [i+1..n-1]]
                   in buildSequence xs next (acc ++ [xs !! next])

longestIncreasing :: [Int] -> [Int]
longestIncreasing [] = []
longestIncreasing xs = buildSequence xs (fst maxIndex) [xs !! fst maxIndex]
  where
    n = length xs
    dp = array ((0,0), (n-1,n-1)) 
         [((i,j), compute i j) | i <- [0..n-1], j <- [0..n-1]]
    compute i j 
      | i >= j = 0
      | xs !! i < xs !! j = 1 + maximum [dp ! (j,k) | k <- [j+1..n-1]]
      | otherwise = maximum [dp ! (j,k) | k <- [j+1..n-1]]
    maxIndex = maximumBy (comparing snd) 
              [(i, maximum [dp ! (i,j) | j <- [i+1..n-1]]) | i <- [0..n-1]]
    buildSequence xs i acc
      | i >= n-1 = acc
      | otherwise = let next = snd $ maximumBy (comparing snd) 
                              [(j, dp ! (i,j)) | j <- [i+1..n-1]]
                   in buildSequence xs next (acc ++ [xs !! next])

main :: IO ()
main = do
  inputData <- readInputFile "rosalind-lgis-input.txt"

  -- Split on spaces (words) and convert to Ints (fmap read).
  let fields = fmap read (words (inputData !! 1)) :: [Int]

  putStrLn (unwords $ map show (longestIncreasing fields))
  putStrLn (unwords $ map show (longestDecreasing fields))
