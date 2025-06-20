module Main where

import Data.List (tails, isPrefixOf, permutations)
import Control.Monad (when)

readFastaFile :: FilePath -> IO [String]
readFastaFile filename = fmap lines (readFile filename)

everySecondLine :: [String] -> [String]
everySecondLine entries = [x | (x,i) <- zip entries [0..], odd i]

findOverlap :: String -> String -> Int
findOverlap str1 str2 = maximum [length pre | pre <- tails str1, pre `isPrefixOf` str2]

compareLines :: Int -> Int -> [String] -> Int
compareLines start current lines
  | current > length lines = -1
  | otherwise = findOverlap (lines !! start) (lines !! current)

-- Get substring from position i with length n
substring :: Int -> Int -> String -> String
substring i n str = take n (drop i str)

iterativeCompare :: Int -> [Int] -> [(String, String)] -> String
iterativeCompare counter overlaps lines
    | counter >= length lines = ""
    | otherwise = fst (lines !! counter) ++ 
                 substring 0 (overlaps !! counter) (snd (lines !! counter)) ++
                 iterativeCompare (counter+1) overlaps lines

combinePairAtLocation :: (String, String) -> Int -> String
combinePairAtLocation (s1, s2) overlap = s1 ++ drop overlap s2

combinePairs :: [(String, String)] -> [Int] -> [String]
combinePairs pairs overlaps = [combinePairAtLocation pair overlap | (pair, overlap) <- zip pairs overlaps]

pairs :: [String] -> [(String, String)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

main :: IO ()
main = do
  long_lines <- readFastaFile "rosalind-long.fasta"

  let lines' = everySecondLine long_lines

  -- print lines'

  -- print $ iterativeCompare 0 1 4 lines'
  let myPairs = pairs lines'

  let overlaps = map (uncurry findOverlap) myPairs

  print overlaps
  print myPairs

  print $ combinePairs myPairs overlaps