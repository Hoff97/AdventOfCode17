module Main where

import           Day5

main :: IO ()
main = do
  a <- readFile "input/day5.txt"
  print $ solution1 a
  print $ solution2 a
