module Day4 where

import           Data.List (nub, sort)

split x [] = [[]]
split x (y:ys)
  | x==y = [] : split x ys
  | otherwise = case split x ys of
      []   -> [[]]
      z:zs -> (y:z):zs

valid1 xs = length (nub s) == length s
  where
    s = split ' ' xs

solution1 = length . filter valid1 . lines

valid2 xs = length (nub s) == length s
  where
    s = map sort $ split ' ' xs

solution2 = length . filter valid2 . lines
