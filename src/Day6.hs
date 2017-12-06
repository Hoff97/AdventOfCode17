module Day6 where

import           Data.Array
import Debug.Trace

split x [] = [[]]
split x (y:ys)
  | x==y = [] : split x ys
  | otherwise = case split x ys of
      []   -> [[]]
      z:zs -> (y:z):zs

reallocate :: Array Int Int -> Array Int Int
reallocate arr = listArray (0,l-1) [if (j-i-1) `mod` l < r then (arr!j)+c+1 else if i /= j then (arr!j)+c else c | j <- [0..l-1]]
  where
    (i,max,l) = foldl (\(i,v,j) a -> if a>v then (j,a,j+1) else (i,v,j+1)) (0,0,0) arr
    n = arr!i
    c = n `div` l
    r = n `mod` l

walk1 i ls s
  | s `elem` ls = i
  | otherwise = walk1 (i+1) (s:ls) (reallocate s)

solution1 = walk1 0 [] . (\xs -> listArray (0,length xs-1) xs) . map read . split '\t'

test = "0\t7\t2\t0"

walk2 i ls s
  | s `elem` ls = s
  | otherwise = walk2 (i+1) (s:ls) (reallocate s)

solution2 = walk1 0 [] . walk2 0 [] . (\xs -> listArray (0,length xs-1) xs) . map read . split '\t'
