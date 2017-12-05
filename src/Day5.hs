module Day5 where

import           Data.Array

update i a []     = []
update 0 a (_:xs) = a:xs
update n a (x:xs) = x:update (n-1) a xs

walk1 :: Int -> Int -> Array Int Int -> Int
walk1 n a xs
  | n >= length xs = a
  | otherwise = walk1 (n+p) (a+1) (xs//[(n,p+1)])
  where
    p = xs!n

solution1 = walk1 0 0 . (\xs -> listArray (0,length xs-1) xs) . map read . filter (/= "") . lines

walk2 :: Int -> Int -> Array Int Int -> Int
walk2 n a xs
  | n >= length xs = a
  | otherwise = walk2 (n+p) (a+1) (xs//[(n,updated)])
  where
    p = xs!n
    updated = if p>=3 then p-1 else p+1

solution2 = walk2 0 0 . (\xs -> listArray (0,length xs-1) xs) . map read . filter (/= "") . lines
