module Day10 where

import Data.Bits
import Data.Char (ord)
import Data.Foldable
import Numeric (showHex)

newtype Xor = Xor Int
instance Monoid Xor where
  mappend (Xor a) (Xor b) = Xor (xor a b)
  mempty = Xor zeroBits
toInt (Xor a) = a

startsWith [] ys         = True
startsWith (x:xs) []     = False
startsWith (x:xs) (y:ys) = x==y && startsWith xs ys

split xs ys
  | startsWith xs ys = []:split xs (drop (length xs) ys)
  | otherwise = case ys of
      [] -> [[]]
      (y:ys) -> case split xs ys of
        s:ss -> (y:s):ss
        []   -> [[y]]

selectCircular :: [a] -> Int -> Int -> [a]
selectCircular xs s l = take l (drop s xs) ++ (take (l - (length xs - s) ) xs)

insertCircular :: [a] -> [a] -> Int -> [a]
insertCircular xs ls p = startI ++ take (p-lStartI) (drop lStartI xs) ++ endI ++ drop (p+length endI) xs
  where
    lenStart = length xs - p
    startI = drop lenStart ls
    lStartI = length startI
    endI = take lenStart ls

tie :: [a] -> Int -> Int -> [a]
tie xs p l = insertCircular xs (reverse $ selectCircular xs p l) p

group :: Int -> [a] -> [[a]]
group _ [] = []
group l xs = take l xs:group l (drop l xs)

walk :: [a] -> [Int] -> Int -> Int -> [a]
walk xs [] p s = xs
walk xs (l:ls) p s = walk (tie xs p l) ls ((p+l+s) `mod` length xs) (s+1)

walk2 :: [a] -> [Int] -> Int -> Int -> ([a],Int,Int)
walk2 xs [] p s = (xs,p,s)
walk2 xs (l:ls) p s = walk2 (tie xs p l) ls ((p+l+s) `mod` length xs) (s+1)

solution1 = (\xs -> xs!!0 * xs!!1) . (\ls -> walk [0..255] ls 0 0) . map read . split "," . (!!0) . split "\n"

solution2 = h . i64 . r
  where
    h = concat . map ((\a -> showHex a "") . toInt . foldMap Xor) . group 16
    i64 = ((\(a,_,_) -> a) . (!!64)) . (\ls -> iterate (\(xs,p,s) -> walk2 xs ls p s) ([0..255],0,0))
    r = (++ [17,31,73,47,23]) . map ord . (!!0) . split "\n"
