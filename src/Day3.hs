module Day3 where

import           Data.Array

sumT :: Num a => (a,a) -> a
sumT (x,y) = abs x + abs y

solution1 x = sumT . (!!x) . map snd . iterate next $ start

data Dir = U | L | D | R deriving (Enum,Show)

left :: Dir -> Dir
left = toEnum . (`mod` 4) . (+1) . fromEnum

go :: Dir -> (Int,Int) -> (Int,Int)
go L (a,b) = (a-1,b)
go R (a,b) = (a+1,b)
go U (a,b) = (a,b+1)
go D (a,b) = (a,b-1)

turns x = [(x,x),(-x,x),(-x,-x),(x,-x+1)]

next :: (Dir,(Int,Int)) -> (Dir,(Int,Int))
next (d,(x,y))
  | (x,y) `elem` turns (max (abs x) (abs y)) = (left d,go (left d) (x,y))
  | otherwise = (d,go d (x,y))

start :: (Dir,(Int,Int))
start = (D,(0,0))

walk :: (a -> (Dir,(Int,Int)) -> a) -> a -> (Dir,(Int,Int)) -> [(a,Dir,(Int,Int))]
walk f a g = (n,d,p):walk f n (next g)
  where
    n = f a g
    (d,p) = g

dim = 10
start2 = listArray ((-dim,-dim),(dim,dim)) [0 | a <- [-dim..dim], b <- [-dim..dim]]

neighbours (i,j) = [(i+a,j+b) | a <- [-1,0,1], b <- [-1,0,1], abs a + abs b /= 0]

computeNext arr (_,(i,j))
  | i==0 && j==0 = arr//[((i,j),1)]
  | otherwise = arr//[((i,j),sum $ map valAt (neighbours (i,j)))]
    where
      valAt (a,b) = arr!(a,b)

solution2 x = head . filter (>x) . map (\(arr,_,pos) ->  arr!pos) . walk computeNext start2 $ start

t (_,_,a) = a
