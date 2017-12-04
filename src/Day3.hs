module Day3 where

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
