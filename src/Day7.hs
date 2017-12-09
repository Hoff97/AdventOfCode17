{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module Day7 where

import           Data.Array
import           Data.List  (nub)

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

data Tree a = Node a [Tree a] | Leaf a deriving (Show, Functor, Foldable)

newtype FT a b = FT (a,b)

instance Eq a => Eq (FT a b) where
  FT (a,b) == FT (c,d) = a==c

getI :: String -> [(String,Int,[String])]
getI = map row . map (split " -> ") . filter (/= "") . split "\n"
  where
    row [x]   = (s x!!0,read . init . drop 1 $ s x!!1,[])
    row [x,y] = (s x!!0,read . init . drop 1 $ s x!!1,split ", " y)
    row _     = ("",0,[])
    s x = split " " x

h :: [(String,Int,[String])] -> String
h i = head . filter (`notElem` m) . map (\(a,_,_) -> a) $ i
  where
    m = i >>= (\(_,_,a) -> a)

solution1 = h . getI

buildTree :: [(String,Int,[String])] -> Tree (String,Int)
buildTree ls = buildFrom top
  where
    top = h ls
    buildFrom x = if length ch == 0 then Leaf (n,i) else Node (n,i) $ buildFrom <$> ch
      where
        (n,i,ch) = head . filter (\(a,_,_) -> a==x) $ ls

balance :: Tree (String,Int) -> Int -> (String,Int,Int)
balance (Leaf (n,i)) s = (n,i,s)
balance (Node (n,i) ts) s = case nub $ zipWith (\t j -> FT (sum $ snd <$> t,j)) ts [0..] of
  []         -> (n,i,s)
  [FT (a,_)] -> (n,i,s-a*length ts)
  [FT (a,i'),FT (b,j')] -> if length (filter ((== a) . sum . fmap snd) ts) > 1
    then balance (ts!!j') a
    else balance (ts!!i') b


