module Day8 where

import           Data.Map   (Map, empty, insert, lookup)
import           Data.Maybe (fromMaybe)

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

data Comp = LT | GT | Eq | Neq | LTE | GTE
data Cond = Cond String Comp Int
data Stmt = Stmt String Bool Int Cond

getVar m n = fromMaybe 0 $ Data.Map.lookup n m

compFunc Day8.LT  = (<)
compFunc Day8.GT  = (>)
compFunc Day8.Eq  = (==)
compFunc Day8.Neq = (/=)
compFunc Day8.LTE = (<=)
compFunc Day8.GTE = (>=)

evalCond :: Map String Int -> Cond -> Bool
evalCond f (Cond n c i) = compFunc c (getVar f n) i

evalStmt :: Map String Int -> Stmt -> Map String Int
evalStmt f (Stmt s d i c)
  | evalCond f c = if d then insert s (getVar f s + i) f else insert s (getVar f s - i) f
  | otherwise = f

evalStmts :: Map String Int -> [Stmt] -> Map String Int
evalStmts f ls = foldl (evalStmt) f ls

readProg = fmap readStmt . filter (/= "") . split "\n"
readStmt x = Stmt (l!!0) (l!!1 == "inc") (read (l!!2)) c
  where
    s = split " if " x
    c = readCond (s!!1)
    l = split " " (s!!0)
readCond x = Cond (s!!0) (readComp (s!!1)) (read (s!!2))
  where
    s = split " " x
readComp "<"  = Day8.LT
readComp ">"  = Day8.GT
readComp "<=" = LTE
readComp ">=" = GTE
readComp "==" = Eq
readComp "!=" = Neq

vars x = varsS <$> x
  where
    varsS (Stmt v _ _ _) = v

solution1 x = maximum $ (getVar r <$> vars p)
  where
    p = readProg x
    r = evalStmts empty p

evalStmtMax :: (Map String Int,Int) -> Stmt -> (Map String Int,Int)
evalStmtMax (f,m) (Stmt s d i c)
  | evalCond f c = if d then (insert s (getVar f s + i) f,max m (getVar f s + i)) else (insert s (getVar f s - i) f,max m (getVar f s - i))
  | otherwise = (f,m)

evalStmtsMax :: (Map String Int,Int) -> [Stmt] -> (Map String Int,Int)
evalStmtsMax f ls = foldl (evalStmtMax) f ls

solution2 x = snd r
  where
    p = readProg x
    r = evalStmtsMax (empty,0) p
