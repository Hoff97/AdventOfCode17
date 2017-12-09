module Day9 where

import           Debug.Trace

readGarbage :: String -> Int -> Bool -> Bool -> Int -> Int -> (Int,Int)
readGarbage [] l g i s sg                = (s,sg)
readGarbage (x:xs) l True True s sg      = readGarbage xs l True False s sg
readGarbage ('!':xs) l True False s sg   = readGarbage xs l True True s sg
readGarbage ('<':xs) l False _ s sg      = readGarbage xs l True False s sg
readGarbage ('>':xs) l True False s sg  = readGarbage xs l False False s sg
readGarbage (_:xs) l True False s sg    = readGarbage xs l True False s (sg+1)
readGarbage ('{':xs) l False False s sg  = readGarbage xs (l+1) False False s sg
readGarbage ('}':xs) l False False s sg  = readGarbage xs (l-1) False False (s+l) sg
readGarbage (',':xs) l False False s sg  = readGarbage xs l False False s sg
readGarbage ('\n':xs) l False False s sg = (s,sg)
readGarbage a b c d e f                = traceShow (a,b,c,d,e,f) (-1,-1)

solution1 xs = fst $ readGarbage xs 0 False False 0 0

solution2 xs = snd $ readGarbage xs 0 False False 0 0
