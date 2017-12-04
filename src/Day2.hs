module Day2 where

split x [] = [[]]
split x (y:ys)
  | x==y = [] : split x ys
  | otherwise = case split x ys of
      []   -> [[]]
      z:zs -> (y:z):zs

solution1 = sum . map ((\x -> maximum x - minimum x) . map read . split '\t') . lines

solution2 = sum . map (combine . map read . split '\t') . lines
  where
    combine xs = sum [x `div` y | x <- xs, y <- xs, x `mod` y == 0 && x /= y]
