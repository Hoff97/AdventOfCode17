module Day1 where

zipSelfShift :: Int -> [a] -> [(a,a)]
zipSelfShift n xs = zip xs (drop n $ xs++xs)

solution1 = sum . map fst . filter (\(x,y) -> x==y) . zipSelfShift 1 . map (\x -> read [x])

solution2 xs = sum . map fst . filter (\(x,y) -> x==y) . zipSelfShift (length xs `div` 2) . map (\x -> read [x]) $ xs
