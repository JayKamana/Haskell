import Data.List
import System.IO

add_evens :: [Int] -> Int
add_evens [] = 0
add_evens (xs) = sum [x | x <- xs, even x]

remove_next_to_last :: [Int] -> [Int]
remove_next_to_last (xs)
 | length xs < 2 = xs
 | otherwise = let (ys,zs) = splitAt (length xs -2) xs   in   ys ++ (tail zs)

 
pairs :: [Int] -> [(Int,Int)]
pairs [_] = []
pairs []  = []
pairs (x:y:xs) = (x, y) : pairs xs

min_max :: [Int] -> (Int, Int)
min_max [] = 0
min_max xs = ((minimum xs),(maximum xs))

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))


