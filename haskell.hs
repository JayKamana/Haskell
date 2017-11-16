import Data.List
import System.IO

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

add_evens :: [Int] -> Int
add_evens [] = 0
add_evens (xs) = sum [x | x <- xs, even x]

remove_next_to_last :: [Int] -> [Int]
remove_next_to_last (xs)
 | length xs < 2 = xs
 | otherwise = let (ys,zs) = splitAt (length xs -2) xs   in   ys ++ (tail zs)

remove_next_to_last' :: [a] -> [a] 
remove_next_to_last' xs = (take ((length xs) - 2) xs) ++ (last xs) : []

pairs :: [a] -> [(a,a)]
pairs [_] = []
pairs []  = []
pairs (x:y:xs) = (x, y) : pairs xs

min_max :: [Int] -> (Int, Int)
min_max [] = (0,0)
min_max xs = ((minimum xs),(maximum xs))

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))


swap (x,y) = (y,x)

safeTail [] = []
safeTail (x:xs) = xs

double x = x * 2
 
safeTail2 xs | (null xs) = []
             | otherwise = tail xs

palindrome xs = reverse xs == xs

twice f x = f (f x)

removeSecond [] = []
removeSecond xs = (take 1 xs) ++ (drop 2 xs)

removeSecond' [] = []
removeSecond' (x:y:rest) = x:rest

removeSecond'' (x:xs) = x:tail xs

multiList n xs = map (\x -> n * x) xs
multiEvens n xs = map (\x -> if x `mod` 2 == 0 then n * x else x) xs

firsts xs = [fst x | x <- xs]
seconds xs = [b | (a,b) <- xs]

factorial 0 = 1
factorial n = n * factorial (n - 1)

average xs = sum xs `div` length xs

lastEle xs = drop ((length xs) - 1) xs

add :: Num a => a -> a -> a
add x y = x + y

add' :: Num a => (a, a) -> a
add' (a,b) = a + b

second xs = head (tail xs)

mystery a b = [(x,y) | y <- a, x <- b]

pair' xs ys = [(y,x) | x <- xs, y <- ys]

drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs) = drop' (n-1) xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                smaller = [a | a <- xs, a <= x]
                larger =  [b | b <- xs, b > x]

checkTrue [] = True
checkTrue (x:xs) = if x then checkTrue xs
                     else False

replicate' 0 item = []
replicate' n item = item : replicate' (n - 1) item

mrerge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

msort [] = []
msort [a] = [a]
msort list = merge (msort (take half list)) (msort (drop half list))
               where
                half = (length list) `div` 2

map_filter f p xs =  map f (filter p xs)

test = map_filter (\x -> x+1) (\y -> y<6) [1,9,3,7,5]

map' f = foldr (\x xs -> (f x):xs) []

test' = map' (\x -> x*2) [1,2,3,4]


filter' p = foldr (\x xs -> if (p x) then x:xs else xs)  []

test'' = filter' (\x -> x<6) [9,2,8,4]



size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

countEvens xs = length [x | x <- xs, even x]

countEvens' xs = length (filter (even) xs)


fibs 0 = 0
fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2)

fiblist x y = x:fiblist y (x+y)

fibonacci = fiblist 0 1

nth_fib i = fibonacci !! i

first_fib_greater_than x = head [y |y <- fibonacci, y>=x]