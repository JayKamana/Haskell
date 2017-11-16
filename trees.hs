data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

insert n Empty = Node Empty n Empty
insert n (Node left value right) = 
       if 
            n<value 
       then 
            (Node (insert n left) value right) 
       else 
             (Node left value (insert n right))

insertList [] = Empty
insertList (h:t) = insert h (insertList t)

size Empty = 0 
size (Node left value right) = 1 + (size left) + (size right)

r = Node (Node (Node Empty 3 Empty) 7 (Node Empty 6 Empty)) 2 (Node Empty 5 Empty)
s = Node (Node Empty 3 Empty) 5 (Node Empty 11 Empty)
t = Node (Node Empty 5 Empty) 10 (Node Empty 15 Empty)

find :: Eq a => a -> Tree a -> Bool
find m Empty = False
find m (Node l n r)
                    | m == n = True
                    | otherwise = find m l || find m r 

inorder Empty = []
inorder (Node l n r) = inorder l ++ [n] ++ inorder r

inner_nodes Empty = []
inner_nodes (Node l n r) = if l /= Empty || r /= Empty then inner_nodes l ++ [n] ++ inner_nodes r
                              else []
subset t1 t2 = [x | x <- (inorder t1), x `elem` (inorder t2)]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
removeDuplicates [] = []

union t1 t2 = removeDuplicates (inorder t1 ++ inorder t2)

max' t1 = maximum (inorder t1)
min' t1 = minimum (inorder t1)
