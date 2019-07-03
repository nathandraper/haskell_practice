data TreeData a = Null | Data a
    deriving Show

data Tree a = Leaf (TreeData a) | Node (TreeData a) (Tree a) (Tree a)
    deriving Show


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs  = merge (msort (take n xs)) (msort (drop n xs))
            where n = length xs `div` 2

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort xs  = qsort smaller ++ [h] ++ qsort larger
            where h = head xs
                  smaller = [x | x <- tail xs, x <= h]
                  larger = [x | x <- tail xs, x > h]

consTree :: Ord a => [a] -> Tree a
consTree [] = Leaf Null
consTree [x] = Leaf (Data x)
consTree xs = Node (Data middle) (consTree left) (consTree right)
              where midpoint = length xs `div` 2
                    middle = xs !! midpoint
                    left = take midpoint xs
                    right =  drop (midpoint + 1) xs

sortAndTree :: Ord a => [a] -> Tree a
sortAndTree = consTree.msort 

verify :: Ord a => Tree a -> Bool
verify (Leaf _) = True
verify (Node (Data x) l r) = ver (x>=) l && ver (x<) r

ver :: Ord a => (a -> Bool) -> Tree a -> Bool
ver _ (Leaf Null) = True
ver f (Leaf (Data y)) = f y
ver f (Node (Data y) l r) = f y && ver (y>=) l && ver (y<) r


occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf Null) = False
occurs x (Leaf (Data y)) = x == y
occurs x (Node (Data y) l r ) | x == y = True
                              | x < y = occurs x l
                              | x > y = occurs x r

direct :: Ord a => a -> Tree a -> Tree a
direct x (Node (Data y) l r) | x == y = Node (Data x) l r
                             | x > y = l
                             | otherwise = r
direct x _ = Leaf Null

find :: Ord a => a -> Tree a -> Tree a
find x (Leaf (Data y)) = if x == y then Leaf (Data y) else Leaf Null
find x (Leaf Null) = Leaf Null
find x nd = find x (direct x nd) 

insert :: Ord a => a -> Tree a -> Tree a
insert x (Leaf Null) = Leaf (Data x)
insert x (Leaf (Data y))     | x > y = Node (Data y) (Leaf Null) (Leaf (Data x)) 
                             | otherwise = Node (Data y) (Leaf (Data x)) (Leaf Null)
insert x (Node (Data y) l r) | x > y = Node (Data y) l (insert x r)
                             | otherwise = Node (Data y) (insert x l) r

flatten :: Tree a -> [a]
flatten (Leaf Null) = []
flatten (Leaf (Data x)) = [x]
flatten (Node (Data x) l r) =  (flatten l) ++ [x] ++ (flatten r)

delnode :: Ord a => Tree a -> Tree a
delnode (Node _ (Leaf Null) (Leaf Null)) = Leaf Null
delnode (Node _ (Leaf Null) (Leaf (Data x))) = Node (Data x) (Leaf Null) (Leaf Null)
delnode (Node _ (Leaf (Data x)) (Leaf Null)) = Node (Data x) (Leaf Null) (Leaf Null)
delnode (Node _ (Leaf (Data y)) (Leaf (Data x))) = Node (Data y) (Leaf Null) (Leaf (Data x))
delnode (Node _ l r) = Node (Data (snd deleted)) l (fst deleted)
                       where deleted = digForLeaf r

digForLeaf :: Ord a => Tree a -> (Tree a, a)
digForLeaf (Leaf (Data x)) = ((Leaf Null), x)
digForLeaf (Node (Data x) (Leaf Null) (Leaf Null)) = ((Leaf Null), x)
digForLeaf (Node (Data x) (Leaf (Data y)) r) = ((Node (Data x) (Leaf Null) r), y)
digForLeaf (Node (Data x) l r) = (Node (Data x) (fst next) r, snd next)
                                 where next = digForLeaf l

delete :: Ord a => a -> Tree a -> Tree a
delete x (Leaf Null) = Leaf Null
delete x (Leaf (Data y)) = if x == y then Leaf Null else Leaf (Data y) 
delete x (Node (Data y) l r) | x == y = delnode (Node (Data y) l r)
                             | x > y = Node (Data y) l (delete x r)
                             | otherwise = Node (Data y) (delete x l) r


                             
testData :: [Int]
testData = [4,6,2,7,8]

