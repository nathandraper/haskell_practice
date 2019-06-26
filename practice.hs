import Data.Char
import Data.List (sort)

-- beginning practice
{-
    nested
    comments
    like
    this
-}

double x = x + x
add2 a = a + b
    where
        b = 2
num = a + b
    where
        a = 1
        b = 2

factorial x = product[1..x]
average x = sum x `div` length x

--ch 2 exercises
--3
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

--4
newlast xs = xs !! (length xs - 1)
newlast2 xs = head(reverse xs)
--recursion
newlastr [x] = x
newlastr xs = newlast (tail xs) 

--5
newinit xs = take ((length xs) - 1) xs
newinit2 xs = reverse (tail(reverse xs))

--ch 3
--basic types
-- Num class
-- 1 :: Int -- int (-2^63 - 2^63 - 1)
-- 1 :: Integer -- integer but as much memory as required
-- 1.0 :: Float -- floating point
-- 1.0000 :: Double -- double space of a float

-- 'a' :: Char
-- "abc" :: String

-- False :: Bool

-- [1,2,3] :: [Int] -- Lists: elements must be of same type. Can be any type, incl non-basic
-- ('a',5,False) :: (Char, Integer, Bool) -- Tuples: don't have to be same type, fixed length

-- Function types
-- takes a tuple and maps to an int
--add :: (Int, Int) -> Int
--add (x, y) = x + y

-- curried version
add :: Int -> Int -> Int
add x y = x+y

-- Polymorphic types
-- polymorphic variable usually use a, b, c....
-- length :: [a] -> Int

-- Overloaded types: + operator is overloaded to work with any type from the Num class
-- Basic classes:
-- Eq - Equality types
-- Ord - Ordered Types
-- Show - Showable types
-- Read - readable types
-- Num - numeric types
-- Fractional - Fractional types

-- Exercises:
-- 3
second :: [a] -> a
second xs = head (tail xs) -- second :: [a] -> a

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x, y)

double2 :: Num a => a -> a
double2 x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> (t -> t)
twice f x = f (f x)

-- ch. 4
-- Defining functions
-- from old:
split :: Int -> [a] -> ([a], [a])
split n xs = (take n xs, drop n xs)

-- conditional:
greaterThanFive :: (Ord n, Num n) => n -> Bool
greaterThanFive n = if n > 5 then True else False

-- nested conditional: DONT FORGET TO (-n) FOR NEGATIVE NUMS
signum2 :: (Ord n, Num n) => n -> Int
signum2 n = if n > 0 then 1 else
                if n < 0 then -1 else 0

-- guarded function:
abs2 :: (Ord n, Num n) => n -> n
abs2 n | n >= 0 = n
      | otherwise = -n

-- pattern matching:
andy :: (Bool, Bool) -> Bool
andy(True, True) = True
andy(True, False) = False
andy(False, True) = False
andy(False, False) = False

-- return whatever value b has, _ wildcard
hyp :: Bool -> Bool -> Bool
hyp True b = b
hyp False _ = True

-- tuple patterns
thrd :: (a, b, c) -> c
thrd (_, _, z) = z

-- list patterns
fivesandwich :: (Num a, Eq a) => [a] -> Bool
fivesandwich [5, _, 5] = True
fivesandwich _ = False

-- : is a function that takes the left arg and prepends to right list
-- can be used to specify list patterns for general length
-- must be parenthesized
firstisz :: [Char] -> Bool
firstisz ('z':_) = True
firstisz _ = False

-- lambdas
applythislambda :: Num a => a -> a
applythislambda x = (\x -> x + 2) x

addy :: Num a => a -> a -> a
addy = \x -> (\y -> x + y)

-- operator sections
-- (#) = \x -> (\y -> x # y)
-- (x#) = \y -> x # y
-- (#y) = \x -> x # y
-- x # y = x # y

-- Exercises
-- 1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

-- 2
thirda :: [a] -> a
thirda xs = head (tail(tail xs))

thirdb :: [a] -> a
thirdb xs = xs !! 2

thirdc :: [a] -> a
thirdc (_:_:a:_) = a

-- 3
safetaila :: [a] -> [a]
safetaila xs = if null xs then xs else tail xs 

safetailb :: [a] -> [a]
safetailb xs | null xs = xs
             | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc [] = []
safetailc xs = tail xs

--4
disjunct :: Bool -> Bool -> Bool
disjunct False b = b
disjunct True _ = True

-- 5
andius :: Bool -> Bool -> Bool
andius x y = if x then 
                if y then True else False
                else False

-- 6
andiusrex :: Bool -> Bool -> Bool
andiusrex x y = if x then y else False

-- 7
multy :: Int -> Int -> Int -> Int
multy = \x -> \y -> \z -> x * y * z

-- 8
luhndouble :: Int -> Int
luhndouble x = if x*2 <=9 then x*2 else x*2 - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if (luhndouble a + b + luhndouble c + d) `mod` 10 == 0 then True else False

-- ch. 5
-- list comprehensions
-- [x | x -> xs]
-- [(x, y) | x <- [1..3], y <- y[4..5]] all combinations of x and y

length2 :: [a] -> Int
length2 xs = sum [1 | _ <- xs]

firsts :: [[a]] -> [a]
firsts xs = [x | [x, _] <- xs]

-- guards
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find x kvs = [v | (k, v) <- kvs, k == x]

-- zip
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = zip xs ys

positions :: Eq a => a -> [a] ->  [Int]
positions x xs = [i | (i, v) <- (zip [0..] xs), v == x]

-- string comprehensions (polymorphic functions work on strings as well)
lowers :: String -> Int
lowers s = sum [1 | c <- s, c >= 'a' && c <= 'z']

count :: Char -> String -> Int
count c s = sum [1 | c' <- s, c' == c] 

-- caesar cypher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

islower :: Char -> Bool
islower c = c >= 'a' && c <= 'z'

shift :: Char -> Int -> Char
shift c n = if islower c then int2let ((let2int c + n) `mod` 25) else c
--shift c n | islower c = int2let ((let2int c + n) `mod` 25)
--          | otherwise = c

encode :: String -> Int -> String
encode s n = [shift c n | c  <- s]

--decode
table :: [Float]
table = [8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 
        0.153, 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 
        9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

freq :: String -> [Float]
freq s = [percent (count x s) (lowers s) | x <- ['a'..'z']]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es] 

rotate :: [a] -> Int -> [a]
rotate xs n = drop n xs ++ take n xs

crack :: String -> String
crack s = encode lows factor
    where lows = [toLower c | c <- s]
          rotations = [rotate (freq s) n | n <- [0..25]]
          ns = [0..]
          chis = [chisqr os table | os <- rotations]
          factor = -snd(minimum(zip chis ns))

-- exercises
-- 1
sumsquare :: Int
sumsquare = sum [x^2 | x <-[0..100]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <- [0..x], y' <- [0..y]]

-- 3 
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, not (x == y)]

-- 4
replicate2 :: a -> Int -> [a]
replicate2 x n = [x | _ <- [1..n]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

-- 6
perfects :: Int -> [Int]
perfects n = [n | n <- [1..n], sum (factors n) - n == n]

-- 7
pairsConcat :: [a] -> [b] -> [(a, b)]
pairsConcat xs ys = concat [[(x, y) | y <- ys] | x <- xs ]

-- 8
positionsFind :: Eq a => a -> [a] -> [Int]
positionsFind x xs = [i | i <- find x (zip xs [0..])]

-- 9
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x, y) <- zip xs ys]

-- 10
-- just changed the crack function to convert string to lowercase

-- ch. 6
-- recursion
fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac n | n > 0 = n * fac(n-1)
      | otherwise = -1

-- recursion on lists
productrec :: Num a => [a] -> a
productrec [] = 1
productrec (x:xs) = x * productrec xs

reverserec :: [a] -> [a]
reverserec [] = []
reverserec (x:xs) = reverserec xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:xs) | x <= y = x:y:xs
                | otherwise = y : insert x xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs) 

-- recursion with multiple arguments

ziprec :: [a] -> [b] -> [(a, b)]
ziprec _ [] = []
ziprec [] _ = []
ziprec (x:xs) (y:ys) = [(x, y)] ++ ziprec xs ys 

droprec :: Int -> [a] -> [a]
droprec 0 xs = xs
droprec _ [] = []
droprec n (x:xs) = droprec (n-1) xs

-- multiple recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-2) + fib (n-1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort smaller) ++ [x] ++ (qsort larger)
    where smaller = [y | y <- xs, y <= x]
          larger = [z | z <-xs, z > x]

-- mutual recursion
evenrec :: Int -> Bool
evenrec 0 = True
evenrec n = _odd (n-1)

_odd :: Int -> Bool
_odd 0 = False
_odd n = evenrec (n-1)

-- exercises
-- 2
sumdown :: Int -> Int
sumdown 1 = 1
sumdown n = n + sumdown (n-1)
 -- 3
exprec :: Int -> Int -> Int
exprec _ 0 = 1
exprec n p = n * (exprec n (p-1))

-- 4
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | otherwise = euclid (min x y) ((max x y) - (min x y))

-- 6
andiusrecs :: [Bool] -> Bool
andiusrecs [True] = True
andiusrecs (False:_) = False
andiusrecs (True:bs) = andiusrecs bs

concatrec :: [[a]] -> [a]
concatrec [] = []
concatrec (xs:xss) = xs ++ concatrec xss

replicaterec :: [a] -> [a]
replicaterec [] = []
replicaterec (x:xs) = x : replicaterec xs

nth :: (Eq a, Num a) => [a] -> Int -> a
nth [] _ = -1
nth (x:xs) n | n == 0 = x
             | otherwise = xs `nth` (n-1) 

elemrec :: Eq a => a -> [a] -> Bool
elemrec _ [] = False
elemrec x (y:xs) | x == y = True
                 | otherwise = elemrec x xs

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) (ys)

-- 8
halverec :: [a] -> ([a], [a])
halverec xs = (take half xs, drop half xs)
    where half = (length xs) `div` 2

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = (mergesort (fst(halves))) `merge` (mergesort (snd(halves)))
    where halves = halverec xs

-- 9
sumrec :: Num a => [a] -> a
sumrec [] = 0
sumrec (x:xs) = x + sumrec xs

takerec :: Int -> [a] -> [a]
takerec _ [] = []
takerec 0 _ = []
takerec n (x:xs) = x : takerec (n-1) xs

lastrec :: [a] -> a
lastrec [x] = x
lastrec xs = lastrec(tail xs)

-- ch 7
-- higher order functions
-- composition operator
sumsqreven :: [Int] -> Int
sumsqreven = sum . map (^2) . filter even

-- foldr
productf :: Num a => [a] -> a
productf = foldr (*) 1

lengthf :: [a] -> Int
lengthf = foldr (\_ n -> n+1) 0

reversef :: [a] -> [a]
reversef = foldr (\x xs -> xs ++ [x] ) []

-- foldl
lengthfl :: [a] -> Int
lengthfl = foldl(\n _ -> n+1) 0

reversefl :: [a] -> [a]
reversefl = foldl (\xs x -> x:xs) []

-- binary string 
--bin2int :: [Int] -> Int
--bin2int bits =  sum [w*b | (b,w) <- weights]
    --where weights = zip (reversefl bits) (iterate (*2) 1) 

bin2intf :: [Int] -> Int
bin2intf = foldr (\b s -> b + 2*s) 0

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin(n `div` 2)

make8 :: [Int] -> [Int]
make8 bits = take 8 (bits ++ (repeat 0))

encode2bin :: String -> [Int]
encode2bin =  concat.map (make8.int2bin.ord)

chop :: [Int] -> [[Int]]
chop xs | xs == [] = []
        | otherwise = (take 8 xs) : chop (drop 8 xs)

decode2str :: [Int] -> String
decode2str xs = map (chr.bin2intf) (chop xs)
    where 

channel :: a -> a
channel a = id a

transmit :: String -> String
transmit = decode2str.channel.encode2bin

-- voting example
-- first past the post
countv :: Eq a => a -> [a] -> Int
countv x = length.filter(==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = sort [(countv x xs, x) | x <- rmdups xs] 

winner :: Ord a => [a] -> a
winner = snd.last.result

-- alternative choices
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/=x))

rank :: Ord a => [[a]] -> [a]
rank = map snd.result.map head

winner2 :: Ord a => [[a]] -> a
winner2 xs = case rank (rmempty xs) of
    [win] -> win
    (y:ys) -> winner2 (elim y xs)

-- exercises
-- 1
-- map f (filter p xs)

--2
myall :: (a -> Bool) -> [a] -> Bool
myall f = and.map f

myany :: (a -> Bool) -> [a] -> Bool
myany f = or.map f

mytakewhile :: (a-> Bool) -> [a] -> [a]
mytakewhile f [] = []
mytakewhile f (x:xs) | not(f x) = []
                     | otherwise = x : mytakewhile f xs

mydropwhile :: (a-> Bool) -> [a] -> [a]
mydropwhile f [] = []
mydropwhile f (x:xs) | not(f x) = x:xs
                     | otherwise = mydropwhile f xs

-- 3
mymapf :: (a -> a) -> [a] -> [a]
mymapf f = foldr (\x y -> (f x):y) []

myfilterp :: (a -> Bool) -> [a] -> [a]
myfilterp p = foldr filt []
    where filt x y = if p x then x:y else y

-- 4
dec2int :: [Int] -> Int
dec2int xs = foldl (\x (y, w) -> x + y*(10^w) ) 0 weighted
    where weighted = reverse(zip(reverse xs) (iterate (+1) 0))

-- 5
mycurry :: ((a, b) -> c) -> (a -> b -> c)
mycurry f = (\x y -> f (x, y))

myuncurry :: (a -> b -> c) -> ((a, b) -> c)
myuncurry f = (\(x, y) -> f x y)

testcurry :: Num a => (a, a) -> a
testcurry (x, y) = x + y

-- 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2binuf :: Int -> [Int]
int2binuf 0 = [0]
int2binuf x = unfold (==0) (`mod` 2) (`div` 2) x

chop8uf :: [Int] -> [[Int]]
chop8uf = unfold (==[]) (take 8) (drop 8)

mapuf :: Eq a => (a -> b) -> [a] -> [b]
mapuf f =  unfold (==[]) (f.head) (tail) 

iterateuf :: (a -> a) -> a -> [a]
iterateuf f = unfold (\a -> False) f f

-- 7 
addparity :: [Int] -> [Int]
addparity xs = xs ++ [((sum xs) `mod` 2)]

checkparity :: [Int] -> [Int]
checkparity xs | (last xs) == ((sum (take 8 xs)) `mod` 2) = take 8 xs 
               | otherwise = error "parity error"

encode2binp :: String -> [Int]
encode2binp = concat.map addparity.map (make8.int2bin.ord)

chop9 :: [Int] -> [[Int]]
chop9 = unfold (==[]) (take 9) (drop 9)

decode2strp :: [Int] -> String
decode2strp =  map (chr.bin2intf.checkparity).chop9

badchannel :: [Int] -> [Int]
badchannel = tail

transmitwparity :: String -> String
transmitwparity =  decode2strp.channel.encode2binp

altmap :: Foldable t1 => (t2 -> a) -> (t2 -> a) -> t1 t2 -> [a]
altmap f g = foldr alt []
    where alt x y | even(length y) = (f x):y
                  | otherwise = (g x):y

int2dec :: Int -> [Int]
int2dec 0 = []
int2dec x = int2dec (x `div` 10) ++ [x `mod` 10]

luhngen :: Int -> Bool
luhngen x | (sum(altmap id luhndouble dec) `mod` 10) == 0 = True
          | otherwise = False
          where dec = int2dec x

-- ch. 8
-- type declarations
type Pos = (Int, Int)
type Trans = Pos -> Pos

-- parameterizing types with other types
type Pair a = (a, a) -- a pair of two of the same type
type Assoc k v = [(k, v)]

findt :: Eq k => k -> Assoc k v -> v
findt k t = head [v | (k', v) <- t, k' == k]

-- data declarations
data Move = North | West | South | East

move :: Move -> Pos -> Pos
move North (x, y) = (x, y+1) 
move West (x, y) = (x-1, y) 
move South (x, y) = (x, y-1) 
move East (x, y) = (x+1, y) 

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev West = East
rev East = West

-- constructors in data class can have arguments
data Shape = Circle Float | Rect Float Float

squaret :: Float -> Shape
squaret n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * (r^2)
area (Rect x y) = x*y

data MyMaybe a = MyNothing | MyJust a
    deriving Show

safediv :: Int -> Int -> MyMaybe Int
safediv _ 0 = MyNothing
safediv x y = MyJust (x `div` y)

safehead :: [a] -> MyMaybe a
safehead [] = MyNothing
safehead (x:xs) = MyJust x

-- newtype declaration - type safety with no increased performance
--newtype Nat = N Int

-- recursive types
data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addnat :: Nat -> Nat -> Nat
addnat Zero n = n
addnat (Succ m) n = Succ((addnat m n))