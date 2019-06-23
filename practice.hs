import Data.Char
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

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

islower :: Char -> Bool
islower c = c >= 'a' && c <= 'z'

shift :: Char -> Int -> Char
--shift c n = if islower c then int2let ((let2int c + n) `mod` 25) else c
shift c n | islower c = int2let ((let2int c + n) `mod` 25)
          | otherwise = c

encode :: String -> Int -> String
encode s n = [shift c n | c  <- s]

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

--9
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x, y) <- zip xs ys]