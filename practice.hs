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



