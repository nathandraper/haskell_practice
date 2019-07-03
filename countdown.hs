data Op = Add | Sub | Mul | Div | Exp
data Expr = Val Integer | App Op Expr Expr

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = pshow l ++ show o ++ pshow r
                       where
                           pshow (Val n) = show n
                           pshow e = "(" ++ show e ++ ")"

--valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = x > y
--valid Mul _ _ = True
--valid Div x y = x `mod` y == 0

apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

values :: Expr -> [Integer]
values (Val n) = [n]
values (App _ x y) = values x ++ values y

eval :: Expr -> [Integer]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o lv rv | (lv) <- eval l, (rv) <- eval r, valid o lv rv]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
            where yss = subs(xs)

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat.map perms.subs

solution :: Expr -> [Integer] -> Integer -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:l, r) | (l, r) <- (split xs)]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Integer] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

solutions :: [Integer] -> Integer -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- improving the algorithm
type Result = (Expr, Integer)

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =  [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Integer] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    rx <- results rs,
                    res <- combine' lx rx]

solutions' :: [Integer] -> Integer -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, n') <- results ns', n' == n]

valid :: Op -> Integer -> Integer -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = y > 1

-- Exercises
-- 1
choices' :: [a] -> [[a]]
choices' ns = [c | ns' <- subs ns, c <- perms ns']

-- 2
remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) | x == y = ys
                | otherwise = (y:) (remove x (ys))

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (remove x ys) 

-- 3
-- the recursion would not terminate as the exprs function would be repeatedly
-- called with the same list

--4
ex4 :: Int
ex4 = length(concat(map exprs (choices[1,3,7,10,25,50])))

ex4' :: Int
ex4' = length [ 1 | e <- concat(map exprs (choices[1,3,7,10,25,50])), eval e /= []]
