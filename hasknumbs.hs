type Complex = (Float, Float)

sieve :: Int -> [Int]
sieve n = if n < 2 then [] else sieve_ [2..n] n 2

sieve_ :: [Int] -> Int -> Int -> [Int]
sieve_ ns n m | m*m > n = ns
              | otherwise = sieve_ ns' n m'
              where ns' = [n' | n' <- ns, n' == m || n' `mod` m /= 0]
                    m'  = head [n' | n' <- ns', n' > m]

numprimes :: Int -> Int
numprimes n = length (sieve n)

addcomp :: Complex -> Complex -> Complex
addcomp (r, c) (r', c') = (r + r', c + c')

subcomp :: Complex -> Complex -> Complex
subcomp (r, c) (r', c') = (r - r', c - c')

multcomp :: Complex -> Complex -> Complex
multcomp (r, c) (r', c') = (r*r' + c*c', r*c' + r'*c)

divcomp :: Complex -> Complex -> Complex
divcomp (_, _) (0, 0) = error "Error: divide by zero"
divcomp (r, c) (r', c') = (r'', c'')
                        where r'' = (r*r' + c*c') / (r'*r' + c'*c')
                              c'' = (r*c' + r'*c) / (r'*r' + c'*c')

fizzbuzz  :: Int -> Int -> Int -> [String]
fizzbuzz n a b | n == 0 = []
               | n `mod` a == 0 && n `mod` b == 0 = "FizzBuzz" : fizzbuzz (n-1) a b
               | n `mod` a == 0 = "Fizz" : fizzbuzz (n-1) a b
               | n `mod` b == 0 = "Buzz" : fizzbuzz (n-1) a b
               | otherwise = show n : fizzbuzz (n-1) a b






