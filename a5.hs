--testing done in a5.ipynb jupyter-notebook with ihaskell

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y:snoc x ys 

myappend :: [a] -> [a] -> [a]
myappend [] []     = []
myappend [] lst2   = lst2
myappend lst1 []   = lst1
myappend lst1 (x:xs)= myappend (snoc x lst1) xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = snoc x (myreverse xs)

--from http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html
smallest_divisor :: Int -> Int
smallest_divisor n
    | n < 0     = error "n must be >= 0"
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

--from http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html
is_prime :: Int -> Bool
is_prime n | n < 2     = False
           | otherwise = (smallest_divisor n) == n
           
--from http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html
primes_less_than :: Int -> [Int]
primes_less_than n = filter is_prime [2..(n-1)]

--adapted https://stackoverflow.com/questions/19808224/haskell-reverse-integer-with-recursion
reverseInt :: Int -> Int
reverseInt 0 = 0
reverseInt n = mod n 10 * 10^place + reverseInt (div n 10)
  where
    n' = fromIntegral n
    place = (floor . logBase 10) n'

is_emirp :: Int -> Bool
is_emirp n = is_prime (reverseInt n) && (n /= reverseInt n)

count_emirps :: Int -> Int
count_emirps n 
        | n < 13 = 0
        | n == 13 = 1
        | otherwise = length (filter is_emirp (primes_less_than n))




