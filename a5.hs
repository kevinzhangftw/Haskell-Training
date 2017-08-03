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


biggest_sum :: [[Int]] -> [Int]
biggest_sum [] = []
biggest_sum (x:xs) 
                   | sum(x) >= sum(biggest_sum(xs)) = x
                   | sum(x) < sum(biggest_sum(xs)) = biggest_sum(xs)

greatest :: (a -> Int) -> [a] -> a
greatest f [] = error "nothing in the list"
greatest f [x] = x
greatest f (x:xs) | f x > f (greatest f xs) = x
                  | otherwise = greatest f xs

is_bit :: Int -> Bool
is_bit x | (x == 0) || (x==1) = True
         | otherwise          = False

flip_bit :: Int -> Int
flip_bit x | is_bit x == False = error "Input is not a bit"
           | x == 0 = 1
           | x == 1 = 0

is_bit_seq1:: [Int]->Bool
is_bit_seq1 [] = True
is_bit_seq1 (x:xs) | is_bit x == False = False
                   | otherwise         = is_bit_seq1(xs)
                    
is_bit_seq2:: [Int]->Bool
is_bit_seq2 [] = True
is_bit_seq2 (x:xs) = if (is_bit x == False) then False else is_bit_seq2(xs)

is_bit_seq3:: [Int]->Bool
is_bit_seq3 [] = True
is_bit_seq3 x = all is_bit x                    

invert_bits1::[Int]->[Int]
invert_bits1 [] = []
invert_bits1 (x:xs) = [flip_bit x] ++ (invert_bits1 xs)

invert_bits2::[Int]->[Int]
invert_bits2 [] = []
invert_bits2 lst = map flip_bit lst

invert_bits3::[Int]->[Int]
invert_bits3 [] = []
invert_bits3 lst = [flip_bit x | x <- lst]

bit_count::[Int]->(Int, Int)
bit_count [] = (0,0)
bit_count lst = (count0s lst, count1s lst)

count0s::[Int]->Int
count0s [] = 0
count0s lst = foldr (\x y -> x + y) 0 (invert_bits1 lst)

count1s::[Int]->Int
count1s [] = 0
count1s lst = foldr (\x y -> x + y) 0 lst

-- Adapted from https://wiki.haskell.org/99_questions/Solutions/49
all_basic_bit_seqs:: Int->[[Int]]
all_basic_bit_seqs 0 = []
all_basic_bit_seqs 1 = [[0],[1]]
all_basic_bit_seqs n = [ 0 : x | x <- prev ] ++ [ 1 : x | x <- prev ]
                        where prev = all_basic_bit_seqs (n-1)

data Bit = Zero | One
    deriving (Show, Eq)

flipBit :: Bit -> Bit
flipBit bit | bit==Zero = One
            | otherwise = Zero

invert :: [Bit] -> [Bit]
invert [] = []
invert lst = map flipBit lst









bitSum1 :: [Bit] -> Int
bitSum1 lst = length (filter (\bit -> bit == One) lst)

bitSum2 :: [Maybe Bit] -> Int
bitSum2 lst = length (filter (\bit -> bit == Just One) lst)

data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons x lst) = [x] ++ toHaskellList(lst)





