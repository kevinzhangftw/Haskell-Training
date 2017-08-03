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
myreverse (x:xs) = myappend (myreverse xs) [x]


isInt :: Float -> Bool 
isInt n = floor n == ceiling n

isPrime :: Int -> Bool
isPrime n = 
    n > 1 && 
    not ( any (\x -> isInt(fromIntegral n / fromIntegral x)) [2.. n `div` 2] )
    
reverseInt :: Int -> Int
reverseInt n = read (myreverse $ show n) ::Int

isEmirp :: Int -> Bool
isEmirp n = isPrime n && 
            isPrime ( reverseInt n ) &&
            n /= reverseInt n

count_emirps :: Int -> Int
count_emirps n = length $ filter isEmirp [1..n]

greatest :: (a -> Int) -> [a] -> a
greatest f [] = error "empty list"
greatest f [x] = x
greatest f (x: xs) = if (f x > f (greatest f xs)) then x else greatest f xs

is_bit :: Int -> Bool
is_bit x = (x == 1) || (x == 0)

is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 x
    | x == [] = True
    | is_bit(head x) = is_bit_seq1(tail x)
    | otherwise = False
    
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 [] = True
is_bit_seq2 (x: xs) = if is_bit(x) then is_bit_seq2(xs) else False

is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 x = all is_bit x

invert_bits1 :: [Int] -> [Int]
invert_bits1 [] = []
invert_bits1 (x:xs) = (1 - x) : (invert_bits1 xs)

invert_bits2 :: [Int] -> [Int]
invert_bits2 xs = map (\x -> (1 - x)) xs

invert_bits3 :: [Int] -> [Int]
invert_bits3 xs = [1 - x | x <- xs]

bit_count :: [Int] -> (Int, Int)
bit_count xs = (length (filter (\x -> x == 0) xs) , length (filter (\x -> x == 1) xs) )

all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs 0 = []
all_basic_bit_seqs 1 = [[0], [1]]
all_basic_bit_seqs n = map (snoc 0) (all_basic_bit_seqs (n-1)) ++ map (snoc 1) (all_basic_bit_seqs (n-1))

data Bit = Zero | One
    deriving (Show, Eq)
    
flipBit :: Bit -> Bit
flipBit b = if b == Zero then One else Zero

invert :: [Bit] -> [Bit]
invert bs = map flipBit bs

to_bit :: Int -> Bit
to_bit x = if x == 1 then One else Zero

bit_place :: (Int, Int) -> Bit
bit_place (num, place)
    | place == 1 = to_bit(num `mod` 2)
    | otherwise = to_bit((num `div` 2^(place-1)) `mod` 2)

bit_seq :: (Int, Int) -> [Bit]
bit_seq (_, 0) = [] 
bit_seq (num, len) = map (\place -> bit_place(num, place)) [1..len] 

all_bit_seqs :: Int -> [[Bit]]
all_bit_seqs n = [bit_seq(x,n) | x <- [1..2^n]]

bitSum1 :: [Bit] -> Int
bitSum1 bits = length $ filter (\x -> x == One) bits

bitSum2 :: [Maybe Bit] -> Int
bitSum2 [] = 0
bitSum2 (x: xs) = if x == Just One then 1 + bitSum2(xs) else bitSum2(xs)

data List a = Empty | Cons a (List a)
    deriving Show
    
toList :: [a] -> List a
toList [] = Empty
toList (x: xs) = Cons x (toList(xs)) 

toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons x xs) = x:(toHaskellList xs)

append :: List a -> List a -> List a
append Empty Empty = Empty
append as Empty = as
append Empty bs = bs
append (Cons a Empty) bs = Cons a bs
append as bs = Cons (listHead as) (append (listTail as) bs)

listHead :: List a -> a
listHead (Cons a as) = a        

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons a as) = as

removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f l = if not $ f $ listHead l then (Cons (listHead l) $ removeAll f (listTail l)) else removeAll f (listTail l)

sort :: Ord a => List a -> List a 
sort Empty = Empty
sort l = let lh = listHead l
             lt = listTail l
             sortedFirst = sort (removeAll (>= lh) lt)
             sortedLast = sort (removeAll (< lh) lt)
        in append sortedFirst (Cons lh sortedLast)



