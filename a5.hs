-- snoc 5 [1,2,3] is [1,2,3,5]
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y:snoc x ys 

--myappend [1] [2] is [1,2]
myappend :: [a] -> [a] -> [a]
myappend [] []     = []
myappend [] lst2   = lst2
myappend lst1 []   = lst1
myappend lst1 (x:xs)= myappend (snoc x lst1) xs





