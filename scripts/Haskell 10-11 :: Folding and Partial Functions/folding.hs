foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- val = z
-- for x in xs:
--   val = f(x, val)

max' :: Int -> Int -> Int
max' x y | x > y = x
         | otherwise = y

maximum' :: [Int] -> Int
maximum' xs = foldr' max' (minBound :: Int) xs

-- and, or
and' :: [Bool] -> Bool
and' = foldr' (&&) True

or' :: [Bool] -> Bool
or' = foldr' (||) False

-- sum, prod
sum' :: Num a => [a] -> a
sum' = foldr' (+) 0

prod' :: Num a => [a] -> a
prod' = foldr' (*) 1

-- length, elem, filter
length' :: [a] -> Int
length' = foldr' (\x val -> val + 1) 0

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldr' (\x val -> val || x == y) False 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' (\x filtered -> if p x then x:filtered else filtered) []

-- reverse
reverse' :: [a] -> [a]
reverse' = foldr' (\x reversed -> reversed ++ [x]) [] 

-- folding from the left
foldl' :: (b -> a -> b) -> b -> [a] -> b 
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

-- reverse again
reverse'' :: [a] -> [a]
reverse'' = foldl' (\reversed x -> x:reversed) []