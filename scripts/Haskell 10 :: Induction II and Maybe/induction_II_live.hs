foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- val = z
-- for x in xs:
--  val = f(x,val)

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

-- length, sum, product, elem
length' :: [a] -> Int
length' = foldr' (\x val -> val + 1) 0

sum' :: Num a => [a] -> a
sum' = foldr' (+) 0 

prod' :: Num a => [a] -> a
prod' = foldr' (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' (\x filtered -> if p x then x:filtered else filtered ) []

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldr' (\x b ->  b || x == y ) False

-- reversing a list
reverse' :: [a] -> [a]
reverse' = foldr' (\x reversed -> reversed ++ [x]) [] 

-- fold from left
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

reverse'' :: [a] -> [a]
reverse'' = foldl' (\reversed x -> x:reversed ) []

-- Partial functions
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

-- Maybe datatype for undefined points
-- data Maybe a = Just a | Nothing deriving (Show, Eq, Ord)

head'' :: [a] -> Maybe a
head'' [] = Nothing
head'' (x:xs) = Just x

maybeMult :: Maybe Int -> Maybe Int -> Maybe Int
maybeMult (Just x) (Just y) = Just (x*y)
maybeMult Nothing _ = Nothing
maybeMult _ Nothing = Nothing 

mult2Head :: [Int] -> Maybe Int
mult2Head xs = maybeMult (Just 2) (head'' xs)

fToMaybe :: (a -> b) -> Maybe a -> Maybe b
fToMaybe f Nothing = Nothing
fToMaybe f (Just x) = Just (f x)

mult2Head' :: [Int] -> Maybe Int
mult2Head' xs = fToMaybe (*2) (head'' xs)

-- Lists for multiple function values
genSqrt :: Double -> [Double]
genSqrt 0 = [0]
genSqrt x = [-sqrt x, sqrt x]

genSqrtby2 :: Double -> [Double]
genSqrtby2 xs = map (*2) (genSqrt xs) 

head''' :: [a] -> [a]
head''' [] = []
head''' (x:xs) = [x]

-- Flattening Lists and Maybe
flattenList :: [[a]] -> [a]
flattenList [] = []
flattenList (x:xs) = x ++ flattenList xs

toList :: a -> [a]
toList x = [x]

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just x)) = Just x

toMaybe :: a -> Maybe a
toMaybe x = Just x