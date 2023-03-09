-- Pattern matching

sayNumber :: Int -> String
sayNumber 0 = "Zero"
sayNumber 1 = "One"
sayNumber x = "Larger than one"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors x y = (fst x + fst y, snd x + snd y)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Empty list!"
head' (x : xs) = x

sayLength :: [a] -> String
sayLength [] = "Zero"
sayLength [_] = "One"
sayLength [_, _] = "Two"
sayLength (_ : _) = "Longer than two"

sayLength' :: [a] -> String
sayLength' [] = "Zero"
sayLength' (x : []) = "One"
sayLength' (x : y : []) = "Two"
sayLength' (x : y : _) = "Longer than two"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Ex: Write a function that calculates the scalar product of two 2D vectors

scalarProd :: (Double, Double) -> (Double, Double) -> Double
scalarProd (x1, x2) (y1, y2) = (x1 * y1) + (x2 * y2)

-- Ex: Write a function that returns the second element of a list, if possible

secondList :: [a] -> a
secondList (x : y : xs) = y
secondList _ = error "List does not have 2nd element"

-- Guards

grade :: Double -> String
grade p
  | p <= 5 = "Fail"
  | p <= 10 = "C"
  | p <= 15 = "B"
  | otherwise = "A"

max' :: Ord a => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- Ex: Write a function that returns the absolute value a a number using guards

abs' :: (Ord a, Num a) => a -> a
abs' x
  | x >= 0 = x
  | x < 0 = -x

-- Where

dist :: (Ord a, Num a) => a -> a -> a
dist x y
  | diff >= 0 = diff + zero
  | otherwise = -diff + zero
  where
    diff = x - y
    zero = 0

-- Let
cylinder :: Double -> Double -> Double
cylinder r h = 2 * topArea + sideArea
  where
    topArea = pi * r ^ 2
    sideArea = h * 2 * pi * r

cylinder' :: Double -> Double -> Double
cylinder' r h =
   let topArea = pi * r ^ 2
       sideArea = h * 2 * pi * r
   in  2 * topArea + sideArea 

-- Case
head'' :: [a] -> a
head'' xs = case xs of  [] -> error "Empty list"
                        (x:_) -> x
