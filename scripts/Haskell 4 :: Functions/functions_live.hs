sayMe :: Int -> String
sayMe 0 = "Zero"
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe _ = "Larger than two"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors x y = (fst x + fst y, snd x + snd y)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y 


sayLength :: [a] -> String
sayLength [] = "Zero"
sayLength [x] = "One"
sayLength [x,y] = "Two"
sayLength _ = "More than two"

sayLength' :: [a] -> String
sayLength' [] = "Zero"
sayLength' (x:[]) = "One"
sayLength' (x:y:[]) = "Two"
sayLength' (x:y:xs) = "More than two" 

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Exercise: Use pattern matching to define a function that sums together all elements of a list of numbers. 

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- Exercise: Write a function that calculates the scalar product of two 2D vectors

scalarProd :: (Double, Double) -> (Double, Double) -> Double
scalarProd (x1, x2) (y1, y2) = x1*y1 + x2*y2 

-- Exercise: Write a function that returns the second element of a list if possible. Otherwise return an error. 

secondList :: [a] -> a
secondList (_:y:_) = y
secondList _ = error "List does not have second element"

-- Guards
grades :: Double -> String
grades points
  | points <= 5 = "Fail"
  | points <= 10 = "C"
  | points <= 15 = "B"
  | otherwise = "A"

max' :: Ord a => a -> a -> a
max' x y
    | x < y = y
    | otherwise = x

-- Ex: Write a function that returns the absolute value a a number using guards

abs' :: (Num a, Ord a) => a -> a
abs' x 
    | x >= 0 = x
    | otherwise = -x

-- Ex: Write a better version of factorial covering negative numbers using pattern matching and guards

factorial' :: Integral a => a -> a
factorial' n 
    | n < 0 = 1
    | otherwise = case n of 0 -> 1
                            n -> n * factorial' (n-1)

factorial'' :: Integral a => a -> a
factorial'' n 
    | n < 0 = 1
    | otherwise = fact n
    where fact 0 = 1
          fact n = n * fact (n-1)

-- Where

dist :: (Num a, Ord a) => a -> a -> a
dist x y 
    | diff >= 0 = diff + zero
    | otherwise = -diff + zero
    where   diff = x-y
            zero = 0


cylinder :: Double -> Double -> Double
cylinder r h = 2 * topArea + sideArea
    where   topArea = pi * r^2
            sideArea = h * 2 * pi * r

-- Let
cylinder' :: Double -> Double -> Double
cylinder' r h =
    let  topArea = pi * r^2
         sideArea = h * 2 * pi * r
    in   2 * topArea + sideArea 

-- Case
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:xs) = x 

head'' :: [a] -> a
head'' list =
    case list of  [] -> error "Empty list"
                  (x:xs) -> x
