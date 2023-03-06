-- Types and Typeclasses

doubleMe :: Int -> Int 
doubleMe x = x + x 

doubleMe' :: Float -> Float
doubleMe' x = x + x 

addUs :: Int -> Int -> Int 
addUs x y = x + y 

-- Some basic types
-- Number: Int, Integer, Float, Double
-- Bool
-- Char
-- String
-- Lists: [..] e.g [Char]
-- Tuples: (.., .., ..) e.g (Bool, Char)

-- Type variables

-- Typeclasses
doubleMe'' :: Num a => a -> a
doubleMe'' x = x + x 

-- Eq, Ord, Enum
-- Show, Read
-- Num, Integral, Floating