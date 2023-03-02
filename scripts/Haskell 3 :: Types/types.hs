-- Types
-- Numbers: Int, Integer, Float, Double
-- Bool
-- String, Char

-- Typeclasses: Enum, Bounded, Num, Integral, Fractional, Floating


addTogether :: Int -> Int -> Int
addTogether x y = x + y

project :: (Integer, String) -> Integer
project xs = fst xs

project' :: Integer -> String -> Integer
project' x y = x