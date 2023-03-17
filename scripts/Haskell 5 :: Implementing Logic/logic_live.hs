-- Let's make our own (cooler) version of booleans
data CoolBool = Nope | Yup  deriving (Show, Eq, Ord)

-- Translate back and forth between Bool and CoolBool
coolToBool :: CoolBool -> Bool
coolToBool Yup = True
coolToBool Nope = False

boolToCool :: Bool -> CoolBool
boolToCool True = Yup
boolToCool False = Nope

boolToCool' :: Bool -> CoolBool
boolToCool' x | x = Yup
              | otherwise = Nope

-- Implement the connectives: not, &&, ||
coolNot :: CoolBool -> CoolBool
coolNot Yup = Nope
coolNot Nope = Yup

coolNot' :: CoolBool -> CoolBool
coolNot' x  | x == Yup = Nope
            | otherwise = Yup

coolBoth :: CoolBool -> CoolBool -> CoolBool
coolBoth Yup Yup = Yup
coolBoth _ _ = Nope

-- Implement: and, or
coolAnd :: [CoolBool] -> CoolBool
coolAnd [] = Yup
coolAnd (x:xs) | x == Nope = Nope
               | otherwise = coolAnd xs

-- Implement elem

-- Implement the quantifiers: all, any
-- all :: (a -> Bool) -> [a] -> Bool

-- Implement filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

-- filter :: (a -> Bool) -> [a] -> [a]

-- Rewrite: all, any using map

-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]

-- Anonymous functions (lambdas)
-- \x -> 3*x
-- \x y -> x + y
