-- Let's define a (cooler) version of Bool
data CoolBool = Nope | Yup deriving (Show, Eq, Ord)

-- Translate between Bool and CoolBool (and conversely)
boolToCool :: Bool -> CoolBool
boolToCool True = Yup
boolToCool False = Nope

coolToBool :: CoolBool -> Bool
coolToBool Yup = True
coolToBool Nope = False

-- Logical Operators: not, &&, ||
coolNot :: CoolBool -> CoolBool
coolNot Yup = Nope
coolNot Nope = Yup 

coolBoth :: CoolBool -> CoolBool -> CoolBool
coolBoth Yup Yup = Yup
coolBoth _ _ = Nope

coolBoth' :: CoolBool -> CoolBool -> CoolBool
coolBoth' x y   | x == Yup && y == Yup = Yup
                | otherwise = Nope

coolEither :: CoolBool -> CoolBool -> CoolBool
coolEither Nope Nope = Nope
coolEither _ _ = Yup

-- Extended connectives: and, or
-- and :: [Bool] -> Bool 
coolAnd :: [CoolBool] -> CoolBool
coolAnd [] = Yup 
coolAnd (x:xs)  | x == Nope = Nope 
                | otherwise = coolAnd xs 

coolAnd' :: [CoolBool] -> CoolBool
coolAnd' [] = Yup
coolAnd' (Nope:xs) = Nope
coolAnd' (_:xs) = coolAnd' xs

coolOr :: [CoolBool] -> CoolBool
coolOr [] = Nope
coolOr (Yup:xs) = Yup
coolOr (_:xs) = coolOr xs

-- Checking elements: elem :: Eq a => a -> [a] -> Bool
coolElem :: Eq a => a -> [a] -> CoolBool
coolElem x [] = Nope
coolElem x (y:ys)   | x == y = Yup
                    | otherwise = coolElem x ys

-- Quantifiers: all, any :: (a -> Bool) -> [a] -> Bool
coolAll :: (a -> CoolBool) -> [a] -> CoolBool
coolAll p [] = Yup 
coolAll p (x:xs) | p x == Nope = Nope
                 | otherwise = coolAll p xs 

coolAny :: (a -> CoolBool) -> [a] -> CoolBool
coolAny p [] = Nope
coolAny p (x:xs) | p x == Yup = Yup
                 | otherwise = coolAny p xs

-- Filter 
coolFilter :: (a -> CoolBool) -> [a] -> [a]
coolFilter p [] = []
coolFilter p (x:xs) | p x == Yup = x:coolFilter p xs
                    | otherwise = coolFilter p xs 

gtThree :: Int -> CoolBool
gtThree x   | x > 3 = Yup
            | otherwise = Nope
