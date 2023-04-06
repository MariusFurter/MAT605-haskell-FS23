import SetList
import Data.List (nub)

-- Datatype for functions
data Fun' a b = Fun' (Set a) (Set b) (Set (a,b)) deriving Show

data Fun a b = Fun {dom :: Set a, 
                    cod :: Set b, 
                    pairs :: Set (a, b)} deriving Show

f = Fun (Set [1,2,3]) (Set ['a','b','c']) (Set [(1,'a'),(2,'b'), (3,'b')])
g = Fun (Set [1,2,3]) (Set ['a','b','c']) (Set [(1,'a'),(2,'b'), (3,'c')])

h = Fun (Set [1,2,3]) (Set ['a','b','c']) (Set [(2,'a'),(2,'b'), (3,'c')])

numUnique ::Eq a => [a] -> Int
numUnique = length . nub

getPairs :: Fun a b -> [(a,b)]
getPairs f = unSet $ pairs f

-- Domain of definition
domDef :: Eq a => Fun a b -> Set a
domDef f = toSet $ map fst (getPairs f)

-- Image
imFun :: Eq b => Fun a b -> Set b
imFun f = toSet $ map snd (getPairs f)

-- Check if function
isFun :: (Eq a, Eq b) => Fun a b -> Bool
isFun f = imFun f `subSet` cod f && and [ numUnique [y' | (x',y') <- getPairs f, x' == x] == 1 | x <- unSet $ dom f]

-- Apply function
applyFun :: Fun a b -> a -> b
applyFun f a = head [y | (x,y) <- getPairs f]

-- Injective / Surjective
isInj :: (Eq a, Eq b) => Fun a b -> Bool
isInj f = and[ numUnique (nub $ [x' | (x',y') <- getPairs f, y' == y]) == 1 | y <- unSet $ cod f ]

isSurj :: Eq b => Fun a b -> Bool
isSurj f = imFun f == cod f

-- Conversion
toFun :: (Eq a, Eq b) => (a -> b) -> [a] -> [b] -> Fun a b
toFun f s t = Fun (toSet s) (toSet t) (toSet [(x,f x) | x <- s]) 

fromFun :: Eq a => Fun a b -> (a -> b) 
fromFun f a = head [y | (x,y) <- getPairs f, x == a]