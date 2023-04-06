import SetList
import Data.List (nub)

-- Datatype for functions
data Fun a b = Fun (Set a) (Set b) (Set (a,b)) deriving Show

-- Record syntax (creates functions called dom, cod, pairs)
data Fun' a b = Fun' {  dom :: Set a,
                        cod :: Set b,
                        pairs :: Set (a,b)} deriving Show
f = Fun' (toSet [1,2,3]) (toSet ['a','b','c']) (toSet [(1,'a'),(2,'a'),(3,'b')])

notf = Fun' (toSet [1,2,3]) (toSet ['a','b','c']) (toSet [(2,'a'),(3,'b')])


-- Get pairs
getPairs :: Fun' a b -> [(a,b)]
getPairs f = unSet (pairs f)

getPairs' :: Fun a b -> [(a,b)]
getPairs' (Fun dom cod pairs)  = unSet pairs

-- Return number of unique elements in list
numUnique :: Eq a => [a] -> Int
numUnique xs = length $ nub xs

numUnique' :: Eq a => [a] -> Int
numUnique' = length . nub 

-- Domain of definition of function
domDef :: Eq a => Fun' a b -> Set a
domDef f = toSet $ map fst (getPairs f)

-- Image of a function
imFun :: Eq b => Fun' a b -> Set b
imFun f = toSet $ map snd (getPairs f)

-- Check if Fun is actually a function
isFun :: (Eq a, Eq b) => Fun' a b -> Bool
isFun f = imFun f `subSet` cod f && 
                and [ numUnique [y' | (x',y') <- getPairs f, x' == x ] == 1 
                                                        | x <- unSet $ dom f]
alsoNotf = Fun' (toSet [1,2,3]) (toSet ['a','b','c']) (toSet [(1,'a'),(2,'a'),(3,'x')])

-- Apply a Fun' to an element of the domain
applyFun :: (Eq a, Eq b) => Fun' a b -> a -> b
applyFun f x | isFun f = head [y' | (x',y') <- getPairs f, x' == x]
             | otherwise = error "not actually a function"

-- Check injectivity
isInj :: (Eq a, Eq b) => Fun' a b -> Bool
isInj f = and [ numUnique [x' | (x',y') <- getPairs f, y' == y] == 1
                                                | y <- unSet $ imFun f]

-- Calculate inverse relation
invFun :: (Eq a, Eq b) => Fun' a b -> Fun' b a
invFun f = Fun' (imFun f) (dom f) (toSet [(y,x) | (x,y) <- getPairs f])

isInj' :: (Eq a, Eq b) => Fun' a b -> Bool
isInj' f = isFun $ invFun f

-- Check surjectivity
isSurj :: (Eq a, Eq b) => Fun' a b -> Bool
isSurj f = imFun f == cod f

-- Conversion
toFun :: (Eq a, Eq b) => (a -> b) -> [a] -> [b] -> Fun' a b
toFun f dom cod = Fun' (toSet dom) (toSet cod) (toSet [ (x, f x) | x <- dom] )