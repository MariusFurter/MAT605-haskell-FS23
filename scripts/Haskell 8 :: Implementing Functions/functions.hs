import SetList
import Data.List (nub)

-- Datatype for function
data Fun' a b = Fun' (Set a) (Set b) (Set (a,b)) deriving Show 

dom' :: Fun' a b -> Set a
dom' (Fun' d c p) = d

data Fun a b = Fun { dom :: Set a, 
                     cod :: Set b, 
                     pairs :: Set (a,b) } 
                        deriving Show

-- Check if Fun is actually a function
getPairs :: Fun a b -> [(a,b)]
getPairs f = unSet (pairs f) 

numUnique :: Eq a => [a] -> Int
numUnique = length . nub 

numYs :: (Eq a, Eq b) => Fun a b -> a -> Int 
numYs f x = numUnique [y' | (x',y') <- getPairs f, x' == x ]

isFun :: (Eq a, Eq b) => Fun a b -> Bool 
isFun f = imFun f `subSet` cod f && 
            and [ numYs f x == 1| x <- unSet (dom f) ]


-- Get the image of a function
imFun :: Eq b => Fun a b -> Set b
imFun f = toSet [y | (x,y) <- getPairs f] 

-- Check if function is injective / surjective
isSurj :: (Eq b) => Fun a b -> Bool
isSurj f = imFun f == cod f

numXs :: (Eq a, Eq b) => Fun a b -> b -> Int
numXs f y = numUnique [x' | (x',y') <- getPairs f, y' == y ]

isInj :: (Eq a, Eq b) => Fun a b -> Bool
isInj f = and [ numXs f y <= 1 | y <- unSet (cod f) ]

-- Conversion
toFun :: (Eq a, Eq b) => (a -> b) -> [a] -> [b] -> Fun a b
toFun f s t = Fun (toSet s) (toSet t) (toSet [ (x, f x) | x <- s ])

fromFun :: (Eq a) => Fun a b -> (a -> b)
fromFun f x = head [y' | (x',y') <- getPairs f, x' == x ]

f = Fun (Set [1,2]) (Set ['a','b']) (Set [(1,'a'),(2,'b')])
f' = fromFun f 
g = Fun (Set [1,2]) (Set ['a','b']) (Set [(1,'a'),(2,'a')])