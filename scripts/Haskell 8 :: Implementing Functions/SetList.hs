module SetList
where

import Data.List ( nub )

-- Datatype for Sets
data Set a = Set [a] deriving Show

instance Eq a => Eq (Set a) where
        (==) :: Eq a => Set a -> Set a -> Bool
        s == t = s `subSet` t && t `subSet` s 

-- Conversion
unSet :: Set a -> [a]
unSet (Set xs) = xs 

toSet :: Eq a => [a] -> Set a
toSet xs = Set (nub xs) 

-- Check whether x is an element of a set
inSet :: Eq a => a -> Set a -> Bool
inSet x s = x `elem` unSet s 

inSet' :: Eq a => a -> Set a -> Bool
inSet' x (Set ys) = x `elem` ys 

-- Check if one set is a subset of another
        -- recursive definition
        -- 'and' together with list comprehension
        -- 'all' together with list comprehension
subSet :: Eq a => Set a -> Set a -> Bool 
subSet (Set []) _ = True
subSet (Set (x:xs)) t = x `inSet` t && Set xs `subSet` t 

subSet' :: Eq a => Set a -> Set a -> Bool 
subSet' s t = and [x `inSet` t | x <- unSet s] 

subSet'' :: Eq a => Set a -> Set a -> Bool 
subSet'' s t = all (`inSet` t) (unSet s) 

-- Union
unionSet :: Eq a => Set a -> Set a -> Set a
unionSet s t = toSet (unSet s ++ unSet t)

unionSet' :: Eq a => Set a -> Set a -> Set a
unionSet' (Set xs) (Set ys) = toSet (xs ++ ys) 

-- Intersection 
intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet s t = toSet [x | x <- unSet s, x `inSet` t] 