import Data.List

data CoolBool = Nope | Yup
data Number = Number Int deriving Show
data PairNumber = PairNumber Int Int deriving Show
data Rectangle = Rectangle Double Double deriving Show

data Set a = Set [a] deriving Show

instance Eq a => Eq (Set a) where
    (==) :: Eq a => Set a -> Set a -> Bool
    xs == ys = xs `subSet` ys && ys `subSet` xs 

unSet :: Set a -> [a]
unSet (Set xs) = xs

toSet :: Eq a => [a] -> Set a
toSet xs = Set (nub xs)
-- Challenge: Implement your own version of nub (hard?)

-- Check if x is an element of a set
inSet :: Eq a => a -> Set a -> Bool
inSet x (Set ys) = x `elem` ys

-- Check whether one set is a subset of another (3 ways: recursion with pattern matching | use 'and' with list comprehension | use 'any' with list comprehension)

subSet :: Eq a => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) ys = x `inSet` ys && Set xs `subSet` ys

subSet' :: Eq a => Set a -> Set a -> Bool
subSet' (Set xs) ys = and [ x `inSet` ys | x <- xs ]

subSet'' :: Eq a => Set a -> Set a -> Bool
subSet'' xs ys = and [ x `inSet` ys  | x <- unSet xs ]

subSet''' :: Eq a => Set a -> Set a -> Bool
subSet''' (Set xs) ys = all (\x -> x `inSet` ys) xs

subSet'''' :: Eq a => Set a -> Set a -> Bool
subSet'''' (Set xs) ys = all (`inSet` ys) xs

-- Define set theoretic operations: union, intersection, set difference
intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = Set [x | x <- xs, x `elem` ys ]

intersectSet' :: Eq a => Set a -> Set a -> Set a
intersectSet' (Set xs) ys = Set [x | x <- xs, x `inSet` ys ]

unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = toSet (xs ++ ys)

subtractSet :: Eq a => Set a -> Set a -> Set a
subtractSet (Set xs) ys = Set [x | x <- xs, not (x `inSet` ys) ]