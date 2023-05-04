import Nats

-- Implement a datatype for integers
-- Hint: Pairing a and b is the type: (a,b) 

-- Method 1
data Sign = Plus | Minus deriving (Eq, Show)
data SignedInt = Pair Sign Nat

instance Eq SignedInt where
    (==) (Pair Plus Z) (Pair Minus Z) = True
    (==) (Pair s n) (Pair t m) = m == n && s == t

-- Method 2
data DiffInt = Pair' Nat Nat
instance Eq DiffInt where
    Pair' n m == Pair' l p = n+p == m+l 

-- Method 3
data MyInt = Pos Nat | Neg Nat deriving Show
instance Eq MyInt where
    Pos Z == Neg Z = True
    Pos n == Pos m = n == m
    Neg n == Neg m = n == m
    _ == _ = False

-- Implementing arithmetic operations

-- Implementing Equality (typeclass Eq)