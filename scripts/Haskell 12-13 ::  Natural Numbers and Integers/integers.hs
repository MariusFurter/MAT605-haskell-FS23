import Nats

-- Integers as a union (N u N)
data MyInt = Pos Nat | Neg Nat deriving Show 

instance Eq MyInt where
    (==) (Pos Z) (Neg Z) = True
    (==) (Pos n) (Pos m) = n == m
    (==) (Neg n) (Neg m) = n == m
    (==) _ _ = False

addMyInt :: MyInt -> MyInt -> MyInt
addMyInt (Pos n) (Pos m) = Pos (n + m)
addMyInt (Neg n) (Neg m) = Neg (n + m)
addMyInt (Pos n) (Neg m) | m <= n = Pos (n - m)
                         | otherwise = Neg (m - n)
addMyInt (Neg n) (Pos m) | n <= m = Pos (m - n)
                         | otherwise = Neg (n - m) 

-- Integers as "difference classes": (n,m) corresponds to n-m  
data DiffInt = Diff Nat Nat deriving Show

-- (n,m) = (l,p) iff n+p = m+l
instance Eq DiffInt where
    (==) (Diff n m) (Diff l p) = n+p == m+l

-- (n-m) + (l-p) = (n+l) - (m+p)
addDiffInt :: DiffInt -> DiffInt -> DiffInt
addDiffInt (Diff n m) (Diff l p) = Diff (n+l) (m+p) 