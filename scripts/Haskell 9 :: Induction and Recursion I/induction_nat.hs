data Nat = Z | S Nat deriving (Show, Eq) 

-- Conversion
fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n) = fromNat n + 1 

toNat :: Int -> Nat
toNat 0 = Z
toNat n | n > 0 = S ( toNat (n-1) )
        | otherwise = error "number is negative"

-- Arithmetic (+,*,^)
plus :: Nat -> Nat -> Nat
plus n Z = n
plus n (S m) = S ( plus n m )       -- n + (m+1) = (n+m) + 1 

mult :: Nat -> Nat -> Nat
mult n Z = Z 
mult n (S m) = (mult n m) `plus` n  -- n * (m+1) = (n*m) + n

pow :: Nat -> Nat -> Nat
pow n Z = S Z
pow n (S m) = (pow n m) `mult` n   -- n^(m+1) = (n^m) * n

-- Comparison
leq :: Nat -> Nat -> Bool
leq Z _ = True
leq (S _) Z = False 
leq (S n) (S m) = leq n m 

geq :: Nat -> Nat -> Bool
geq n m = leq m n

lt :: Nat -> Nat -> Bool
lt n m = not (geq n m) 

gt :: Nat -> Nat -> Bool
gt n m = lt m n 

-- Folds over Nat 
-- foldn h c N = h ( h ( ... h( c ))) 
foldn :: (a -> a) -> a -> Nat -> a
foldn h c Z = c
foldn h c (S n) = h ( foldn h c n )

plus' :: Nat -> Nat -> Nat
plus' = foldn S 

mult' :: Nat -> Nat -> Nat
mult' n = foldn (plus' n) Z

pow' :: Nat -> Nat -> Nat
pow' n = foldn (mult' n) (S Z)