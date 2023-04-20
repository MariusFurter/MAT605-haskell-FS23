data Nat = Z | S Nat deriving (Eq, Show)

fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n) = fromNat n + 1 

toNat :: Int -> Nat
toNat 0 = Z
toNat x | x > 0 = S (toNat (x-1))
        | otherwise = error "negative number"

plus :: Nat -> Nat -> Nat
plus n Z = n
plus n (S m) = S (plus n m)

mult :: Nat -> Nat -> Nat
mult n Z = Z
mult n (S m) = n `plus` mult n m

pow :: Nat -> Nat -> Nat
pow n Z = S Z
pow n (S m) = n `mult` pow n m

leq :: Nat -> Nat -> Bool
leq Z _ = True
leq (S _) Z = False
leq (S n) (S m) = leq n m

qeg :: Nat -> Nat -> Bool
qeg n m = leq m n

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

plus' :: Nat -> Nat -> Nat
plus' n m = foldn S n m

mult' :: Nat -> Nat -> Nat
mult' n m = foldn (plus' n) Z m

expn' :: Nat -> Nat -> Nat
expn' n m = foldn (mult' n) (S Z) m
