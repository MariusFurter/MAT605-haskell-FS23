data Nat = Z | S Nat deriving (Eq, Show)

-- Arithmetic
plus :: Nat -> Nat -> Nat
plus n Z = n
plus n (S m) = S (plus n m)

mult :: Nat -> Nat -> Nat
mult n Z = Z 
mult n (S m) = (n `mult` m) `plus` n

pow :: Nat -> Nat -> Nat
pow n Z = S Z
pow n (S m) = pow n m `mult` n

-- Order
leq :: Nat -> Nat -> Bool
leq Z _ = True
leq (S _) Z  = False
leq (S n) (S m) = leq n m

geq :: Nat -> Nat -> Bool
geq n m = leq m n

-- strict inequality <
lt :: Nat -> Nat -> Bool
lt n m = not (geq n m)

gt :: Nat -> Nat -> Bool
gt n m = lt m n

-- Conversion
fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n) = (fromNat n) + 1

toNat :: Int -> Nat
toNat 0 = Z
toNat n | n > 0 = S (toNat (n - 1))
        | otherwise = error "number is negative"

-- Folding across natural numbers
foldn :: a -> (a -> a) -> Nat -> a
foldn s op Z = s
foldn s op (S m) = op ( foldn s op m )

excl :: Nat -> [Char]
excl m = foldn [] ('!':) m

plus' :: Nat -> Nat -> Nat
plus' n m = foldn n S m

mult' :: Nat -> Nat -> Nat
mult' n m = foldn Z (`plus'` n) m

pow' :: Nat -> Nat -> Nat
pow' n m = foldn (S Z) (`mult'` n) m



