module Nats where

import Data.Ratio

data Nat = Z | S Nat deriving (Eq, Show)

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

instance Ord Nat where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare (S n) (S m) = compare n m

instance Enum Nat where
    succ = S
    pred Z = Z
    pred (S n) = n 
    toEnum 0 = Z
    toEnum i | i < 0 = Z
             | otherwise = S ( toEnum (i-1) )
    fromEnum Z = 0
    fromEnum (S n) = fromEnum n + 1

instance Num Nat where
    (+) = foldn S 
    (*) n = foldn (+n) Z
    (-) = foldn pred 
    abs = id 
    signum Z = Z 
    signum n = (S Z)
    fromInteger 0 = Z
    fromInteger i | i < 0 = Z
                  | otherwise = S ( fromInteger (i-1) )

instance Real Nat where
    toRational n = toInteger n % 1

instance Integral Nat where
    toInteger = foldn (+1) 0
    quotRem n d | d > n = (Z, n)
                | otherwise = (S q, r) where
                    (q,r) = quotRem (n-d) d





