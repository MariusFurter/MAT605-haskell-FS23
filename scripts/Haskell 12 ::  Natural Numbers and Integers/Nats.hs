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
    toEnum i | i > 0 = S (toEnum (i-1))
             | otherwise = Z
    fromEnum Z = 0
    fromEnum (S n) = fromEnum n + 1

instance Num Nat where
    (+) = foldn succ
    (*) n m = foldn (+m) Z n 
    (-) = foldn pred
    abs = id
    signum Z = 0
    signum n = S Z
    fromInteger 0 = Z
    fromInteger i | i > 0 = S (fromInteger (i-1))
                  | otherwise = Z
     
instance Real Nat where
    toRational n = (toInteger n) % 1

instance Integral Nat where
    quotRem n d | d == 0 = error "division by zero"
                | d > n = (Z, n)
                | otherwise = (S q, r) where
                    (q, r) = quotRem (n-d) d
    toInteger :: Nat -> Integer
    toInteger = foldn (+1) 0
