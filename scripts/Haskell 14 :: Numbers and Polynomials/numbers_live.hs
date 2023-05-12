import Data.Ratio
import Data.Complex

-- "cabal install --lib poly"
import Data.Poly

newton :: (Rational -> Rational) -> (Rational -> Rational) 
            -> Rational -> Int -> Rational
newton f f' x0 0 = x0
newton f f' x0 n = x - (f x / f' x)
            where x = newton f f' x0 (n-1)

newton2 :: (Rational -> Rational) -> (Rational -> Rational) 
           -> Rational -> Rational -> Rational
newton2 f f' x0 eps | abs (x0 - x1) < eps = x1
                    | otherwise = newton2 f f' x1 eps
        where  x1 = x0 - (f x0 / f' x0)

newtonPoly :: VPoly Rational -> Rational -> Int -> Rational 
newtonPoly p = newton f f'
    where  f = eval p
           f' = eval $ deriv p

newtonPoly2 :: VPoly Rational -> Rational -> Rational -> Rational 
newtonPoly2 p = newton2 f f'
    where  f = eval p
           f' = eval $ deriv p
