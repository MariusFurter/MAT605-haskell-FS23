-- streams
ones = 1:ones
nats = 0: map (+1) nats
fib = 0:1:zipWith (+) fib (tail fib)

-- sieve of Eratosthenes
sift :: Integral a => [a] -> [a]
sift (n:xs) = n : sift ( filter (\m -> m `rem` n /= 0) xs )
primes = sift [2..]

-- dynamical systems
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

nats' = iterate' (+1) 0

double x | 2*x < 1 = 2*x
         | otherwise = 2*x - 1 

doubleOrbit = iterate' double 0.3

-- power series
shiftl :: Integer -> [a] -> [a]
shiftl 0 xs = xs
shiftl n xs = tail $ shiftl (n-1) xs

shiftr :: Num a => Integer -> [a] -> [a]
shiftr 0 xs = xs
shiftr n xs = 0 : shiftr (n-1) xs

zero :: Num a => [a]
zero = 0:zero

plus :: Num a => [a] -> [a] -> [a]
plus = zipWith (+)

times :: Num a => [a] -> [a] -> [a]
times (a0:as) (b0:bs) = ((a0*b0):zero) `plus` 
                        shiftr 1 ( map (*b0) as ) `plus`
                        shiftr 1 ( map (*a0) bs ) `plus`
                        shiftr 2 ( times as bs )