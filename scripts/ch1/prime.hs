divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

ldf :: Integral a => a -> a -> a
ldf k n
  | divides k n = k
  | k ^ 2 > n = n
  | otherwise = ldf (k + 1) n

ld :: Integral a => a -> a
ld n = ldf 2 n

prime0 :: Integral a => a -> Bool
prime0 n
  | n < 1 = error "not a positive integer"
  | n == 1 = False
  | otherwise = ld n == n