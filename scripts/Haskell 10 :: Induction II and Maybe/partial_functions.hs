-- data Maybe a = Nothing | Just a deriving (Show, Eq, Ord)

head' :: [a] -> a
head' [] = error "list is empty"
head' (x:xs) = x

head'' :: [a] -> Maybe a
head'' [] = Nothing
head'' (x:xs) = Just x

head''' :: [a] -> [a]
head''' [] = []
head''' (x:xs) = [x]

sqrt' :: Double -> [Double]
sqrt' 0.0 = [0.0]
sqrt' x = [- sqrt x, sqrt x]

-- fmap
fToMaybe :: (a -> b) -> Maybe a -> Maybe b
fToMaybe f Nothing = Nothing
fToMaybe f (Just x) = Just (f x) 

fToList :: (a -> b) -> [a] -> [b]
fToList f [] = []
fToList f (x:xs) = (f x):fToList f xs 

-- converting to Maybe and lists
toMaybe :: a -> Maybe a
toMaybe x = Just x

toList :: a -> [a]
toList x = [x]

-- flattening
flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just x)) = Just x

flattenList :: [[a]] -> [a]
flattenList [] = []
flattenList (x:xs) = x ++ flattenList xs
