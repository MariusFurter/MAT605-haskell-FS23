import SetList

type Rel a = Set (a,a)

type Rel' a = a -> a -> Bool

domRel :: Eq a => Rel a -> Set a
domRel r = toSet [x | (x,_) <- unSet r]

ranRel :: Eq a => Rel a -> Set a
ranRel r = toSet [y | (_,y) <- unSet r]

invRel :: Eq a => Rel a -> Rel a
invRel r = toSet [(y,x) | (x,y) <- unSet r]

invRel' :: Eq a => Rel' a -> Rel' a
invRel' r = flip r

inRel :: Eq a => (a, a) -> Rel a -> Bool
inRel = inSet

inRel' :: Eq a => a -> a -> Rel' a -> Bool
inRel' x y r = r x y

reflRel :: Eq a => Set a -> Rel a -> Bool
reflRel s r = and [(x,x) `inRel` r | x <- unSet s]

reflRel' :: Eq a => Set a -> Rel' a -> Bool
reflRel' s r = and [r x x | x <- unSet s]

symRel :: Eq a => Rel a -> Bool
symRel r = and [(y,x) `inRel` r | (x,y) <- unSet r]

transRel :: Eq a => Rel a -> Bool
transRel r = and [(x,y') `inRel` r | (x,y) <- unSet r, (x',y') <- unSet r, y == x']

infix 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = toSet [(x,z) | (x,y) <- unSet r , (y',z) <- unSet s, y == y']

compRel' :: Eq a => Set a -> Rel' a -> Rel' a -> Rel' a
compRel' a r s x y = or [ r x u && s u y  | u <- unSet a ]