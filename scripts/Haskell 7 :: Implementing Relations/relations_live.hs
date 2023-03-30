import SetList

type Rel a = Set (a,a)
type Rel' a = a -> a -> Bool

-- Return domain / range of a relation for Rel a
domRel :: Eq a => Rel a -> Set a
domRel r = toSet [x |(x,y) <- unSet r]

domRel' :: Eq a => Rel a -> Set a
domRel' r = toSet [fst xs | xs <- unSet r]

domRel'' :: Eq a => Rel a -> Set a
domRel'' r = toSet (map fst (unSet r))

-- Return domain / range of a relation for Rel' a

-- Check if pair in relation
inRel ::Eq a => (a,a) -> Rel a -> Bool
inRel = inSet

-- Check relation properties
-- Reflexivity
reflRel :: Eq a => Set a -> Rel a -> Bool
reflRel a r = and [(x,x) `inRel` r | x <- unSet a]

-- Symmetry
symmRel :: Eq a => Rel a -> Bool
symmRel r = and [(y,x) `inRel` r | (x,y) <- unSet r ]

-- Transitivity
transRel :: Eq a => Rel a -> Bool
transRel r = and [ (x,y') `inRel` r | 
                        (x,y) <- unSet r, (x',y') <- unSet r, y == x'  ]
-- Relation composition
infix 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = toSet [(x,z') | (x,y) <- unSet r, (y',z') <- unSet s, y == y' ]