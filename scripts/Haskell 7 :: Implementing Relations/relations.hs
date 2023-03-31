import SetList

-- Relations R: A -> A
type Rel a = Set (a,a) 
-- Relations R: A -> B
type Rel' a b = Set (a,b) 
-- Relation R: A -> A as function
type FunRel a = a -> a -> Bool 
-- Relations R: A -> B as functions
type FunRel' a b = a -> b -> Bool

-- Domain / range of a relation
domRel :: Eq a => Rel a -> Set a
domRel r = toSet [fst xs | xs <- unSet r ] 

domRel' :: Eq a => Rel a -> Set a
domRel' r = toSet [x | (x,_) <- unSet r ] 

ranRel :: Eq a => Rel a -> Set a
ranRel r = toSet [snd xs | xs <- unSet r ] 

ranRel' :: Eq a => Rel a -> Set a
ranRel' r = toSet [y | (_,y) <- unSet r ] 

-- Check if (x,y) is in relation
inRel :: Eq a => (a,a) -> Rel a -> Bool 
inRel xs r = xs `inSet` r 

inRel' :: Eq a => (a,a) -> Rel a -> Bool 
inRel' = inSet 

-- Properties:
-- Reflexivity
reflRel :: Eq a => Set a -> Rel a -> Bool 
reflRel a r = and [(x,x) `inRel` r | x <- unSet a ] 

-- Symmetry
symRel :: Eq a => Rel a -> Bool
symRel r = and [ (y,x) `inRel` r | (x,y) <- unSet r] 

-- Transitivity
transRel :: Eq a => Rel a -> Bool 
transRel r = and [ (x,z') `inRel` r | 
                        (x,y) <- unSet r, (y',z') <- unSet r, y == y' ]

-- Relation composition
infix 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a 
r @@ s = toSet [(x,z') | (x,y) <- unSet r, (y',z') <- unSet s, y == y']