-- Arithmetic Operations: + - * / ^
-- Logical Operations: && || not
-- Comparison: == /= < <= > >=
-- Convert infix f to prefix: (f)
-- Convert prefix f to infix: `f`

-- Basic Functions
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else 2 * x

seven = "seven"

-- Lists
-- Concatenation: ++
-- Cons: :
-- Get index: !!
-- head, tail, last, init
-- length, null, reverse, take, drop
-- maximum, minimum, sum, product
-- elem

-- Ranges

-- List Comprehension
-- Exercise: Create ["he","hi","ho"] by list comprehension

add5 xs = [ x + 5 | x <- xs ]

-- Exercise: Write a function that removes all vowels from a string using list comprehension
isVowel letter = letter `elem` ['a','e','i','o','u','y']

removeVowels :: [Char] -> [Char]
removeVowels xs = [ letter | letter <- xs, not (isVowel letter) ]

-- Tuples
-- fst, snd operate on pairs
-- Exercise: Create a list [(1,'a'),(2,'b'),...,(26,'z')] using zip and ranges
l = zip [1..26] ['a'..'z']

-- Bonus Exercise: Create a list of triples (a,b,c) where a^2 + b^2 = c^2
t = [(a,b,c) | a <- [0..], b <- [0..a], c <- [0..b], (a^2 + b^2) == c^2 ]
