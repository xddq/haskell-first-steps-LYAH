-- @file Practicing Haskell by going through the book
-- http://learnyouahaskell.com and coding some functions based on their given
-- description.

-- creates module which can be imported elsewhere
module CodeBaseLibrary
  (
  )
where

-- map: takes function and a list. Applies function to each element in the list.
-- Will return a new list.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- map: takes function and applies it to each element in the list using list
-- comprehension (I guess recursion is the more 'functional' way/syntax)
map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f xs = [f x | x <- xs]

-- filter: takes a predicate and a list. When the predicate is applied to an
-- element and it returns false, it will be filtered from the resulting list.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  -- otherwise is a constant that is defined as True
  | otherwise = filter' f xs

-- zip: takes to lists and returns a list of tuples. stops once one list is
-- empty.
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- elem: takes an element and a list. Returns True if elem is in list, else
-- False.
-- TODO(pierre): why page not found for Eq docu? src:
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:EQ I
-- I THINK: (Eq a) means that a returns a result for the (==) and (/=) function.
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = elem' x ys

-- takes a function and applies it twice to the second argument
applyFuncTwice :: (a -> a) -> a -> a
applyFuncTwice f x = f (f x)

-- continue here:
-- http://learnyouahaskell.com/higher-order-functions#higher-orderism
