-- @file Practicing Haskell by going through the book
-- http://learnyouahaskell.com and coding some functions based on their given
-- description. Then adapting them when I don't get them right.

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

-- applying mult as infix using map. Will result in a list of partially applied
-- functions. [1*,2*,3*,..]
listOfMultFuns = map (*) [1 .. 30]

-- results in [3,6,9,..]
applyThreeToListOfMultFuns = map (\x -> x 3) listOfMultFuns

-- getting the element of a list by a given index? [1..] !! 3
fourthItemInList = [1 ..] !! 3

-- why/when to use lambdas?
-- Functions that are normally used only once and mostly passed to higher order
-- functions
lambdaFun = (\x -> x + 1)

-- equal to
noLambdaFun x = x + 1

-- builder for partially applied function that multiplies n times based on given
-- x.
nTimes x = (x *)

doubleTimes = nTimes 2

-- zipWith: joining two lists, putting the result of applying the given function
-- inside the resulting list.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- zipWith' test
-- zipWith' (\x y -> ((nTimes 3) x) + (nTimes 5) x) [1..10] [1..10]

-- flip: return given function but flip their input arguments
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- flip' test
numByDenom x y = x / y

-- numByDenom 5 4
-- flip' numByDenom 5 4

-- quicksort: pick pivot element, compare with whole list. put in the correct
-- place. Then pick pivot element of smaller and bigger than previous pivot and
-- repeat.
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
-- MAYBE(pierre): Why do we need parentheses here?
-- quicksort' (x:xs) = quicksort' filter (<x) xs ++ [x] ++ quicksort' filter (>=x) xs
-- quicksort' (x : xs) = quicksort' (filter (< x) xs) ++ [x] ++ quicksort' (filter (>= x) xs)
-- quicksort using let
quicksort' (x : xs) =
  let smallerSorted = quicksort' (filter (< x) xs)
      greaterOrEqualSorted = quicksort' (filter (>= x) xs)
   in smallerSorted ++ [x] ++ greaterOrEqualSorted

divisibleByX x y = x `mod` y == 0

divisibleBy3289 = flip' divisibleByX 3289

-- find the largest number under 100,000 that's divisible by 3829 NOTE(pierre):
-- It looks like this is lazy evaluation and we actually finish after getting
-- the first result.
findThatNum = head (filter divisibleBy3289 [100000, 99999 ..])

-- takeWhile function. It takes a predicate and a list and then goes from the
-- beginning of the list and returns its elements while the predicate holds
-- true. Once an element is found for which the predicate doesn't hold, it
-- stops.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

-- takewhile test:
-- takeWhile' (< 100) [0, 5 ..]

-- For our next problem, we'll be dealing with Collatz sequences. We take a
-- natural number. If that number is even, we divide it by two. If it's odd, we
-- multiply it by 3 and then add 1 to that. We take the resulting number and
-- apply the same thing to it, which produces a new number and so on. In
-- essence, we get a chain of numbers. It is thought that for all starting
-- numbers, the chains finish at the number 1.
-- TODO(pierre): Check Integral type. In general check haskell type definitions.
-- Have seen Ord, Eq, Integral, Num so far. Check them. (Probably at hoogle
-- since I can't use go to definition?)
collatzSeq :: (Integral a) => a -> [a]
collatzSeq 1 = [1]
collatzSeq x
  | even x = x : collatzSeq (x `div` 2)
  | otherwise = x : collatzSeq ((x * 3) + 1)

-- What to do if some functions are Int instead of Num?
-- length returns an Int instead of a Num a for historical reasons. If we wanted to return a more general Num a, we could have used fromIntegral on the resulting length.

-- currying:
-- These two are equal! That's why the type declaration always uses a -> b -> c
-- although coming from non functional background that looked somewhat weird.
addThree' :: (Num a) => a -> a -> a -> a
addThree' x y z = x + y + z

addThree'' :: (Num a) => a -> a -> a -> a
addThree'' = \x -> \y -> \z -> x + y + z

-- TODO(pierre): continue here with foldl.
-- http://learnyouahaskell.com/higher-order-functions
