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

-- fold: starting with a value and a binary function (function that takes two
-- arguments). Then we apply the value and the first(or last) value of the list
-- to the binary function. We then take the result and apply it to the next
-- first or last value of the list. etc.. until we finished with the list.
-- foldl -> start with first elem of list foldr -> start with last elem of list.
-- initial value is also called "accumulator".
-- TODO(pierre): check base code for implementation.
-- foldl' :: (a -> a -> b) -> a -> [a] -> b
-- foldl' _ x [] = id(x)
-- foldl' f x (y:ys) = f (f x y) (foldl' ys)

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- Anyhoo, let's implement another function with a left fold before moving on to
-- right folds. I'm sure you all know that elem checks whether a value is part
-- of a list so I won't go into that again (whoops, just did!). Let's implement
-- it with a left fold.
-- NOTE: really weird that left and right fold has the acc and x swapped?!
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- map with foldr goes from last elem and applies function to each elem and
-- current accumulator
map''' f xs = foldr (\x acc -> f x : acc) [] xs

-- map with foldl
map'''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- NOTE: pref x:xs notation over xs ++ x since appending to linked list needs
-- full iteration through it

-- folds together with maps and filters are the most often used functions in
-- functional programming.
-- foldl1 and foldr1 -> drop the initial value for accumulator.

-- max with foldl
maximum' (x : xs) = foldl (\acc x -> if x > acc then x else acc) x xs

-- max with foldl1
maximum'' xs = foldl1 (\acc x -> if x > acc then x else acc) xs

-- implements sum with foldl1 and partial application of + function
sumFold :: (Num a) => [a] -> a
sumFold = foldl1 (+)

-- scanl -> foldl while writing all acculumators in the resulting list
-- scanr, scanl1 scanr1 equal to foldr, foldl1, foldr1
-- result will be in the last elem of the list.
a = scanl (\acc x -> acc + x) 0 [1 .. 10]

-- result is equal to foldl
ab = last (scanl (\acc x -> acc + x) 0 [1 .. 10])

abc = foldl (\acc x -> acc + x) 0 [1 .. 10]

-- head -> first elem
-- tail -> all but first elem
-- init -> all but last elem
-- last -> last elem
aa = (+ 1)

bb = (+ 2)

-- $ is used to make rhs of operator right associative (default is left
-- associative). Pretty useful to avoid having to write to many parentheses. f $
-- g x basicly puts a ( before g and ) after x.
-- f $ g x is equal to f ( g x)
-- sum (filter (> 10) (map (*2) [2..10])) is equal to
-- sum $ filter (> 10) $ map (*2) [2..10]

-- function composition f compose g (x) = f (g (x))
-- required that f and g take the same amount of arguments. And return a value
-- of a type that the other one can use.
-- Say we have a list of numbers and we want to turn them all into negative
-- numbers. One way to do that would be to get each number's absolute value and
-- then negate it, like so:
double = (* 2)

negateList = map (double . negate . abs) [1, -3, -100, 5, 4, 22]

-- partially applied function combined with composition:
abcd = sum (replicate 5 (max 6.7 8.9))

-- whatever max takes will be applied to max 6.7, then replicate 5 will be
-- called on the result and them sum will be called on the result.
abcde x = sum . replicate 5 . max 6.7 $ x

-- equal to (because of currying)
abcdef = sum . replicate 5 . max 6.7

-- NOTE: don't overdo composition without including let bindings.
-- example:
-- In the section about maps and filters, we solved a problem of finding the sum of
-- all odd squares that are smaller than 10,000. Here's what the solution looks
-- like when put into a function.
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

-- Being such a fan of function composition, I would have probably written that
-- like this:
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

-- However, if there was a chance of someone else reading that code, I would have
-- written it like this:
oddSquareSum'' :: Integer
oddSquareSum'' =
  let oddSquares = filter odd $ map (^ 2) [1 ..]
      belowLimit = takeWhile (< 10000) oddSquares
   in sum belowLimit


-- finished with everything including http://learnyouahaskell.com/higher-order-functions
-- continue with /modules in CodeModules.hs
