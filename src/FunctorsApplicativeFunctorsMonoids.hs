-- src: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--

-- for intersperse

-- for applicative functors
import Control.Applicative
import qualified Data.Char as Char
import Data.List

--
-- FUNCTORS
-- - basicly boxes/contexts which can be mapped over using fmap.
-- - if something is an instance of the Functor typeclass haskell does not check
-- for you if it does obey the functor laws. (need to do yourself to be sure the
-- rules hold regardless of which functor you are using)
--
-- FUNCTOR LAWS
-- - two rules that must hold.
-- 1. mapping id over the functor must yield the same result as the given
-- functor.
-- e.g. fmap id [1,2,3] == id [1,2,3]
-- NOTE(pierre): this normally holds. It only does not hold if we change
-- something automaticly when we are mapping.
-- 2. function composition does yield the same result.
-- e.g. fmap g $ fmap f [1,2,3] == fmap (g . f) [1,2,3]

-- ghci examples with list functor (id and function composition)
-- Prelude> fmap (+1) $ fmap (*2) [1,2,3]
-- [3,5,7]
-- Prelude> fmap ((+1) . (*2)) [1,2,3]
-- [3,5,7]

-- example for something that was is an instance of functor typeclass but not a functor.
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- Why is this not a functor? It does implement the functor typeclass?!
-- - id law does NOT hold. Because when mapping, we change the value of
-- something. --> fmap id (CJust 1 2) /= id (CJust 1 2)

-- io functor as example:
-- MAYBE(pierre): how to overwrite GHC.Base ?
-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

noIOFunctor = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

useIOFunctor = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-- take input, make it uppercase, reverse it, and put '-' after each char.
useIOFunctor' = do
  line <- fmap (intersperse '-' . reverse . map Char.toUpper) getLine
  putStrLn line

-- NOTE(pierre): functions are also a functor, did not really understand this
-- for now, but just accept it and continue. ((->) r) notation is weird to me
-- right now.

-- LIFTING
-- since everything is curried (takes one argument and returns a function that
-- takes one argument until no more arguments to be taked) fmap can be seen as
-- 1) :t fmap :: Functor f => (a -> b) -> f a -> f b
-- --> fmap takes a function and a functor and returns a functor with that
-- function applied to the boxed value.
-- but also as
-- 2) :t fmap :: Functor f => (a -> b) -> (f a -> f b)
-- --> fmap takes a function that takes one argument and returns a value and
-- returns a function that applies this function over a functor.

-- 1)
functorTest = fmap (+ 1) [1, 2, 3]

-- 2)
-- NOTE(pierre): Why did I have to put type annotations for this to compile?
-- I think because every function must be typed? Why does haskell not infere
-- that it takes a functor and returns a functor?
-- example: functor over numbers, since f b -> f b are both of types number.
functorTest' :: (Functor f, Num b) => f b -> f b
functorTest' = fmap (+ 2)

-- APPLICATIVE FUNCTORS
-- - using functors we can partially apply functions to another functor. This
-- functor now holds partially applied functions on his boxed values.
-- examples:
testApplicativeFunctor = do
  -- x will be a list of functions that take a parameter and return a value.
  let x = fmap (*) [1, 2, 3, 4]
  -- now we take every function of that functor and apply the value 2 to it.
  -- --> we will multiply every value of our applicatve functor with 2.
  putStrLn $ show $ fmap (\f -> f 2) x

-- we can apply multi parameter function to a functor and then map functions
-- over them that take a function and apply the rest of the parameters.
-- ghci> let a = fmap (*) [1,2,3,4]
-- ghci> :t a
-- a :: [Integer -> Integer]
-- ghci> fmap (\f -> f 9) a
-- [9,18,27,36]

-- MAYBE(pierre): Test pattern-match with just to map function that is inside
-- a functor over another functor.
-- "
-- But we can't map a function that's inside a functor over another functor with
-- what fmap offers us. We could pattern-match against the Just constructor to
-- get the function out of it and then map it over Just 5, but we're looking for
-- a more general and abstract way of doing that, which works across functors.
-- "
-- --> This can be done with the Applicative typeclass:
-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- If an instance of functor is part of the typeclass applicative(meaning it is
-- an applicative) then we can map the function of a applicative over the values
-- of other applicatives.

-- pure --> takes a value and returns an applicative functor with that value
-- inside it's box/context
-- <*> --> takes a functor with a function and a functor with values and returns
-- a functor with the function applied over the functors values.

-- applicative Functor for Maybe
-- instance Main.Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   (Just f) <*> x = fmap f x

-- ghci
-- :l FunctorsApplicativeFunctorsMonoids.hs
test1 = pure (+ 3) <*> Just 4

-- Just 7
test2 = pure (* 2) <*> Nothing

-- Nothing
test3 = pure (+) <*> Just 3 <*> Just 5

-- Just 8

-- Main.pure f <*> x == fmap f x --> Control.Applicative exports a function that
-- does just this in short term and infix notation.
-- f <$> x = fmap f x NOTE(pierre): f is a function and not a functor here :] X
-- is the applicative functor.
test4 = (+) <$> Just 3 <*> Just 5

-- Just 8

-- how [] is an applicative functor:
-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]
-- examples:
test5 = [(subtract 1), (* 2), (+ 50)] <*> [1, 2, 3]

-- [0,1,2,2,4,6,51,52,53]
test6 = [(subtract), (*), (+)] <*> [1, 2, 3] <*> [4, 5, 6]

-- all possible products of [2,5,10] and [8,10,11]
test7 = [x * y | x <- [2, 5, 6], y <- [8, 10, 11]]

-- using <*>
test8 = (*) <$> [2, 5, 6] <*> [8, 10, 11]

-- products value > 50:
test9 = filter (> 50) $ (*) <$> [2, 5, 6] <*> [8, 10, 11]

-- how IO is an applicative functor:
-- instance Applicative IO where
--     pure = return
--     a <*> b = do
--         f <- a
--         x <- b
--         return (f x)
-- examples:
test10 :: IO String
test10 = do
  a <- getLine
  b <- getLine
  return $ a ++ b

test11 :: IO String
-- TODO(pierre): why did this not work?
-- test11 = return $ (++) <$> getLine <*> getLine
test11 = (++) <$> getLine <*> getLine

-- NOTE: if we are binding some IO action to names and then calling functions on
-- these names, rather use applicative functor syntax <$> and <*>
test12 = do
  a <- (++) <$> getLine <*> getLine
  -- first <- getLine
  -- second <- getLine
  -- let concat = first ++ second
  putStrLn $ "You typed in the lines: " ++ a

-- how ((->) r) FUNCTIONS are an applicative functor.
-- NOTE(pierre): I still do not understand this ((->) r) thingy. I think it
-- means functions.
-- TODO(pierre): ask online about this, research more later.
-- - This is about mapping functions!
-- - Basicly left to right ends up in inside (right) out (left) function
-- composition.
-- instance Applicative ((->) r) where
--     pure x = (\_ -> x)
--     f <*> g = \x -> f x (g x)
-- examples:
test13 = (+) <$> (+ 2) <*> (* 2) $ 100

-- results in ((100 + 2) + (100 * 2))
-- these are the equal.
test14a x = (+ 2) <$> (* 3) $ x

test14b x = 2 + (3 * x)

-- these are equal.
test15a x = (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2) $ x

test15b x = (\x y z -> [x, y, z]) (x + 3) (x * 2) (x / 2)

-- how ZipList is an applicative functor.
-- instance Applicative ZipList where
--         pure x = ZipList (repeat x)
--         ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
-- - since [] / list does apply all given functions on all given entries.
-- e.g. [(+1),(*2)] <*> [1,2,3] === [2,3,4,2,4,6]
-- we need a type to apply the first function to the first entry, the second to
-- the second entry etc...
-- --> ZipList was born!
-- NOTE: resulting list will be of length of the shorter of both given lists(
-- same as normal zipWith function)
test16a = ZipList [(+ 1), (* 2)] <*> ZipList [1, 2]

test16b = ZipList [2, 4]

-- haskell way to generate a minimal context for the <*> function to yield the
-- correct result. Just generate a lazy infinite list of that value :D
-- making tuples is also a function
test17a = (,,)

-- "The (,,) function is the same as \x y z -> (x,y,z).
-- Also, the (,) function is the same as \x y -> (x,y)."
testTest17a = test17a 2 4 10

test18 = getZipList $ (,,) <$> ZipList "hello" <*> ZipList "zip" <*> ZipList "world"

-- liftA2
-- :t liftA2
-- --> shortcut for writing f <$> a <*> b
-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- definition
liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b

-- --> takes a function, applies it to both values of the applicative
-- functors/boxes/contexts and returns a boxed value
test19 = getZipList $ liftA2 (,) (ZipList "hello") (ZipList "zip")

-- we can box boxed values (have applicative functor that has an applicative
-- functor inside his value). [] is a box and Just is a box.
test20 = fmap (\x -> [x]) (Just 4)

-- these are equal
test21 = (:) <$> (Just 3) <*> (Just [4])

test22 = liftA2 (:) (Just 3) (Just [4])

-- takes list of applicatives and returns an applicative with a list as its
-- result values.
sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldl (\acc x -> liftA2 (:) x acc) (pure [])

sequenceA'' :: Applicative f => [f a] -> f [a]
sequenceA'' = foldr (\x acc -> liftA2 (:) x acc) (pure [])

-- solution: either recursion or fold.
sequenceAq :: (Applicative f) => [f a] -> f [a]
sequenceAq [] = pure []
sequenceAq (x : xs) = (:) <$> x <*> sequenceAq xs

-- NOTE: since liftA2 (:) takes two arguments we can drop the \x and acc :]
sequenceA''' :: Applicative f => [f a] -> f [a]
sequenceA''' = foldr (liftA2 (:)) (pure [])

test23 = sequenceA''' [Just 1, Just 2, Just 3]

-- since here the function is the applicative functor we get a result (\x ->
-- [x+3,x*2,x+1]) therefore we have to pass another value to this :]
test24 = sequenceA''' [(+ 3), (* 2), (+ 1)] 5

test25 = sequenceA''' [[1, 2, 3], [4, 5, 6]]

test26 = getZipList $ sequenceA''' [ZipList [1, 2, 3], ZipList [4, 5, 6]]

-- TODO(pierre): why is the result the empty list?
test27 = sequenceA''' [[1, 2, 3], [4, 5, 6], [3, 4, 4], []]

-- somehow applying sequenceA'/''/''' has semantic of "if any is false, display
-- false" --> if all correct, then display results.
-- --> sequenceA is similar to map when we are mapping over an array of functions.
test28a = map (\f -> f 7) [(+1),(*2),(+20)]
-- will apply (:) to each function (+1), (*2), (+20) resulting in a list of
-- functions that take one i
test28b = sequenceA''' [(+1),(*2),(+20)]


-- sequenceA''' :: Applicative f => [f a] -> f [a]
-- sequenceA''' = foldr (liftA2 (:)) (pure [])
-- same as
sequenceA'''' applicativeFunctorsList = foldr (liftA2 (:)) (pure []) applicativeFunctorsList

-- input [f 1,f 2,f 3] will yield
-- 3:[]; 2:[3]; 1:[2,3] --> f [1,2,3]
-- same as
sequenceA''''' applicativeFunctorsList = foldr (\x acc -> liftA2 (:) x acc) (pure []) applicativeFunctorsList

-- with input [f +1, f *2, f +20]
-- applicative functor here is function. with example [(+1), (*2), (+20)]
-- (+20):[]; (*2):[(+20)]; (+1):[(*2),(+20)] --> f [(+1),(*2),(+20)]
-- --> ([(+1),(*2),(+20))
-- TODO(pierre): Ask in matrix,slack or reddit for the solution/explanation to
-- this result.

test29 = sequenceA''' [[1,2,3],[4,5,6]]
-- TODO:continue here.
