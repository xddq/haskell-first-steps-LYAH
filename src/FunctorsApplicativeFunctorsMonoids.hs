-- src: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--
import qualified Data.Char as Char
-- for intersperse
import Data.List
-- for applicative functors
import Control.Applicative
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
    fmap f (CJust counter x) = CJust (counter+1) (f x)
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
functorTest = fmap (+1) [1,2,3]
-- 2)
-- NOTE(pierre): Why did I have to put type annotations for this to compile?
-- I think because every function must be typed? Why does haskell not infere
-- that it takes a functor and returns a functor?
-- example: functor over numbers, since f b -> f b are both of types number.
functorTest' :: (Functor f, Num b) => f b -> f b
functorTest' = fmap (+2)


-- APPLICATIVE FUNCTORS
-- - using functors we can partially apply functions to another functor. This
-- functor now holds partially applied functions on his boxed values.
-- examples:
testApplicativeFunctor = do
    -- x will be a list of functions that take a parameter and return a value.
    let x = fmap (*) [1,2,3,4]
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
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-- If an instance of functor is part of the typeclass applicative(meaning it is
-- an applicative) then we can map the function of a applicative over the values
-- of other applicatives.

-- pure --> takes a value and returns an applicative functor with that value
-- inside it's box/context
-- <*> --> takes a functor with a function and a functor with values and returns
-- a functor with the function applied over the functors values.

-- applicative for Maybe
-- TODO: continue here.
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x
