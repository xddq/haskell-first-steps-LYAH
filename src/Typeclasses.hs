-- Typeclasses are like interfaces. Classes have NOTHING in common with OOP
-- classes. Lets forget about OOP for now and try to grasp haskell and FP.
-- NOTE(pierre): like this we can overwrite stuff in prelude without having to
-- prefix it with module name :]
module Typeclasses () where

import qualified Data.Map as Map
-- NOTE(pierre): if you see an error for loading this file in ghci.
-- Start ghci repl from src folder!
-- src: https://www.reddit.com/r/haskell/comments/53qxqk/how_to_import_modules_from_the_same_directory/
import RecursiveDataStructures
import Prelude hiding (Functor, fmap)

-- class Keyword is used to define typeclasses.
-- This is how Eq is defined in base Prelude:
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

-- lets define new class instances from Eq class.
-- remember: left from = is the type constructor
-- right from = -> value constructor. Arity can be >=0.
-- sum type -> since its red (or/+) yellow (or/+) green
data TrafficLights = Red | Yellow | Green

-- we could just derive (Show, Read, Eq, etc..)
-- but we want to create our own Eq logic.
-- instance keyword -> making a type an instance of a typeclass.
-- The typeclass here is Eq and the type is TrafficLights.
-- Since in Eq we define x == y = not (x /= y) we get the minimal complete
-- defintion by just specifying the == cases. Otherwise we would have to also
-- define the /= cases.
instance Eq TrafficLights where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

-- if we had Eq like this. Then we have to define all the == and all the /=
-- cases. Since Haskell now has no way to find out whether/how these cases are
-- related!
-- class BadEq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- create instance of Show by hand (instead of deriving) -> will allow us to
-- create custom output of show.
instance Show TrafficLights where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

-- we can also create type classes with subclasses. Subclass here would be Eq.
-- Subclass just means that the constraing Eq must be fullfilled for the
-- definition.
-- Snippet from Prelude Num definition:
-- class (Eq a) => Num a where
-- here we can see that every Num at least has to support the Eq type
-- constraints. (implement == and /=)

-- create instance of Maybe by hand. Because Maybe Int, Maybe String, etc.. are
-- concrete types we can use Maybe m for matching multiple types.
-- We have to make sure that the type of m does fullfill the Eq constraint,
-- otherwise we could not define Eq for Maybe.
-- TODO(pierre): why can we use x and y here? When we have m defined?
-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

-- can write :info anyTypeClassHere
-- to see the type definition!

-- Lets mimic javascript true/false results inside haskell!!
-- each concrete type instance of this typeclass will a function that takes
-- a parameter and returns a value of type Bool.
class YesNo a where
  yesNo :: a -> Bool

-- 0 is false, rest is true
instance YesNo Int where
  yesNo 0 = False
  yesNo _ = True

-- empty list is false, rest is true NOTE: will also contain string case since
-- String is a list of chars [Char] :]
instance YesNo [a] where
  yesNo [] = False
  yesNo _ = True

-- True -> True, False -> False
-- NOTE: id="identity" returns input as same output value.
instance YesNo Bool where
  yesNo = id

-- handle Maybe a cases. Just -> True, Maybe -> False
instance YesNo (Maybe a) where
  yesNo (Just _) = True
  yesNo Nothing = False

-- for (in this file non existant) binary tree.
-- instance YesNo BinaryTree a where
--     yesNo EmptyTree = False
--     yesNo _ = True

instance YesNo TrafficLights where
  yesNo Red = False
  yesNo _ = True

-- examples:

-- * Main> yesNo $ length [1..20]

-- True

-- * Main> yesNo ""

-- False

-- * Main> yesNo (Just 4)

-- True

-- * Main> yesNo Nothing

-- False

-- * Main> yesNo Red

-- False

-- * Main> yesNo Green

-- True

-- * Main>

-- TODO(pierre): Is there a way to have Either b c as argument?
-- (Either y z) does not work.
-- Takes into account that b and c may be of different type by returning an
-- either
ifElse :: (YesNo a) => a -> b -> c -> Either b c
ifElse x y z
  | yesNo x = Right z
  | otherwise = Left y

-- * Main> ifElse "" "falsecase" "truecase"

-- Left "falsecase"

-- * Main> ifElse "12" "falsecase" "truecase"

-- Right "truecase"

-- FUNCTOR
-- default implementation of functor typeclass.
-- meaning: fmap is a function that takes a function that maps values from type
-- a to type b. The functor gets a functor (sort of container that contains our
-- values, e.g. a list for lists..!) which will contain values of type a and map
-- them inside the same functor to values of type b.
-- functor is a type that acts as a box for value/s
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- For lists fmap is straight up defined by map.

-- * Main> :info map

-- map :: (a -> b) -> [a] -> [b] 	-- Defined in ‘GHC.Base’
-- here the functor/container/box that contains our values of type a and type b
-- will be the list ([])
-- literally the functor typeclass instance for lists.
instance Functor [] where
  fmap = map

-- Functor -> applies function and keeps box
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

functorMaybeTest = fmap (++ " world") (Just "hello")

-- Functor for trees. tree is the box.
instance Functor BinaryTree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftTree rightTree) = Node (f x) (fmap f leftTree) (fmap f rightTree)
increaseAllValuesInTree = fmap (+100) insertNumbersIntoTree

-- Functor for Either. Either is the box.
-- Functor always takes one type parameter. Either takes two.. So we can use it
-- when partially applying either.
-- TODO(pierre): Think about this one. Did need to check solution to get it
-- working.. Why can we not apply Left (f x) ?
instance Functor (Either a) where
  fmap f (Left x) = Left x
  fmap f (Right x) = Right (f x)

-- instance Functor (Map.Map k) where
--     fmap f (Map.singleton x y) = fmap (Map.singleton x (f y))

-- src: https://stackoverflow.com/questions/27197419/haskell-functors
instance (Ord k) => Functor (Map.Map k) where
  fmap f m = Map.fromList $ map (\(x, y) -> (x, f y)) $ Map.toList m
sampleMap = Map.fromList [(1, 2), (3, 4), (5, 6)]
testMapFunctor = fmap (+ 1) sampleMap

-- ADDITIONAL FUNCTOR LAWS:
-- 1) correct identity function (e.g. fmap (\a -> a) boxWithValues) returns the
-- same result as boxWithValues.
-- 2) keeps order from boxWithValue in result
-- more detail on these later.


-- KINDS and SOME-FOO
-- TODO(pierre): continue wiht KINDS and SOME-FOO :]
