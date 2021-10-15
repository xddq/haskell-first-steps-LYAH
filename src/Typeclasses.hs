-- Typeclasses are like interfaces. Classes have NOTHING in common with OOP
-- classes. Lets forget about OOP for now and try to grasp haskell and FP.

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
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

-- can write :info anyTypeClassHere
-- to see the type definition!
-- TODO(pierre): continue with yes-no typeclass.
