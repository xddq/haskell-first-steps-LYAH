-- src: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- NEWTYPE
-- - used for making types out of existing types. BUT WHY ARE WE DOING THIS?


-- used for intersperse
import Data.List

-- what is the difference between
data ZipList' a = ZipList' {getZipList' :: [a]}
-- and
newtype ZipList'' a = ZipList'' {getZipList'' :: [a]}
-- newtype is faster (since it has less overhead) BUT we can only make newtype
-- for types that take only one value constructor. and that value constructor
-- may only take ony parameter.
-- these work only with data keyword.
data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

-- works with newtype. --> everytime we wrap stuff of another type and perhaps
-- give our wrapper new behaviour (e.g. ziplist does not apply all functions to
-- all values in lists, rather one to one)
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
test1 = CharList "this will be shown"
test2 = CharList "this will be shown" == CharList "test"
test3 = CharList "this will be shown" == CharList "this will be shown"

-- often we want to give our types certain behaviour. This is done by making
-- them instances of certain typeclasses. e.g. Functor f. But therefore we need
-- types which take one parameter. To achieve this we can use newtype to create
-- wrapper which take one parameter and then implement behaviour on our wrapper
-- type.
-- reminder: Functor with Maybe.
class Functor' f where
    fmap :: (a -> b) -> f a -> f b
instance Functor' Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)
test4 = Main.fmap (+3) (Just 3)

-- TODO: how would we implement Functor for tuples? When we apply function to
-- tuple we want to apply it to the first value of the tuple.
newtype TupleWrapper a b = TupleWrapper { getTuple :: (b, a)}
instance Functor' (TupleWrapper a) where
    fmap f (TupleWrapper (x,y)) = TupleWrapper (f x, y)
test5 = getTuple $ Main.fmap (*3) (TupleWrapper (10,"hello"))
test6 = getTuple $ Main.fmap ((intersperse '-') . reverse) (TupleWrapper ("HELLO",1))

-- NEWTYPE LAZYNESS
-- - since we are only wrapping an already existing type it will only be
-- evaluated once we actually use it (and then use the existing type to build of
-- of)
data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool' = CoolBool' { getCoolBool' :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"
test7 = helloMe undefined
test8 = helloMe' undefined
-- --> throws error for data definition, since haskell thinks it has to pattern
-- match and evaluated undefined first.
-- --> does not throw an error for newtype since haskell knows knewtype only has
-- one param and this is pattern matched against already.

-- TYPE vs NEWTYPE vs DATA
-- - TYPE --> type alias. e.g. type String = [Char]
-- - NEWTYPE --> create new type which probably will be made an instance of
-- functor. New type can onyl have one value constructor which also does only
-- take one value
-- - DATA --> create new type. no semantic constraints.
--
--
-- CONTINUE with Monoids.hs
