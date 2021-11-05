-- src: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- MONOIDS

-- stuff usually gets thrown into behaviour categories in haskell. e.g. into Functor,
-- Applicative, Eq, Ord, etd..

-- MONOID LAWS:
-- - associative binary function (function where parenthesis don't matter. e.g.
-- +, *, ++; contra example: -)
-- - parameter of binary function and its return value has same type. (e.g. +,
-- ++, *)
-- - a value that acts as identity/id for the given function (e.g. 0 for +, ""
-- for ++, 1 for *)
-- definition:
class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

-- REMINDER: associativity --> (a + b) + c == a + b + c
-- commutativity --> a + b == b + a
-- - only associativity must hold for monoids.

-- --> only concrete types can be made an instance of monoid.
-- mempty --> returns the id/identity value of given monoid.
-- mappend --> binary function of monoid. does (in most cases) not really append
--      stuff. badly named. Think of it as function that takes two monoid values
--      and returns a monoid value. Nothing to do with mapping.
-- mconcat' --> takes a list of monoids and applies the binary function onto it
--      to return a result of type monoid.
-- mconcat can be overwritten but for most cases it suffices to take the default
-- implementation. (does get overwritten for performance benefits somethimes.
-- --> normally we onyl have to implement mappend and mempty)

-- can check with these rules (remember, only because someone makes it an
-- instance of monoid, does not mean it follows the monoid rules)
    -- mempty `mappend` x = x
    -- x `mappend` mempty = x
    -- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- NOTE: just using ' after all stuff to not overwrite the base behaviour and
-- avoid having to prefix stuff because of naming conflicts.
-- how Lists are a Monoid.
instance Monoid' [a] where
    mempty' = []
    mappend' = (++)
test1 = [1] `mappend'` [2,3]
test2 :: [a]
test2 = mempty'
test3 = "hello" `mappend'` " world"
test4 = mconcat' ["hello ", "how ", "are ", "you ", "doing?"]

-- how Num is a monoid when using Product (could also use Sum).
newtype Product' a =  Product' { getProduct' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
    mempty' = Product' 1
    Product' x `mappend'` Product' y = Product' (x * y)
test5 = getProduct' $ (Product' 3) `mappend'` (Product' 5)
-- "for now this seems insane, but a little later will make sense." I am curious
-- how this will end up.

-- how Bool is a monoid when using Any (could also use All).
newtype Any' = Any' { getAny' :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any' where
    mempty' = Any' False
    Any' x `mappend'` Any' y = Any' (x || y)
test6 = getAny' $ (Any' False) `mappend'` (Any' True)
test7 = getAny' $ (Any' False) `mappend'` (Any' False)

-- how Ord is a monoid. TODO: WHUT?
instance Monoid' Ordering where
    mempty' = EQ
    LT `mappend'` _ = LT
    EQ `mappend'` y = y
    GT `mappend'` _ = GT
-- NOTE: read about LT, GT, EQ in book for reference/intuition.
-- src: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
test8 = LT `mappend` GT
test9 = GT `mappend` LT
test10 = mempty `mappend` LT
test11 = mempty `mappend` GT
-- compare length of strings and return GT or LT, if eq length compare if they are equal.
test12 :: String -> String -> Ordering
test12 x y = (length x `compare` length y) `mappend'` (x `compare` y)
test13 = test12 "circlee" "cicle"
test14 = test12 "ANTS" "ANTS"
test15 = test12 "ANTS" "ANTZ"
-- adds number of vowels as second most important criterion.
test16 :: String -> String -> Ordering
test16 x y = (length x `compare` length y) `mappend'`
             (numVowels x `compare` numVowels y) `mappend'`
                (x `compare` y)
    where numVowels = length . filter (`elem` "aeiou")
