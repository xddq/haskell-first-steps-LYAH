-- src: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-- MONOIDS
import qualified Data.Foldable as F
import Data.Monoid

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
-- NOTE: this is the old Monoid. Only works because this references our own
-- implementation of Monoid'. See "FewMoreMonads.hs" for more info when using
-- Semigroup.
instance Monoid' [a] where
  mempty' = []
  mappend' = (++)

test1 = [1] `mappend'` [2, 3]

test2 :: [a]
test2 = mempty'

test3 = "hello" `mappend'` " world"

test4 = mconcat' ["hello ", "how ", "are ", "you ", "doing?"]

-- how Num is a monoid when using Product (could also use Sum).
newtype Product' a = Product' {getProduct' :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
  mempty' = Product' 1
  Product' x `mappend'` Product' y = Product' (x * y)

test5 = getProduct' $ (Product' 3) `mappend'` (Product' 5)

-- "for now this seems insane, but a little later will make sense." I am curious
-- how this will end up.

-- how Bool is a monoid when using Any (could also use All).
newtype Any' = Any' {getAny' :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any' where
  mempty' = Any' False
  Any' x `mappend'` Any' y = Any' (x || y)

test6 = getAny' $ Any' False `mappend'` Any' True

test7 = getAny' $ Any' False `mappend'` Any' False

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
test16 x y =
  (length x `compare` length y)
    `mappend'` (numVowels x `compare` numVowels y)
    `mappend'` (x `compare` y)
  where
    numVowels = length . filter (`elem` "aeiou")

-- --> Ordering monoid allows us to compare different criteria based on
-- different priorities. highest first.

-- how Maybe is a Monoid.
-- 1) one way is to treat the value which is boxed by maybe as a monoid and then
-- apply mappend to this value.
instance (Monoid' a) => Monoid' (Maybe a) where
  mempty' = Nothing
  (Just x) `mappend'` (Just y) = Just (x `mappend'` y)
  Nothing `mappend'` m = m
  m `mappend'` Nothing = m

test17 = Nothing `mappend'` Just "hello"

test18 = Just "hello" `mappend'` Just " world"

-- --> Comes in handy when we are using monoids for computations that may have
-- failed. We can just operate over them and dont have to check if they have
-- failed or not.

-- how Maybe is a Monoid
-- 2) if we don't expect the parameter to be an instance of Monoid itself.
-- --> just use the first value if it  is Just x, else use the second.
-- NOTE: normally available from Data.Monoid.
newtype First' a = First' {getFirst' :: Maybe a}
  deriving (Eq, Ord, Read, Show)

-- instance Monoid' (First' a) where ---wrong. Forgot to wrap in First'.
--     mempty' = Nothing
--     (Just x) `mappend'` (Just y) = Just x
--     Nothing `mappend'` x = x
--     x `mappend'` Nothing = x
instance Monoid' (First' a) where
  mempty' = First' Nothing
  First' (Just x) `mappend'` _ = First' (Just x)
  -- x here basicly stands for Just x
  First' Nothing `mappend'` x = x

test19 = getFirst' $ First' (Just 'a') `mappend'` First' (Just 'b')

test20 = getFirst' $ First' Nothing `mappend'` First' (Just 'b')

-- The First' Monoid is useful when we have a bunch of maybes and want to find
-- out if there is a value that is a Just.
test21 = getFirst' $ mconcat' [First' Nothing, First' Nothing, First' (Just 3), First' (Just 4)]

-- if we want to always pick the second value if we have two just values for
-- this kind of Maybe Monoid implementation, there is a Last a type available.
-- Skipping because it is basicly the same as First' a, but `mappend'` picks the
-- second First'.
-- NOTE: available from Data.Monoid.

-- using Monoids to FOLD data structures.
-- so far we have mostly folded over lists. BUT we can fold over almost any data
-- structure. Trees somehow are especially good for folding?
-- --> because we have so many, the Foldable type class was introduced!
-- reminder: Functor --> stuff that can be mapped over.
-- Foldable --> stuff that can be folded up.
-- NOTE: found in Data.Foldable.
-- difference between prelude foldr and Data.Foldable.foldr?
-- prelude only folds up lists, foldable foldr folds up any foldable data
-- structure.
-- import qualified Foldable as F
-- ghci> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- ghci> :t F.foldr
-- F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

-- folding over a Maybe.
test22 = F.foldr (||) False (Just True)

-- TODO: why do we need the parenthesis around -1? Would it else be interpreted
-- as a function that is partially applied with 1?
test23 = F.foldr (+) (-1) (Just 4)

-- --> pretty boring since it behaves basicly like a list with one entry.

-- folding over custom data structures. E.g. our BinaryTree from the
-- RecursiveDataStructures.hs file.
data BinaryTree a
  = EmptyTree
  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show, Read, Eq)
-- when making something an instance of foldable we can either: implement foldr
-- for the instance OR implement foldMap for the instance.
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- by implementing foldMap for some type we get foldr and foldl implementations
-- for free.
-- TODO: how does this work without f x resulting in a monoid? We know F.foldmap
-- f l results in one, and F.foldMap f r results in one.. but f x?
-- --> we also know it about f x since foldmap takes a function that takes a
-- value of any type and returns a monoid/box with that value inside.
instance F.Foldable BinaryTree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                                                  f x `mappend`
                                                  F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )
test24 = foldl (+) 0 testTree
test25 = foldl (*) 1 testTree
test26 = foldl (\acc x -> if x < 10 then acc + x else acc) 0 testTree

-- NOTE: using the real Any for this one. Since the own implementation did throw
-- an error?
-- TODO: why did we get an error with Any' ??
-- checking if any value in our tree has value 6
test27 = getAny $ F.foldMap (\x -> Any $ x == 6) testTree
-- checking if any value in our tree is > 10
test28 = getAny $ F.foldMap (\x -> Any $ x > 10) testTree
-- convert tree to list
test29 = F.foldMap (\x -> [x]) testTree
-- all of this does not only work on BinaryTree.. it works on EVERY INSTANCE of
-- Foldable <3
--
-- CONTINUE AT Monads.hs
