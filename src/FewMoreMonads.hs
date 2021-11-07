-- src: http://learnyouahaskell.com/for-a-few-monads-more
-- remider:
-- - Maybe Monad --> adds context of possible failure
-- - List Monad --> adds context of non-deterministic
-- used for Sum type.

-- used for Writer Monad.
import Control.Monad.Writer
-- used for WriterT typedef
import Data.Functor.Identity
import Data.Monoid

-- Writer Monad --> values which have another value attached which will be
-- combined into one log.

-- checks if gang is big.
isBigGang' :: Int -> Bool
isBigGang' = (> 9)

-- checks if gang is big and we also want to notify/log result as string
isBigGang :: Int -> (Bool, String)
isBigGang x = ((< 9) x, "Compared gang size to 9.")

testGang1 = isBigGang 3

testGang2 = isBigGang 10

-- ensure that string is appended.
-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog (val, log) f = (newVal, log ++ msg)
  where
    (newVal, msg) = f val

-- test with (Int, String)
testApplyLog = applyLog (2, "hello") (\x -> (x + 2, " world"))

testApplyLog1 = (30, "A freaking platoon.") `applyLog` isBigGang

testApplyLog2 = (3, "Smallish gang.") `applyLog` isBigGang

-- test with (String, String)
testApplyLog3 = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))

-- applyLog to include lists of any type beside just chars.
applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' (val, log) f = (newVal, log ++ msg)
  where
    (newVal, msg) = f val

-- applyLog to include monoids.
-- TODO: why do we have to write m instead of m c? Monoid is also a box/context,
-- no?
-- applyLog'' :: (Monoid m) => (a, m c) -> (a -> (b, m c)) -> (b, m c)
applyLog'' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog'' (val, log) f = (newVal, log `mappend` msg)
  where
    (newVal, msg) = f val

-- "Because the accompanying value can now be any monoid value, we no longer have
-- to think of the tuple as a value and a log, but now we can think of it as a
-- value with an accompanying monoid value. For instance, we can have a tuple that
-- has an item name and an item price as the monoid value. We just use the Sum
-- newtype to make sure that the prices get added as we operate with the items.
-- Here's a function that adds drink to some cowboy food: "
type Food = String

type Drink = String

type Price = Sum Int

addDrink :: Food -> (Drink, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- NOTE: Sum newtype just makes sure we add stuff together using a monoid.
testAddDrink1 = ("I eat nuffin", Sum 0) `applyLog''` addDrink

-- beans cost 20x. cause we drink milk with it we get Sum 20 `mappend` Sum 25 =
-- Sum 45.
testAddDrink2 = ("beans", Sum 20) `applyLog''` addDrink `applyLog''` addDrink

-- --> so we can build a reader Monad which could log messages, add up costs or
-- do whatever the characteristic of the given Monoid is.

-- WRITER Monad
-- we have just seen what a value with an attached Monoid value can be used for.
-- Now lets explore what can happen with a value with an attached Monad.
-- first step. wrap tuple in newtype so it can easily be made an instance of
-- Monad and is different from the default tuple type.
-- newtype Writer w a = Writer { runWriter :: (a, w) }
-- how Writer is a Monad:
-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)
--     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-- expect default identity values inside the second parameter of our resulting
-- writer.
-- id = 0
testWriter1 = runWriter (return 3 :: Writer String Int)

-- id = Sum 0
testWriter2 = runWriter (return 3 :: Writer (Sum Int) Int)

-- id = Product 1
testWriter3 = runWriter (return 3 :: Writer (Product Int) Int)

-- Writer with do notation.
-- TODO: why does this copied example not work? Did something in prelude /base
-- change?
-- looks like from mtl 1.X to mtl 2.X this changed. We have to create writer
-- using the writer function instead of the Writer newtype.
-- src: https://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell/11684566#11684566
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog' :: Writer [String] Int
multWithLog' = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

-- we can use some functions to just target the monoid value of our writer using
-- tell.
multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["We will multiply both values."]
  return (a * b)

-- adding logging to problems:
-- Euclidic algorithm to get the gcd="greatest common divisor"/ggt="größter
-- gemeinsamer Teiler"
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

-- add logging by using list of strings as our monoid.
-- NOTE: here again we had to adapt the type definition to match the mtl 2.X
gcd'' :: Int -> Int -> WriterT [String] Identity Int
gcd'' a b
  | b == 0 = do
    -- can use tell here because we are inside the Writer monad within the
    -- do block.
    tell ["Finished with: " ++ show b]
    return b
  | otherwise = do
    -- can use tell here because we are inside the Writer monad within the
    -- do block.
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd'' b (a `mod` b)

-- gcd'' with logging, using the writerT monad.
testGcd :: WriterT [String] Identity Int
testGcd = gcd'' 8 3

-- display the [String] monoid using mapM_ and putStrLn.
printResultsGcd :: IO ()
-- reminder: mapM_ takes function which takes a value and returns a
-- monad/context/boxed value. also takes a foldable/can be mapped over data
-- structure where it gets the value where it applies the first function on
-- from. Returns the resulting monad with ()/void/nothing.
printResultsGcd = mapM_ putStrLn $ snd $ runWriter testGcd

-- INEFFICIENT LIST CONSTRUCTION
-- reminder:
-- 1) xs ++ x goes through every element in xs and prepends them one by one to x.
-- 2) x ++ xs takes x and prepends it the first elem of xs.
-- examples:
-- in our gcd'' function it logs like this:
-- a ++ (b ++ (c ++ (d ++ (e ++ f))))
-- but if we changed the order of tell to be after the recursive call, we would
-- end up with something like (in our gcdReverse function)
-- ((((a ++ b) ++ c) ++ d) ++ e) ++ f
-- which is way, way worse / takes way longer to calculate.
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result


-- DIFFERENCE LISTS
-- can be used instead of lists when we have repeated appending of lists.
-- - function that takes a list and prepends another list to it.
-- appending with function and using composition:
-- create infix append definition
f `append` g = \xs -> f (g xs)
-- --> no matter how often we append, we only use the small junk we are starting
-- with. And we don't have to iterate through given lists multiple times. Just
-- once.
diffListApproach1 = ("dog"++) `append` ("meat"++)
diffListApproach2 = ("dog"++) `append` ("meat"++) `append` ("hello"++)
diffListApproach3 = diffListApproach2 "world"

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

diffListappend :: DiffList a -> DiffList a -> DiffList a
(DiffList f) `diffListappend` (DiffList g) = (DiffList (\xs -> f (g xs)))

-- NOTE: have to write it like this because the mappend moved from the Monoid
-- typeclass to the semigroup. (<>) and mappend seem to the same operation.
-- https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
-- TODO: what is a semigroup? How/When to use it?
instance Semigroup (DiffList a) where
    (<>) = (diffListappend)

instance Monoid (DiffList a) where
    mempty = toDiffList []
    -- mappend = (diffListappend)

-- <> is the same as `mappend`
testDiffList1 = fromDiffList $ toDiffList [1,2,3] <> toDiffList [4,5,6]
testDiffList2 = fromDiffList $ toDiffList [1,2,3] `mappend` toDiffList [4,5,6]

-- apply diffLists to gcdReverse to avoid the right associativity problem
-- (concat longer and longer lists with small lists multiple times.) see above
-- when gcdReverse was defined.
gcdReverseDiffList :: Int -> Int -> Writer (DiffList String) Int
gcdReverseDiffList a b
    | b == 0 = do
        tell $ toDiffList ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverseDiffList b (a `mod` b)
        tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

testGcdReverseDiffList = mapM_ putStrLn . fromDiffList . snd . runWriter
                            $ gcdReverseDiffList 110 34

-- comparing performance between diffList (fixes right associativity problem)
-- and a right associatively built/constructed list.
-- Counts from 100 to 0 but tells/adds/concats/ log in reverse order. (resulting
-- in right assoc list concat) see info above at gcdReverse.
countdownRightAssoc :: Int -> Writer [String] Int
countdownRightAssoc x
    | x == 0 = do
        tell ["Finished!"]
        return x
    | otherwise = do
        result <- countdownRightAssoc (x-1)
        tell ["At " ++ show x]
        return result

countdownRightAssocDiffList :: Int -> Writer (DiffList String) Int
countdownRightAssocDiffList x
    | x == 0 = do
        tell $ toDiffList ["Finished!"]
        return x
    | otherwise = do
        result <- countdownRightAssocDiffList (x-1)
        tell $ toDiffList ["At " ++ show x]
        return result

-- SOLUTION: (shorter, better since we are just using tell. and not retuning
-- values)
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

-- NOTE: if you really don't see a difference in speed just increase the value
-- given here.
testCountdownRightAssoc = mapM_ putStrLn . snd . runWriter $ countdownRightAssoc 5000
testCountdownRightAssocDiffList = mapM_ putStrLn . fromDiffList . snd . runWriter $ countdownRightAssocDiffList 5000


-- READER Monad
-- --> about combining functions with Monads.
-- reminder:
-- results in (x+3) * 5
-- same as (*5) . (+3)
reminderFunctorFuncComp1 x = fmap (*5) (+3) x
reminderFunctorFuncComp2 x = (*5) . (+3) $ x
-- results in \x -> (x*2) + (x+5)
reminderFunctorFuncComp3 x = (+) <$> (*2) <*> (+5) $ x

-- how functions are Monads
-- instance Monad ((->) r) where
--    return a = \_ -> a
--    h >>= f = \w -> f (h w) w
-- TODO: try to get this. Start with examples first for now.
-- "The implementation for >>= seems a bit cryptic, but it's really not all that.
-- When we use >>= to feed a monadic value to a function, the result is always a
-- monadic value. So in this case, when we feed a function to another function, the
-- result is a function as well. That's why the result starts off as a lambda. All
-- of the implementations of >>= so far always somehow isolated the result from the
-- monadic value and then applied the function f to that result. The same thing
-- happens here. To get the result from a function, we have to apply it to
-- something, which is why we do (h w) here to get the result from the function and
-- then we apply f to that. f returns a monadic value, which is a function in our
-- case, so we apply it to w as well. "

-- can use do expression for Int -> Int since it equal to (Int -> Int) and
-- functions are Monads.
-- addStuff :: Int -> Int
-- clearer for me with this notation.
-- TODO: continue here!
addStuff :: (Int -> Int)
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)
