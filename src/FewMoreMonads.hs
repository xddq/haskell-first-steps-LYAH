-- src: http://learnyouahaskell.com/for-a-few-monads-more
-- remider:
-- - Maybe Monad --> adds context of possible failure
-- - List Monad --> adds context of non-deterministic
-- used for Sum type.
import Data.Monoid
-- used for Writer Monad.
import Control.Monad.Writer
-- used for WriterT typedef
import Data.Functor.Identity

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
    where (newVal, msg) = f val
-- test with (Int, String)
testApplyLog = applyLog (2, "hello") (\x -> (x+2," world"))
testApplyLog1 = (30, "A freaking platoon.") `applyLog` isBigGang
testApplyLog2 = (3, "Smallish gang.") `applyLog` isBigGang
-- test with (String, String)
testApplyLog3 = ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
-- applyLog to include lists of any type beside just chars.
applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' (val, log) f = (newVal, log ++ msg)
    where (newVal, msg) = f val
-- applyLog to include monoids.
-- TODO: why do we have to write m instead of m c? Monoid is also a box/context,
-- no?
-- applyLog'' :: (Monoid m) => (a, m c) -> (a -> (b, m c)) -> (b, m c)
applyLog'' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog'' (val, log) f = (newVal, log `mappend` msg)
    where (newVal, msg) = f val

-- "Because the accompanying value can now be any monoid value, we no longer have
-- to think of the tuple as a value and a log, but now we can think of it as a
-- value with an accompanying monoid value. For instance, we can have a tuple that
-- has an item name and an item price as the monoid value. We just use the Sum
-- newtype to make sure that the prices get added as we operate with the items.
-- Here's a function that adds drink to some cowboy food: "
type Food = String
type Drink = String
type Price = Sum Int
addDrink :: Food -> (Drink,Price)
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
    return (a*b)

-- we can use some functions to just target the monoid value of our writer using
-- tell.
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["We will multiply both values."]
    return (a*b)

-- adding logging to problems:
-- Euclidic algorithm to get the gcd="greatest common divisor"/ggt="größter
-- gemeinsamer Teiler"
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0    = a
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
-- TODO: continue here.
