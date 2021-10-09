-- starting from: http://learnyouahaskell.com/modules#loading-modules
-- modules: logical grouping of functions, types and typeclasses.
-- imports all functions for Data.List, Data.map and Data.Set
-- ghci> :m + Data.List Data.Map Data.Set
-- selectively import functions: (only imports nub and sort for data.list)
import Data.List (nub, sort)

-- importing all BUT specific ones:
import Data.List hiding (nub)

-- to avoid name clashes qualified/full imports are used.
-- import qualified Data.Map
-- since Data.Map prefix before every function is quite big, we can alias the
-- full import. And then write M.funName instead.
import qualified Data.Map as M

-- put elem between each elem in list with intersperse
putA = intersperse "A" ["test1", "test2", "test3", "test4"]

putComma = intersperse ',' "abcdef"

-- takes list and list of lists. intersperses each list in a given list and
-- Value
putFlat = intercalate [9, 9, 9] [[1], [1 .. 5], [1 .. 10]]

-- transpose. iterates through a list of lists and always picks n-th value from
-- each list to create a new list. can e.g. do 2d matrix with vector of rows and
-- columns and create points.
rows = [1, 2, 3, 4]

cols = [5, 5, 6, 6]

create2D = transpose [rows, cols]

-- x2 -> x² x3 -> x³ etc.
-- Say we have the polynomials 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1 and
-- we want to add them together
addPolynomials =
  map sum $ transpose [[9, 5, 3, 0], [9, 0, 0, 10], [-1, 1, 5, 8]]

-- note: when getting stackoverflow with foldx we can use foldx' which does not
-- use lazy evaluation. Reason:
-- "foldl' and foldl1' are stricter versions of their respective lazy
-- incarnations.  When using lazy folds on really big lists, you might often get
-- a stack overflow error. The culprit for that is that due to the lazy nature
-- of the folds, the accumulator value isn't actually updated as the folding
-- happens. What actually happens is that the accumulator kind of makes a
-- promise that it will compute its value when asked to actually produce the
-- result (also called a thunk). That happens for every intermediate accumulator
-- and all those thunks overflow your stack. The strict folds aren't lazy
-- buggers and actually compute the intermediate values as they go along instead
-- of filling up your stack with thunks. So if you ever get stack overflow
-- errors when doing lazy folds, try switching to their strict versions."
concatDoesFlat = concat [[1, 2], [3, 4], [5, 6]]

-- concatMap does map container with function and concats (and therefore also
-- flats) the result
concatMapTest = concatMap (\x -> [x] ++ [1, 2]) [1 .. 4]

concatMapTest' = concatMap (replicate 4) [1 .. 3]

-- and -> true if all elem in container are true
andTest = and $ map (< 4) [1 .. 3]

andTest' = and $ map (< 4) [1 .. 4]

-- or -> true if one elem in container is true
orTest = or $ map (== 4) [1 .. 5]

-- any and all take a predicate and check if >=1(any) or every(all) element
-- return true for that predicate. Is normally used instead of combinating
-- and/or and map (predicate) container
allTest = all (< 4) [1 .. 3]

allTest' = all (< 4) [1 .. 4]

anyTest = any (== 4) [1 .. 5]

-- iterate -> takes function and starting value. In each step the result is
-- stored in a list and used as starting value to calculate the next result.
-- Will return infinite(lazy) list of values for each step.
iterateTest = take 5 $ iterate (+ 1) 0

-- splitAt -> takes position and list and returns tuple containing the
-- left(inclusive cut position) and right side of the splitat  position.
-- starts counting at 1
splitAtTest = splitAt 2 [1, 2, 3, 4, 5]

-- takeWhile -> returns list with values of given list that satisfy(return true)
-- the given predicate.  stops at first "false"
-- takes first 4 values that are above 25 from a given list. then increments
-- each value by one and calculates the sum of these values.
takeWhileTest = sum . map (+ 1) . take 4 $ takeWhile (> 25) [40,39 .. 1]

-- dropWhile -> drops first X values from a list. Drops value if given predicate
-- does return false. Stops dropping as soon as first predicate application
-- returns true.
dropWhileTest = sum . map (+ 1) . take 5 $ dropWhile (> 25) [40,39 .. 1]

dropFirstWord = tail $ dropWhile (/= ' ') "This is a test"

-- We're given a list that represents the value of a stock by date. The list is
-- made of tuples whose first component is the stock value, the second is the
-- year, the third is the month and the fourth is the date(I guess day is
-- meant?). We want to know when the stock value first exceeded one thousand
-- dollars!  NOTE: tuples can have more than two values. Assumes order by date.
stonks = [(1227, 2014, 3, 1), (1229, 2015, 2, 2), (1555, 2016, 4, 1)]

-- NOTE: head is not safe. could use find instead an operate on Maybe and Just
-- Value
getFirstOver1554 =
  head $ dropWhile (\(price, year, month, day) -> price < 1555) stonks

getFirstOver1554' = find (\(price, year, month, day) -> price < 1555) stonks

-- span -> returns tuple of lists based on predicate and given list. Will
-- cut(inclusive) as soon as predicate returns false.
tuplesFromList = span (< 7) [4 .. 10]

-- break -> somewhat similar to span. breaks list in two tuples as soon as
-- predicate is true. span p === break (not . p)
tuplesFromList' = break (not . (< 7)) [4 .. 10]

-- sort -> sorts list. requires element to be of Ord typeclass (todo: check Ord
-- typeclass definition?). Basicly requires element to be able to be ordered.
-- NOTE: sorts asc.
sortList = sort [5,4 .. 1]

-- group -> takes list and returns list of list. Takes current value and puts it
-- in a list. Takes next value and puts it in the same list if it is equal to
-- current value, else to next list.
groupList = group [1, 1, 1, 1, 1, 2, 2, 2, 3, 4, 5, 6, 6]

-- this does not return the same result :]
groupListNonNeighbour = group [1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]

-- inits -> creates list of lists starting from the smalles sublist (empty list)
-- of given list and ending at the full list as sublist.
initsTest = inits [1 .. 4]

-- tails -> opposite of inits. takes largest sublist and ends as smalles sublist
-- which is the empty list.
tailsTest = tails [1 .. 4]

-- Let's use a fold to implement searching a list for a sublist.
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
   in foldl (\acc x -> (take nlen x == needle) || acc) False (tails haystack)

-- this is equal to the isInfixOf function
equalTest =
  (==) (search [1, 2] [3, 4, 5, 1, 2]) (isInfixOf [1, 2] [3, 4, 5, 1, 2])

-- isPrefixOf -> checks if first entry of list does match the given list.
isPrefixOfTest = isPrefixOf [1, 2] [1, 2, 3, 4]

-- isSuffixOf -> checks if last entry of list does match the given list.
isSuffixOfTest = isSuffixOf [3, 4] [1, 2, 3, 4] -- TODO(pierre): continue with book. don't go through all examples. Just try to

-- udnerstand the gist.
-- continue: http://learnyouahaskell.com/modules#loading-modules at elem and
-- notElem
-- find -> goes through list and returns a Maybe (is an ADT-> algebraic data
-- type), will learn more about it in next chapter. Maybe returns 'Just
-- Something' or 'Nothing'
findSomething = find (> 5) [1 .. 10]

findNothing = find (> 10) [1 .. 10]

-- elemIndex -> get first index out of given list that is equal to given elem.
-- elemIndices -> get all indices out of given list that are equal to given
-- elem.
-- zip and zipWith:
-- "We already covered zip and zipWith. We noted that they zip together two
-- lists, either in a tuple or with a binary function (meaning such a function
-- that takes two parameters). But what if we want to zip together three lists?
-- Or zip three lists with a function that takes three parameters? Well, for
-- that, we have zip3, zip4, etc. and zipWith3, zipWith4, etc. These variants go
-- up to 7. While this may look like a hack, it works out pretty fine, because
-- there aren't many times when you want to zip 8 lists together. There's also a
-- very clever way for zipping infinite numbers of lists, but we're not advanced
-- enough to cover that just yet."
-- lines -> takes string and returns a list that contains them line by line.
-- useful for reading files or any input.
getLines = lines "hello\nstring\nworld in haskell"

-- unlines -> takes lists and joins them together with appending \n after each
-- elem
makeText = unlines getLines

-- words -> split lines of text into elements, separated by ' '.
makeWords = words ["hello how are you doing?"]

-- unwords -> joint elements of list into a string, separated by ' '.
unWords = unwords makeWords

-- nub -> takes list and returns list with only unique items (basicly set)
makeUnique = nub [1, 2, 3, 1, 34, 2, 3, 11, 1, 2, 3]

-- union -> like union on sets, but applied on lists.
makeUnion = union [1 .. 5] [1 .. 10]

-- insert -> inserts element into list. elem will be at first occurence where it
-- is <= x. Using insert on sorted list will end up in sorted list.
makeInsert = insert 3 [1, 2, 3, 4, 5, 6, 7]

makeInsert' = insert 3 [5, 3, 5, 1, 2, 3, 4]

-- NOTE(pierre): Don't use take, drop, length, replicate, splitAt and !!
-- operator! Use genericTake, genericDrop, genericLength, genericReplicate etc..
-- ! For historical reasons it returns Int instead of Num.  "That's why
-- Data.List has their more generic equivalents, named genericLength,
-- genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate.
-- For instance, length has a type signature of length :: [a] -> Int. If we try
-- to get the average of a list of numbers by doing let xs = [1..6] in sum xs /
-- length xs, we get a type error, because you can't use / with an Int.
-- genericLength, on the other hand, has a type signature of genericLength ::
-- (Num a) => [b] -> a. Because a Num can act like a floating point number,
-- getting the average by doing let xs = [1..6] in sum xs / genericLength xs
-- works out just fine."
placeHolder' = show 3

-- we can replace nub, delete, union, intersect and group functions with xBy,
-- yBy, etc.. The difference is that we can pass our own binary equality
-- function to the xBy functions. Therefore nub xs === nubBy (==) xs etc.
nubNoBy = nub [1, 1, 2, 3, 4]

nubWithBy = nubBy (==) [1, 1, 2, 3, 4]

nubDifferentEq = nubBy (\x y -> (x > 1) == (y > 1)) [1, 2, 3, 4]

-- "For instance, say we have a list that describes the value of a function for
-- every second. We want to segment it into sublists based on when the value was
-- below zero and when it went above. If we just did a normal group, it would
-- just group the equal adjacent values together. But what we want is to group
-- them by whether they are negative or not. That's where groupBy comes in! The
-- equality function supplied to the By functions should take two elements of
-- the same type and return True if it considers them equal by its standards."
groupByDifferentEq =
  let values =
        [ -4.3
        , -2.4
        , -1.2
        , 0.4
        , 2.3
        , 5.9
        , 10.5
        , 29.1
        , 5.3
        , -2.4
        , -14.5
        , 2.9
        , 2.3
        ]
   in groupBy (\x y -> (x > 0) == (y > 0)) values

-- normally the 'on' function is used in infix notation.
-- definition of 'on' in Data.Function.on:
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

-- this enables us to write the groupByDifferentEq in a more readable way.
-- reads: groupBy on equality where x > 0 is equal to y > 0.
groupByDifferentEq' =
  let values =
        [ -4.3
        , -2.4
        , -1.2
        , 0.4
        , 2.3
        , 5.9
        , 10.5
        , 29.1
        , 5.3
        , -2.4
        , -14.5
        , 2.9
        , 2.3
        ]
   in groupBy ((==) `on` (> 0)) values

-- passing custom comparison functions is also available for sort, insert,
-- maximum, minimum -> sortBy, insertBy, maximumBy, minimumBy.
-- TODO(pierre). how to sortBy with given fn?
xs' = [[5, 4, 5, 4, 4], [1, 2, 3], [3, 5, 4, 3], [], [2], [2, 2]]

-- compare returns LT, EQ, or GT based on x and y. This makes sure we run
-- compare (length xs) (length ys) for sorting the given list. Sorts lt,eq,gt
-- order(asc).
sortByLengthAsc = sortBy (compare `on` length)

-- To sort desc we can use 'Down'. Down from Ord.Down: "If a has an Ord instance
-- associated with it then comparing two values thus wrapped will give you the
-- opposite of their normal sort order."
sortByLengthDesc = sortBy (compare `on` Down . length)

-- Data.Char -> Gives us stuff to operate on characters. Also useful for strings
-- since they are just lists of characters. Note: skip most, just check docs or
-- book if needed. http://learnyouahaskell.com/modules#loading-modules
-- (Data.Char)
-- checks whether all characters are /[0-9]|[a-z]|[A-Z]/
allAlphaNum = all isAlphaNum "behuwqheq1231"

words' = words "hey guys its me"

wordsWithIsSpace = groupBy ((==) `on` isSpace) "hey guys its me"

-- will return ["hey"," ","guys"," ","its"," ","me"]. To make it equal to the
-- output from words which is ["hey","guys","its","me"] we need to filter all
-- entries for which isSpace is true for all chars.
wordsWithIsSpace' = filter (\x -> not $ all isSpace x) wordsWithIsSpace

wordsWithIsSpace'' =
  filter (\x -> not $ all isSpace x) $
  groupBy ((==) `on` isSpace) "hey guys its me"

-- solution was this?
wordsWithIsSpace''' =
  filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"

-- can use point-free-style. src:
-- http://learnyouahaskell.com/higher-order-functions
-- basicly 'not . all isSpace' is equal to: not (all isSpace) $ x. But with the
-- point-free-style we use currying to drop the $ x in the declaration. It will
-- still be applied as if it was there (because isSpace takes another argument
-- that we don't provide)
-- the clean point-free-style would be: now we can call this fn with an arg and
-- get a result thats equal to 'words' arg
wordsWithIsSpacePointFree =
  filter (not . all isSpace) . groupBy ((==) `on` isSpace)

-- NOTE: in general if we prodive anonymous/lambda functions we can use the
-- point-free-style if we have only one argument and our function inside
-- operates on one argument. MAYBE(pierre): is it the same for 2 and 2, 3 and 3
-- etc? try create examples.
notPointFree = filter (\x -> x > 3) [1 .. 10]

pointFree = filter (> 3) [1 .. 10]

wordsWithIsSpaceAdapted =
  filter (not . all isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"

-- there are types for different chars. The Type of this enumeration is
-- GeneralCategory. There are 31 categories which can be used. e.g.
-- ghci> generalCategory ' '
-- Space
-- ghci> generalCategory 'A'
-- UppercaseLetter
-- ghci> generalCategory 'a'
-- LowercaseLetter
-- ghci> generalCategory '.'
-- OtherPunctuation
-- digitToInt -> convert char to int. allowed range is hex values.
-- intToDigit -> converts int to char. allowed range is 0..15 (hex values)
digitToInt' = digitToInt 'a'

intToDigit' = intToDigit 14
-- ord -> convert char to number based on ascii
-- chr -> number to char base don ascii
-- samples:
-- use ':m + Data.List Data.Function Data.Ord Data.Char' in ghci. only Data.Char
-- is required
-- here.
-- Prelude Data.List Data.Function Data.Ord Data.Char> ord '3'
-- 51
-- Prelude Data.List Data.Function Data.Ord Data.Char> ord 'a'
-- 97
-- Prelude Data.List Data.Function Data.Ord Data.Char> ord 'A'
-- 65
-- Prelude Data.List Data.Function Data.Ord Data.Char> ord 'Z'
-- 90
-- Prelude Data.List Data.Function Data.Ord Data.Char> chr 1
-- '\SOH'
-- Prelude Data.List Data.Function Data.Ord Data.Char> chr 90
-- 'Z'
-- Prelude Data.List Data.Function Data.Ord Data.Char> chr 66
-- 'B'
