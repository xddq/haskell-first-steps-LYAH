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
-- returns a flattened concatenated result.
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

getFirstOver1554 =
  head $ dropWhile (\(price, year, month, day) -> price < 1555) stonks

-- span -> returns tuple of lists based on predicate and given list. Will
-- cut(inclusive) as soon as predicate returns false.
tuplesFromList = span (< 7) [4 .. 10]

-- break -> somewhat similar to span. breaks list in two tuples as soon as
-- predicate is true. span p === break (not . p)
tuplesFromList = break (not . (< 7)) [4 .. 10]

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
   in foldl
        (\acc x ->
           if take nlen x == needle
             then True
             else acc)
        False
        (tails haystack)
