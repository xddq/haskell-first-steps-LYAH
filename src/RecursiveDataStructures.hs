-- define own list with ADT (NOTE: ADT="Algebraic Data Types" -> haskell name
-- for type definition)
-- data list' a :: a list'
-- either empty list or some value with a list of values of same type.
-- NOTE: using BinaryTree(..) exports all value constructors!
module RecursiveDataStructures
  ( insertInBinaryTree,
    insertNumbersIntoTree,
    genNumbers,
    createBaseTree,
    BinaryTree (..),
  )
where

data List' a = TestEmpty | Cons a (List' a) deriving (Show, Eq, Ord, Read)

-- cons is ':' in normal lists.
-- calling cons in infix to create a list'
testList' = 3 `Cons` TestEmpty

testLargeList' = 3 `Cons` (4 `Cons` (5 `Cons` TestEmpty))

-- "We can define functions to be automatically infix by making them comprised
-- of only special characters. We can also do the same with constructors, since
-- they're just functions that return a data type. So check this out."
infixr 5 :.:

data List a = Empty | a :.: (List a) deriving (Show, Read, Eq, Ord)

testLista = 2 :.: 3 :.: 4 :.: Empty

-- infixr val symbol -> means that symbol is right associative with value val.
-- infixl val symbol -> means that symbol is left associative with value val.
-- example: + is defined with fixity 6, infixr6.

-- * is defined with fixity 7. inifxr7.

-- Meaning that both are right associative -> a * b * c === a * (b * c)
-- Since * has higher right fixity -> a + b * c === a + (b * c)
-- BUT a * b + c !== a * (b + c)

-- concat two lists is defined as:
-- infixr 5  ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
-- custom concat two lists. uses our previous definitions :] (Empty, List, :.:,
-- etc.)
infixr 5 .++

(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :.: xs) .++ ys = x :.: (xs .++ ys)

-- TODO(pierre): understand this text..
-- NOTE: xs is List a x is x and :.: is :.. -> (x :.: xs) === a :.. (List a)
-- "Notice how we pattern matched on (x :-: xs). That works because pattern
-- matching is actually about matching constructors. We can match on :-: because
-- it is a constructor for our own list type and we can also match on : because
-- it is a constructor for the built-in list type. Same goes for []. Because
-- pattern matching works (only) on constructors, we can match for stuff like
-- that, normal prefix constructors or stuff like 8 or 'a', which are basically
-- constructors for the numeric and character types, respectively."

-- BINARY SEARCH TREE (BinaryTree)
-- LEFT FROM = -> type constructor -> cant be created as value
-- RIGHT FROM = -> value constructor -> can be created as value
-- -> There will never be BinaryTree 1 as value. Since BinaryTree 1 is either EmptyTree or
-- Node 1 (EmptyTree | Node val (EmptyTree | Node val ....))
data BinaryTree a
  = EmptyTree
  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show, Read, Eq)

-- helper function that determines whether the tree is empty.
isEmptyTree :: BinaryTree a -> Bool
isEmptyTree EmptyTree = True
isEmptyTree _ = False

-- helper function that creates a base tree(node empty empty) for given number.
createBaseTree :: a -> BinaryTree a
createBaseTree x = Node x EmptyTree EmptyTree

-- three cases: eq -> drop, bigger -> right, smaller -> left
-- Notation: first BinaryTree(a) is the left leaf and second BinaryTree(a) is the right leaf.
-- my solution:
-- NOTE: If we use compare it is exhaustive pattern matched. If we use > < ==,
-- this is not the case. E.g. for floats maybe? Idk?
-- src: https://github.com/haskell/error-messages/issues/20
-- src: https://www.reddit.com/r/haskell/comments/q8lomp/beginner_question_warnings_in_binary_tree/
insertInBinaryTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insertInBinaryTree EmptyTree input = createBaseTree input
insertInBinaryTree (Node x leftTree rightTree) input = case compare input x of
  EQ -> Node x leftTree rightTree
  LT -> Node x (insertInBinaryTree leftTree input) rightTree
  GT -> Node x leftTree (insertInBinaryTree rightTree input)

valueInTree :: (Ord a) => BinaryTree a -> a -> Bool
valueInTree EmptyTree _ = False
valueInTree (Node x leftTree rightTree) value = case compare value x of
  EQ -> True
  LT -> valueInTree leftTree value
  GT -> valueInTree rightTree value

-- generate some numbers to insert into tree
genNumbers = map (\x -> if even x then x + 10 else x) [1 .. 5]

-- insert all these numbers into BinaryTree with base 10
-- insertNumbersIntoTree =
--   foldl (\acc x -> insertInBinaryTree acc x) (createBaseTree 10) genNumbers
-- insertNumbersIntoTree =
--   foldl (\acc x -> insertInBinaryTree acc x) (createBaseTree 10) genNumbers
-- NOTE: since insertInBinaryTree is a function that takes two arguments (the tree and
-- then the input) we can directly invoke it with acc and x as params without
-- defining a separate lambda.
insertNumbersIntoTree =
  foldl insertInBinaryTree (createBaseTree 10) genNumbers

-- CONTINUE WITH Typeclasses 102 (Typeclasses.hs)
