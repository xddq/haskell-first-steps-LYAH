-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses starting at
-- record syntax block.
-- Can define value constructor in a somewhat json like format. Called Record
-- Syntax.
import qualified Data.Map as Map
import Data.Maybe as Maybe

data PersonRecord = PersonRecord
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

-- normal notation of value constructor
data PersonNormal
  = PersonNormal String String Int Float String String
  deriving (Show)

-- When to use record syntax and when to use the normal value constructor?
-- - has multiple fields and it is unclear where which field belongs?
-- -> record
-- - want to query specific fields by name?
-- -> record
-- - obvious what the fields are for?
-- -> normal
-- check differences in print.
showPersonNormalSyntax =
  PersonNormal "max" "test" 20 190.0 "1234567" "strawberry"

showPersonRecordSyntax =
  PersonRecord "max" "test" 20 190.0 "1234567" "strawberry"

-- only possible for record syntax (otherwise need to define that function
-- first)
recordFlavor = flavor showPersonRecordSyntax

-- fields are obvious or it is unnecessary to know which is which.
data Rectangle
  = Rectangle Float Float

-- TODO(pierre): Postpone until type and data was explained. Can't figure it out
-- now.
-- type Person = PersonNormal | PersonRecord
-- displayDifference :: String
-- person :: Person -> Person String String Int Float String String
-- displayDifference = person PersonNormal where person x = x "Max" "Xddq" 20 190.2 "1234567" "strawberry"
-- TYPE PARAMETERS
-- -> Parameters for value constructors that yield to multiple different
-- possible implementations. Somewhat similar to templates in c++.
-- ghci examples:

-- * Main> data Test a = Nothing | Just a deriving (Show)

-- * Main> :t Just "hello"

-- Just "hello" :: Test [Char]

-- * Main> Just 30 :: Test Double

-- Just 30.0
-- when to use type parameters? -> only if it doest matter what the
-- parameterized types of data will be. E.g. Maybe a -> Type does not Matter.
-- Maybe is still a Box around Type.
-- E.g. Person a b = Person {name:: a, lastName:: b, ...} -> Name will be String
-- anyway.
-- another example:
-- Map k v -> it does not matter what types key or value are.
-- NOTE: Never add typeclass constraints to value constructors. Will result in
-- constraining the functionality of our value. e.g. Map k v would take Ord
-- typeclass. But then toList function that maps Map to List would require Ord
-- type, even though it does not matter if it is ordered or not.
-- create vector with parameterized value constructor.
-- allows for Int Int Int or Float Float Float
-- NOTE: what is Integer when we have type Int?
data Vector a
  = Vector a a a
  deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
-- infix definition
Vector x y z `vplus` Vector j k l = Vector (x + j) (y + k) (z + l)

vmult :: (Num a) => Vector a -> Vector a -> Vector a
Vector x y z `vmult` Vector j k l = Vector (x * j) (y * k) (z * l)

-- OKAY. everything left from = is the type constructor. Everything right from =
-- is the value constructor.
-- data Vector a = -> type constructor
-- Vector a a -> value constructor
vector1 = Vector 1 2 3

vector2 = Vector 4 5 6

addVectors = vector1 `vplus` vector2

multVectors = vector1 `vmult` vector2

-- Typeclasses are more like interfaces (but they actually get applied and don't
-- require additional implementation).
-- e.g.
-- data Vector a = Vector a a a deriving (Show)
-- will now be able to be showed. ghci> show $ Vector 4 10 18
-- Type classes in action. especially Eq, Ord, Enum, Bounded, Show, Read.
-- we can't compare instances of this type.
-- TODO(pierre): Why can't we define same name of record fields for multiple
-- different data?
-- src: https://stackoverflow.com/questions/24352280/multiple-declarations-of-xo
-- NOTE: can use DuplicateRecordFields language extension!
data PersonNoTypeclassInstance = PersonNoTypeclassInstance
  { nfirstName :: String,
    nlastName :: String
  }

-- now we can compare instances of this type. :] And get true or false :]
data PersonTypeclassInstance = PersonTypeclassInstance
  { ifirstName :: String,
    ilastName :: String
  }
  deriving (Eq)

-- show -> type will be able to create a string.
-- read --> type will be able to be created from a string. NOTE: read needs to
-- be given a type. Otherwise it won't be able to know what it is reading and
-- how to convert it! :]
-- example ghci

-- * Main> data PersonX = PersonX { firstName :: String , lastName :: String , age :: Int } deriving (Eq, Show, Read)

-- * Main> let mikeD = PersonX {firstName = "Michael", lastName = "Diamond", age = 43}

-- * Main> read "PersonX {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" :: Person

-- <interactive>:36:1: error:
--     • No instance for (Read Person) arising from a use of ‘read’
--     • In the expression:
--           read
--             "PersonX {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" ::
--             Person
--       In an equation for ‘it’:
--           it
--             = read
--                 "PersonX {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" ::
--                 Person

-- * Main> read "PersonX {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" :: PersonX

-- PersonX {firstName = "Michael", lastName = "Diamond", age = 43}
-- haskell can also infer type if we use it accordingly later on:
--     ghci> read "PersonX {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
-- True
-- ORD typeclass
-- when using ord we can specify the ordering of our data based on the given
-- order on definition.
-- like this testBool will be True
-- data BoolX = False | True deriving (Eq, Ord)
-- like this testBool will be False :]
data BoolY
  = True
  | False
  deriving (Eq, Ord)

testBool = Main.False < Main.True

-- ENUM typeclass
-- used for stuff that has predecessors and successors. Can only be applied when
-- all Constructors are nullary (take no parameters)
-- example:
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- day after Monday?
afterMonday = succ Monday

-- day before Saturday?
beforeSaturday = pred Saturday

-- list ranges <3
days = [Monday .. Sunday]

weekend = [Friday .. Sunday]

-- BOUNDED typeclass
-- used to get the lowest and highest possible value for our typed data.
-- NOTE: haskell does infer type for minBound by type definition.
-- minimal Day?
minDay :: Day
minDay = minBound

-- maximum Day?
maxDay :: Day
maxDay = maxBound

-- TYPE SYNONYMS
-- giving types another name to make more sense to other people. E.g.
-- implementation of String type synonym:
-- type String = [Char]
-- e.g. make our code more readable.
phoneBookNoSynonym :: [(String, String)]
phoneBookNoSynonym =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- -> type would be [(String, String)] .. mhhh
-- create more verbose alias for String type
type PhoneNumber = String

-- create more verbose alias for String type
type Name = String

type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- now :info PhoneBook results in
-- ghci> :info PhoneBook
-- type PhoneBook :: *
-- type PhoneBook = [(Name, PhoneNumber)]
--   	-- Defined at src/RecordSyntax.hs:207:1
-- ghci> :info Name
-- type Name :: *
-- type Name = String
--   	-- Defined at src/RecordSyntax.hs:206:1
placeholder = show 1

-- check if user is in phonebook by given name and number
inPhoneBook :: Name -> PhoneNumber -> Bool
inPhoneBook name phoneNumber = not . null $ matches
  where
    matches =
      takeWhile
        (\tuple -> fst tuple == name && snd tuple == phoneNumber)
        phoneBook

-- XD could just use elem
-- solution:
inPhoneBook' :: Name -> PhoneNumber -> Bool
inPhoneBook' name phoneNumber = (name, phoneNumber) `elem` phoneBook

-- We can also parameterize type synonyms. E.g. List of tuples with type k and v
-- will be called AssocList :)
type AssocList k v = [(k, v)]

-- example: takes an assoc list of any type and returns a list of its values.
assocListValues :: AssocList k v -> [v]
assocListValues [] = []
assocListValues [(k, v)] = [v]
assocListValues ((k, v) : xs) = v : assocListValues xs

-- concrete type -> Type constructor with (0-n) parameters
-- example: map that maps integers to something
type IntMap a = Map.Map Int a

-- can also write partially applied, since Data.Map takes two parameters (check
-- with :info Data.Map.Map).
-- type IntMap = Map.Map Int
-- EITHER
-- takes two types as parameters. Convention: On failure, Left a is returned.
-- On success, Right b is returned.
data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Read, Show)

-- takes a number. Returns the number if it was even. Else returns an error
-- string.
-- TODO(pierre): Figure out difference between Integral, Int, Integer.
-- Why did this code not work with (Num a, Show a)?
-- Note we can print the result because Left and Right are deriving Show :]
getEven :: (Integral a, Show a) => a -> Main.Either a String
getEven number =
  if even number
    then Main.Left number
    else Main.Right $ show number ++ " was not an even number!"

-- When to use Maybe a and when to use Either a b?
-- - Do we have one fail case where result does not matter and multiple success
-- results? (or vice versa)
-- -> Maybe a since Nothing is our fail case and Maybe a will display the value
-- of the success case.
-- - Do we have multiple fail and a success case where we need a value? (or vice
-- versa)
-- -> Either a b since we need to display which success/fail result we got.

-- EXAMPLE:
-- multiple fail and a success case. e.g. buy icecream and ask friend to pick
-- flavor for you. fail cases: no friend, flavor not existant. success case:
-- your rank of the flavor that was picked by friend.
type Friend = String

flavours = ["vanilla", "chocolate", "strawberry"]

available :: [Char] -> Bool
available [] = Prelude.False
available _ = Prelude.True

-- was to stupid to generate random number. somehow need IO for this, which
-- comes later in this book anyway. so just pick 3.
tasteRanking = 3

-- NOTE: did not know how to include two fail cases here. something like:
-- if a then (if b then c else d) else e
orderIcecream :: Maybe Friend -> Prelude.Either String Int
orderIcecream name =
  if Maybe.isNothing name
    then
      Prelude.Left $
        Maybe.fromJust name
          ++ " did not bring ice cream because flavour was not available."
    else Prelude.Right tasteRanking
  where
    friendsPick = flavours !! magicNumber
    magicNumber = length name `mod` 4

-- book example:
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

-- NOTE: we have to use Prelude.Either, Prelude.Left etc.. because we defined
-- these types and values in our own module as well. Normally we could just use
-- Either or Left or Right etc..!
lockerLookup :: Int -> LockerMap -> Prelude.Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Prelude.Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Prelude.Right code
        else Prelude.Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- Continue at file: RecursiveDataStructures.hs
