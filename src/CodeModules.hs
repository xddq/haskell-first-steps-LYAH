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
