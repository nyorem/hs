-- module : collections of functions, types, typeclasses...
-- all of we saw is in the Prelude module

-- loading an entire module : import
import Data.List
-- loading specific functions from a module : import - (fun1, fun2...)
import Data.List (sort)
-- loading all but some functions : import - hiding (fun1, fun2...)
import Data.List hiding (sort)
-- qualified imports : use functions of this module by prepending it by the name of the module
import qualified Data.Map
-- WARNING: rename modules when doing qualified imports
import qualified Data.Map as M

-- nub : remove duplicates
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- using ghci : :m + Data.List Data.Set to import modules
-- Data.List -> List.hs
-- Data.Char -> Char.hs
-- Data.Map -> Map.hs
-- Data.Set -> Set.hs
