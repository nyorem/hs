-- map : associative list with no orders and no duplicates
import qualified Data.Map as Map

-- without using a Data.Map
type PhoneBook = [(String, String)]
phoneBook :: PhoneBook
phoneBook =
    [("me", "0635509086")
    ,("you", "0488633666")
    ]

lookUp :: PhoneBook -> String -> Maybe String
lookUp phoneBook key =
    foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing phoneBook

-- with a Data.Map : tree representation
-- creation from an associative list by removing the duplicates keys
m = Map.fromList phoneBook

-- empty : create an empty map
-- insert : return a new map by inserting a (key, value) pair
n = Map.empty
nn = Map.insert "me" "0635509086" n

-- own fromList
fromList' :: (Ord k) => [(k, a)] -> Map.Map k a
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

-- null : check if a map is empty
-- size : size of a map
s = Map.size nn

-- singleton : create a map with one mapping
-- lookup : lookup function
-- member : check if a key is a member of a map or not
-- map / filter : apply a function on the values

-- toList : inverse of fromList

-- keys / elems : return a list of the keys or the values
