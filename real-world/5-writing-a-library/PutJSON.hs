module PutJSON
where

import SimpleJSON
import Data.List ( intercalate )

-- | Renders a JSON value as a string.
renderJValue :: JValue -> String

renderJValue (JString str) = str
renderJValue (JNumber n) = show n
renderJValue (JBool b) = show b
renderJValue JNull = "null"
renderJValue (JArray a) = "[" ++ array ++ "]"
    where array = intercalate ", " $ map renderJValue a
renderJValue (JObject o) = "{" ++ object ++ "}"
    where object = intercalate ",\n" $ map (\(name, value) -> name ++ ": " ++ renderJValue value) o
