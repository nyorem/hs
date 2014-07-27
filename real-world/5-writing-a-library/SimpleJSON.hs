-- if we did not specify functions (or type constructors), then everything is exported

module SimpleJSON (
                   JValue(..),
                   getString,
                   getInt,
                   getDouble,
                   getBool,
                   getArray,
                   getObject,
                   isNull,
                 )
where

-- truncate: remove the floating part of a floating number or a rational

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Ord, Show)

-- Accessors
getString :: JValue -> Maybe String
getString (JString str) = Just str
getString _             = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n)   = Just $ truncate n
getInt _             = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n)   = Just n
getDouble _             = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b)     = Just b
getBool _             = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a)     = Just a
getArray _              = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o)     = Just o
getObject _               = Nothing

isNull :: JValue -> Bool
isNull = (== JNull)

