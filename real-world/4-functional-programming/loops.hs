import Data.Char ( digitToInt )

-- | Converts a string to an int.
asInt :: String
      -> Int
asInt str = loop 0 str
    where loop acc [] = acc
          loop acc (x:xs) =
              let acc' = acc * 10 + digitToInt x in
                  loop acc' xs

