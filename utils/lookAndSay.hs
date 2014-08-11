import Data.List ( group )

stepLookAndSay :: Integer -> Integer
stepLookAndSay = read .
                 concatMap (\xs -> show (length xs) ++  [head xs]) .
                 group .
                 show

lookAndSay :: Integer -> Int -> [Integer]
lookAndSay n0 n = take n $ iterate stepLookAndSay n0

digits :: Integer -> Integer
digits = fromIntegral . length . show

conwayConstant :: Integer -> Int -> [Double]
conwayConstant n0 n = zipWith (\x y -> (fromIntegral $ digits x) / (fromIntegral $ digits y)) cnp1 cn
    where cn = lookAndSay n0 n
          cnp1 = tail $ lookAndSay n0 (n + 1)

