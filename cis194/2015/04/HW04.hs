{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List ( intercalate )

newtype Poly a = P { coeffs :: [a] }

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x =
    P [ 0, 1 ]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P p == P q =
        length p == length q && p == q

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = showPoly

showPoly :: (Num a, Eq a, Show a) => Poly a -> String
showPoly (P p)
    | all (== 0) p = "0"
    | otherwise =
        intercalate " + " . filter (not . null) . reverse $ map (\(c, i) ->
            if c == 0 then ""
            else ((if c == 1 && i /= 0 then ""
                   else if c == -1 && i /= 0 then "-"
                   else show c) ++ (if i == 0 then ""
                                    else "x" ++ (if i == 1 then ""
                                                 else "^" ++ show i)))) $ zip p [ 0 .. length p - 1 ]

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p) (P q) =
    let (lenp, lenq) = (length p, length q)
        len = if lenp >= lenq then lenp - lenq else lenq - lenp
        p' = if lenp >= lenq then p else p ++ replicate len 0
        q' = if lenq >= lenp then q else q ++ replicate len 0
    in
        P $ zipWith (+) p' q'

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p) (P q) =
    sum $ map (\(c, i) -> P $ replicate i 0 ++ map (c *) q) $ zip p [ 0 .. ]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P . map negate $ p
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) a =
    foldr (\c acc -> c + a * acc) 0 p

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 a = a
    nderiv n a = nderiv (n - 1) (deriv a)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P p) =
        P . tail . map (\(c, i) -> c * fromIntegral i) $ zip p [ 0 .. length p - 1]

