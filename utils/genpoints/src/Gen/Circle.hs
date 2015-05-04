module Gen.Circle
    ( generate
    ) where

import Types

generate :: Point3 -> Float -> Plane -> Int -> [Point3]
generate (xo, yo, zo) r p n =
    case p of
        XY -> [ (xo * r * cos (2 * fromIntegral i * pi / fromIntegral n),
                 yo + r * sin (2 * fromIntegral i * pi / fromIntegral n),
                 0)
                 | i <- [ 0 .. (n - 1) ]
              ]
        XZ -> [ (xo + r * cos (2 * fromIntegral i * pi / fromIntegral n),
                 0,
                 zo + r * sin (2 * fromIntegral i * pi / fromIntegral n))
                 | i <- [ 0 .. (n - 1) ]
              ]
        YZ -> [ (0,
                 yo + r * cos (2 * fromIntegral i * pi / fromIntegral n),
                 zo + r * sin (2 * fromIntegral i * pi / fromIntegral n))
                 | i <- [ 0 .. (n - 1) ]
              ]

