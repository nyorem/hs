-- we define our own Complex data type
data Complex = Complex { re :: Float, im :: Float } deriving (Show, Eq)

instance Num Complex where
    fromInteger n = Complex (fromIntegral n) 0.0
    Complex x y + Complex x' y' = Complex (x + x') (y + y')
    Complex x y * Complex x' y' = Complex (x * x' - y * y') (x + y' + x' * y)
    negate (Complex x y) = Complex (-x) (-y)
    abs (Complex x y) = Complex (sqrt (x * x + y * y)) 0.0
    signum (Complex x y) = Complex (signum x) 0.0

complex :: Float -> Float -> Complex
complex = Complex

magnitude :: Complex -> Float
magnitude = re . abs

-- Mandelbrot sequence
mandel :: Complex -- ^ c
       -> Complex -- ^ z_0
       -> Int     -- ^ Maximum number of iterations
       -> Int     -- ^ Number of iterations done.
mandel c z 0 = 0
mandel c z n
    | magnitude z > 2 = n
    | otherwise = mandel c (z * z + c) (n - 1)

