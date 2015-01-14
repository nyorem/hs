module Geometry where

import Graphics.Rendering.OpenGL ( GLfloat )

type Point = (GLfloat, GLfloat)
type Vector = (GLfloat, GLfloat)

-- | Multiplication by a scalar.
(<**>) :: GLfloat -> Point -> Point
s <**> (x, y) = (s * x, s * y)

-- | Division by a non zero scalar.
(</>) :: Point -> GLfloat -> Point
(x, y) </> s = (x / s, y / s)

-- | Point addition.
(<+>) :: Point -> Point -> Point
(x, y) <+> (x', y') = (x + x', y + y')

-- | Point subtraction.
(<->) :: Point -> Point -> Point
(x, y) <-> (x', y') = (x - x', y - y')

-- | Dot product.
(<.>) :: Point -> Point -> GLfloat
(x, y) <.> (x', y') = x * x' + y * y'

-- | Unary minus for points.
negateP :: Point -> Point
negateP (x, y) = (-x, -y)

-- | Unary minus for vectors.
negateV :: Vector -> Vector
negateV (x, y) = (-x, -y)

-- | Determinant.
det :: Point -> Point -> GLfloat
det (x, y) (x', y') = x * y' - x' * y

-- | Computes the squared norm of a point.
squaredNorm :: Point -> GLfloat
squaredNorm (x, y) = sqrt $ x * x + y * y

-- | Squared distance between two points.
squaredDist :: Point -> Point -> GLfloat
squaredDist p q = squaredNorm $ p <-> q

-- | Normalizes a point.
normalize :: Point -> Point
normalize p = (recip $ sqrt (squaredNorm p)) <**> p

