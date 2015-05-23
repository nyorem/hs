module SubSurf.Geometry where

import Graphics.Gloss
import SubSurf.Types

-- | Multiplication by a scalar.
(<**>) :: Float -> Point -> Point
s <**> (x, y) = (s * x, s * y)

-- | Point addition.
(<+>) :: Point -> Point -> Point
(x, y) <+> (x', y') = (x + x', y + y')

-- | Point subtraction.
(<->) :: Point -> Point -> Point
(x, y) <-> (x', y') = (x - x', y - y')

-- | Dot product.
(<.>) :: Point -> Point -> Float
(x, y) <.> (x', y') = x * x' + y * y'

-- | Determinant.
det :: Point -> Point -> Float
det (x, y) (x', y') = x * y' - x' * y

-- | Computes the squared norm of a point.
squaredNorm :: Point -> Float
squaredNorm (x, y) = sqrt $ x * x + y * y

-- | Squared distance between two points.
squaredDist :: Point -> Point -> Float
squaredDist p q = squaredNorm $ p <-> q

-- | Normalizes a point.
normalize :: Point -> Point
normalize p = (recip $ sqrt (squaredNorm p)) <**> p

-- | Angle between three points.
angle :: Point -> Point -> Point -> Float
angle a b c = atan2 (det u v) (u <.> v)
    where u = a <-> b
          v = c <-> b

-- | List of all angles of a polygon.
angles :: Polygon -> [Float]
angles poly = angles' poly'
    where poly' = poly ++ init poly
          angles' (a:b:c:ps) = angle a b c : angles' (b:c:ps)
          angles' _ = []

-- | Does the projection of a point belong to a segment?
projectionOnSegment :: Point -> Segment -> Bool
projectionOnSegment v s = onSegment (projectionLine v s) s

-- | Computes the projection of a point on a line.
projectionLine :: Point -> Segment -> Point
projectionLine p (q, r) = proj
    where u = normalize $ r <-> q
          v = p <-> q
          proj = q <+> ((u <.> v) <**> u)

-- | Equation of a line.
lineEquation :: Segment -> LineEquation
lineEquation ((sx, sy), (tx, ty)) = (a, b, c)
    where dx = sx - tx
          dy = sy - ty
          a = dy
          b = -dx
          c = - a * sx - b * sy

-- | Does a point belong to a line?
onLine :: Point -> LineEquation -> Bool
onLine (x, y) (a, b, c) = a * x + b * y + c == 0

-- | Does a point belong to a segment?
onSegment :: Point -> Segment -> Bool
onSegment p@(x, y) (q@(sx, sy), r@(tx, ty)) =
              onLine p eq &&
              ( (x >= sx && x <= tx ) &&
                (y >= sy && y <= ty ) ) ||
              ( (x >= tx && x <= sx ) &&
                (y >= ty && y <= sy ) )
    where eq = lineEquation (q, r)

-- | Computes the distance to a segment.
distanceToSegment :: Point -> Segment -> Float
distanceToSegment (x, y) l = abs (a * x + b * y + c) / n
    where (a, b, c) = lineEquation l
          n = sqrt $ squaredNorm (a, b)

-- | Faces of a polygon.
faces :: Polygon -> [Segment]
faces poly = zip poly poly'
    where poly' = (tail poly) ++ [head poly]

