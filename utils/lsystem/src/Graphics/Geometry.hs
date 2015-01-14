module Graphics.Geometry where

import Graphics.Rendering.OpenGL ( GLfloat )

type Point = (GLfloat, GLfloat)
type Vector = (GLfloat, GLfloat)
type Segment = (Point, Point)
type LineEquation = (GLfloat, GLfloat, GLfloat)

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
distanceToSegment :: Point -> Segment -> GLfloat
distanceToSegment (x, y) l = abs (a * x + b * y + c) / n
    where (a, b, c) = lineEquation l
          n = sqrt $ squaredNorm (a, b)

-- | Rotation of a point around the origin.
rotO :: GLfloat -> Point -> Point
rotO theta (x, y) = (xrot, yrot)
    where xrot = x * (cos theta) - y * (sin theta)
          yrot = x * (sin theta) + y * (cos theta)

-- | Rotation of a point.
rot :: GLfloat -> Point -> Point -> Point
rot theta o p =
    let rotated = rotO theta $ p <-> o
    in rotated <+> o

