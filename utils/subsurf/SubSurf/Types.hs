module SubSurf.Types where

import qualified Data.Vector as V
import Graphics.Gloss

type Coeff = [(Int, Float)]
type Polygon = Path
type Points = V.Vector Point

type Scheme = (Coeff, Coeff)

type Segment = (Point, Point)
type LineEquation = (Float, Float, Float)

