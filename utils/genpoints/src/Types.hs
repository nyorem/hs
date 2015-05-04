module Types
    ( Point2
    , Point3
    , Plane(..)
    ) where

-- | A 2D point.
type Point2 = (Float, Float)

-- | A 3D point.
type Point3 = (Float, Float, Float)

-- | Type of a plane parallel to an axis.
data Plane = XY | XZ | YZ

