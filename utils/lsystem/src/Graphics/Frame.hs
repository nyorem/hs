module Graphics.Frame where

import Graphics.Rendering.OpenGL ( GLfloat )

import Graphics.Geometry

data Frame = Frame { origin :: Point
                    , xaxis :: Vector
                    , yaxis :: Vector
                    , angle :: GLfloat
                    }
                    deriving Show

-- | Frame translation.
translateFrame :: Vector -> Frame -> Frame
translateFrame v f@Frame { origin = o } = f { origin = o <+> v }

-- | Frame rotation.
rotateFrame :: GLfloat -> Frame -> Frame
rotateFrame theta f@Frame { xaxis = x, yaxis = y, angle = t } =
    f { xaxis = rotO theta x
      , yaxis = rotO theta y
      , angle = t + theta
      }

-- | The cartesian frame.
cartesianFrame :: Frame
cartesianFrame = Frame { origin = (0, 0)
                       , xaxis = (1, 0)
                       , yaxis = (0, 1)
                       , angle = 0
                       }

