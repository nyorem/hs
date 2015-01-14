-- http://en.wikipedia.org/wiki/L-system

module Examples where

import LSystem
import Graphics.Frame
import Graphics.Geometry
import Graphics.Display

import Graphics.Rendering.OpenGL

-- Algae:
-- A -> A B
-- B -> A
-- axiom: A
algae :: Grammar
algae = changeConstants [] .
        changeVars ["A", "B"] .
        changeAxiom ["A"] .
        addRule "A" ["A", "B"] .
        addRule "B" ["A"] $ emptyGrammar

-- Pythagoras tree:
-- 1 -> 1 1
-- 0 -> 1 [ 0 ] 0
-- axiom: 0
pythagorasTree :: Grammar
pythagorasTree = changeConstants ["[", "]"] .
                 changeVars ["0", "1"] .
                 changeAxiom ["0"] .
                 addRule "1" ["1", "1"] .
                 addRule "0" ["1", "[", "0", "]", "0"] $ emptyGrammar

-- Cantor dust:
-- A -> A B A
-- B -> B B B
-- axiom: A
cantorDust :: Grammar
cantorDust = changeConstants [] .
             changeVars ["A", "B"] .
             changeAxiom ["A"] .
             addRule "A" ["A", "B", "A"] .
             addRule "B" ["B", "B", "B"] $ emptyGrammar

cantorVarFunctions :: [(Symbol, Frame -> IO Frame)]
cantorVarFunctions =
    [ ("A", \f@Frame { origin = (x0, y0), xaxis = x } -> do
        let (x1, y1) = (x0, y0) <+> (x </> 16)
            newFrame = translateFrame (x </> 16) f
        renderPrimitive Lines $ do
            color3f $ Color3 1 1 1
            vertex3f $ Vertex3 x0 y0 0
            vertex3f $ Vertex3 x1 y1 0
        return newFrame
      )
    , ("B", \f@Frame{ xaxis = x } -> return $ translateFrame (x </> 16) f)
    ]

cantorConstantFunctions :: [(Symbol, Frame -> IO Frame)]
cantorConstantFunctions = []

-- Koch curve:
-- F -> F + F - F - F + F
-- axiom: F
kochCurve :: Grammar
kochCurve = changeConstants ["+", "-"] .
            changeVars ["F"] .
            changeAxiom ["F"] .
            addRule "F" ["F", "+", "F", "-", "F", "-", "F", "+", "F"] $ emptyGrammar


kochVarFunctions :: [(Symbol, Frame -> IO Frame)]
kochVarFunctions =
    [ ("F", \f@Frame { origin = (x0, y0), xaxis = x } -> do
        let (x1, y1) = (x0, y0) <+> (x </> 16)
            newFrame = translateFrame (x </> 16) f
        renderPrimitive Lines $ do
            color3f $ Color3 1 1 1
            vertex3f $ Vertex3 x0 y0 0
            vertex3f $ Vertex3 x1 y1 0
        return newFrame
      )
    ]

kochConstantFunctions :: [(Symbol, Frame -> IO Frame)]
kochConstantFunctions =
    [ ("+", return . rotateFrame (pi / 2)
      )
    , ("-", return . rotateFrame (-pi / 2)
      )
    ]

-- Dragon curve:
-- X -> X + Y F +
-- Y -> - F X - Y
-- axiom: F X
-- angle: 90
dragonCurve :: Grammar
dragonCurve = changeConstants ["F", "+", "-"] .
              changeVars ["X", "Y"] .
              changeAxiom ["F", "X"] .
              addRule "X" ["X", "+", "Y", "F", "+"] .
              addRule "Y" ["-", "F", "X", "-", "Y"] $ emptyGrammar

dragonVarFunctions :: [(Symbol, Frame -> IO Frame)]
dragonVarFunctions =
    [ ("X", return . id
      )
    , ("Y", return . id
      )
    ]

dragonConstantFunctions :: [(Symbol, Frame -> IO Frame)]
dragonConstantFunctions =
    [ ("F", \f@Frame { origin = (x0, y0), xaxis = x } -> do
        let (x1, y1) = (x0, y0) <+> (x </> 32)
            newFrame = translateFrame (x </> 32) f
        renderPrimitive Lines $ do
            color3f $ Color3 1 1 1
            vertex3f $ Vertex3 x0 y0 0
            vertex3f $ Vertex3 x1 y1 0
        return newFrame
      )
    , ("+", return . rotateFrame (pi / 2)
      )
    , ("-", return . rotateFrame (-pi / 2)
      )
    ]

