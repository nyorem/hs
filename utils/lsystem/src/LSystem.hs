-- TODO: remove the restriction of one rule for each predecessor (stochastic grammar)
-- TODO: check changeAxiom
-- TODO: file parsing

module LSystem where

import Data.List ( intercalate )

type Symbol = String

type Predecessor = Symbol
type Successor = Symbol
type Rule = (Predecessor, [Successor])

data Grammar = Grammar { vars :: [Symbol]
                       , constants :: [Symbol]
                       , axiom :: [Symbol]
                       , rules :: [Rule]
                       }

showRule :: Rule -> String
showRule (p, ss) = p ++ " -> " ++ (intercalate " " ss)

showGrammar :: Grammar -> String
showGrammar g = concat
    [ "vars: " ++ (show $ vars g) ++ "\n"
    , "constants: " ++ (show $ constants g) ++ "\n"
    , "rules: " ++ (intercalate ", " . map showRule $ rules g) ++ "\n"
    , "axiom: " ++ (show $ axiom g)
    ]

addRule :: Predecessor -> [Successor] -> Grammar -> Grammar
addRule p ss g = g { rules = (p, ss) : (rules g) }

changeVars :: [Symbol] -> Grammar -> Grammar
changeVars vs g = g { vars = vs }

changeConstants :: [Symbol] -> Grammar -> Grammar
changeConstants cs g = g { constants = cs }

changeAxiom :: [Symbol] -> Grammar -> Grammar
changeAxiom a g = g { axiom = a }

emptyGrammar :: Grammar
emptyGrammar = Grammar { vars = []
                       , constants = []
                       , axiom = []
                       , rules = []
                       }

isVariable :: Symbol -> Grammar -> Bool
isVariable s Grammar { vars = v } = s `elem` v

step :: Grammar -> [Symbol] -> [Symbol]
step _ [] = []
step g (s:ss) =
    case lookup s (rules g) of
        Nothing -> s : step g ss
        Just r -> r ++ step g ss

stepN :: Int -> Grammar -> [Symbol] -> [[Symbol]]
stepN 0 _ ss = [ss]
stepN n g ss = ss : stepN (n - 1) g (step g ss)

stepNAxiom :: Int -> Grammar -> [[Symbol]]
stepNAxiom n g = stepN n g (axiom g)

nthStep :: Int -> Grammar -> [Symbol]
nthStep n g = last $ stepNAxiom n g

