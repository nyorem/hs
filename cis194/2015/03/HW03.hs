module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st var val =
    \x -> if x == var then val else st x

empty :: State
empty =
    const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var x) = st x
evalE _ (Val i) = i
evalE st (Op e1 bop e2) =
    case bop of
        Plus   -> evalE st e1 + evalE st e2
        Minus  -> evalE st e1 - evalE st e2
        Times  -> evalE st e1 * evalE st e2
        Divide -> evalE st e1 `div` evalE st e2
        Gt     -> fromEnum $ evalE st e1 > evalE st e2
        Ge     -> fromEnum $ evalE st e1 >= evalE st e2
        Lt     -> fromEnum $ evalE st e1 < evalE st e2
        Le     -> fromEnum $ evalE st e1 >= evalE st e2
        Eql    -> fromEnum $ evalE st e1 == evalE st e2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign str expr) = DAssign str expr
desugar (Incr str) = DAssign str (Op (Var str) Plus (Val 1))
desugar (If cond true false) = DIf cond (desugar true) (desugar false)
desugar (While expr instrs) = DWhile expr (desugar instrs)
desugar (For init cond after body) =
    DSequence (desugar init)
              (DWhile cond (DSequence (desugar body) (desugar after)))
desugar (Sequence expr expr') = DSequence (desugar expr) (desugar expr')
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign str expr) =
    extend st str (evalE st expr)
evalSimple st (DIf cond true false) =
    if evalE st cond /= 0 then
        evalSimple st true
    else
        evalSimple st false
evalSimple st while@(DWhile cond body) =
    if evalE st cond /= 0 then
        evalSimple (evalSimple st body) while
    else
        st
evalSimple st (DSequence ds1 ds2) =
    evalSimple (evalSimple st ds1) ds2
evalSimple st DSkip = st

run :: State -> Statement -> State
run st =
    evalSimple st . desugar

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
