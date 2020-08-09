module Expression exposing
  ( Expression(..)
  , Function(..)
  , runExpression
  )

type Expression
  = Number Float
  | Negate Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Mod Expression Expression
  | Apply Function Expression

type Function
  = Sqrt
  | Exp
  | Log
  | Sin
  | Cos
  | Tan

runExpression : Expression -> Float
runExpression expression =
  case expression of
    Number x -> x
    Negate x -> -(runExpression x)
    Add e1 e2 -> runExpression e1 + runExpression e2
    Sub e1 e2 -> runExpression e1 - runExpression e2
    Mul e1 e2 -> runExpression e1 * runExpression e2
    Div e1 e2 -> runExpression e1 / runExpression e2
    Mod e1 e2 -> fmod (runExpression e1) (runExpression e2)
    Apply Sqrt e -> sqrt <| runExpression e
    Apply Exp e -> (\x -> Basics.e ^ x) <| runExpression e
    Apply Log e -> logBase Basics.e <| runExpression e
    Apply Sin e -> sin <| runExpression e
    Apply Cos e -> cos <| runExpression e
    Apply Tan e -> tan <| runExpression e

fmod : Float -> Float -> Float
fmod p q =
  let
    r = frac (abs (p / q)) * abs q
    frac x = x - toFloat (floor x)
  in if p >= 0.0 then r else -r
