module Expression exposing
  ( Expression(..)
  , Function1(..)
  , Function2(..)
  , runExpression
  )

import Dict exposing (Dict)

type Expression
  = Number Float
  | Negate Expression
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Mod Expression Expression
  | Apply1 Function1 Expression
  | Apply2 Function2 Expression Expression
  | Variable String

type Function1
  = Sqrt
  | Exp
  | Log
  | Sin
  | Cos
  | Tan

type Function2
  = Pow

runExpression : Dict String Float -> Expression -> Float
runExpression variables expression =
  let run = runExpression variables
  in
    case expression of
      Number x -> x
      Negate x -> -(run x)
      Add e1 e2 -> run e1 + run e2
      Sub e1 e2 -> run e1 - run e2
      Mul e1 e2 -> run e1 * run e2
      Div e1 e2 -> run e1 / run e2
      Mod e1 e2 -> fmod (run e1) (run e2)
      Apply1 Sqrt e -> sqrt <| run e
      Apply1 Exp e -> Basics.e ^ run e
      Apply1 Log e -> logBase Basics.e <| run e
      Apply1 Sin e -> sin <| run e
      Apply1 Cos e -> cos <| run e
      Apply1 Tan e -> tan <| run e
      Apply2 Pow e1 e2 -> run e1 ^ run e2
      Variable s -> let nan = 0.0 / 0.0 in Maybe.withDefault nan <| Dict.get s variables

fmod : Float -> Float -> Float
fmod p q =
  let
    r = frac (abs (p / q)) * abs q
    frac x = x - toFloat (floor x)
  in if p >= 0.0 then r else -r
