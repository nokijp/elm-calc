module ExpressionTests exposing (tests)

import Dict exposing (Dict)
import Expect
import Test exposing (..)

import Expression exposing (..)

tests : Test
tests =
  describe "Expression"
    [ describe "runExpression"
        (expressions |> List.map (\(e, r) ->
          test ("it can evaluate " ++ Debug.toString e) <|
            \_ -> runExpression variables e |> Expect.within (Expect.Absolute 1e-10) r)
        )
    ]

variables : Dict String Float
variables =
  Dict.fromList
    [ ("v1", 1.5)
    , ("v2", 100.0)
    ]

expressions : List (Expression, Float)
expressions =
  [ (Number 1.0, 1.0)
  , (Number 2.0, 2.0)
  , (Negate (Number 5.0), -5.0)
  , (Add (Number 2.0) (Number 3.0), 5.0)
  , (Sub (Number 2.0) (Number 3.0), -1.0)
  , (Mul (Number 2.0) (Number 3.0), 6.0)
  , (Div (Number 2.0) (Number 4.0), 0.5)
  , (Mod (Number 7.0) (Number 5.0), 2.0)
  , (Mod (Number -7.0) (Number 5.0), -2.0)
  , (Mod (Number -7.0) (Number -5.0), -2.0)
  , (Mod (Number 7.0) (Number -5.0), 2.0)
  , (Mod (Number 7.5) (Number 5.2), 2.3)
  , (Mod (Number -7.5) (Number 5.2), -2.3)
  , (Mod (Number -7.5) (Number -5.2), -2.3)
  , (Mod (Number 7.5) (Number -5.2), 2.3)
  , (Apply1 Sqrt (Number 2.0), 1.4142135624)
  , (Apply1 Exp (Number 2.0), 7.3890560989)
  , (Apply1 Log (Number 10.0), 2.3025850930)
  , (Apply1 Sin (Number 2.0), 0.9092974268)
  , (Apply1 Cos (Number 2.0), -0.4161468365)
  , (Apply1 Tan (Number 2.0), -2.1850398633)
  , (Apply2 Pow (Number 2.0) (Number 3.0), 8.0)
  , (Variable "v1", 1.5)
  , (Variable "v2", 100.0)
  , (Add (Sub (Add (Number 1.0) (Number 2.0)) (Number 3.0)) (Number 4.0), 4.0)  -- 1 + 2 - 3 + 4
  , (Sub (Add (Number 1.0) (Number 2.0)) (Mul (Number 3.0) (Number 4.0)), -9.0)  -- 1 + 2 - 3 * 4
  ]
