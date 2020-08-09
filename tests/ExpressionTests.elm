module ExpressionTests exposing (tests)

import Expect
import Test exposing (..)

import Expression exposing (..)

tests : Test
tests =
  describe "Expression"
    [ describe "runExpression"
        (expressions |> List.map (\(e, r) ->
          test ("it can evaluate " ++ Debug.toString e) <|
            \_ -> runExpression e |> Expect.within (Expect.Absolute 1e-10) r)
        )
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
  , (Apply Exp (Number 2.0), 7.3890560989)
  , (Apply Log (Number 10.0), 2.3025850930)
  , (Apply Sin (Number 2.0), 0.9092974268)
  , (Apply Cos (Number 2.0), -0.4161468365)
  , (Apply Tan (Number 2.0), -2.1850398633)
  , (Add (Sub (Add (Number 1.0) (Number 2.0)) (Number 3.0)) (Number 4.0), 4.0)  -- 1 + 2 - 3 + 4
  , (Sub (Add (Number 1.0) (Number 2.0)) (Mul (Number 3.0) (Number 4.0)), -9.0)  -- 1 + 2 - 3 * 4
  ]
