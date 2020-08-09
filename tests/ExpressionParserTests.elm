module ExpressionParserTests exposing (tests)

import Expect
import Test exposing (..)

import Expression exposing (..)
import ExpressionParser exposing (..)

tests : Test
tests =
  describe "ExpressionParser"
    [ describe "parseExpression"
        (validExpressions |> List.map (\(s, e) ->
          test ("it can parse " ++ s) <|
            \_ -> parseExpression variables s |> Expect.equal (Ok e))
        )
    ]

variables : List String
variables =
  [ "v"
  , "v0"
  , "abc"
  , "exp"
  ]

validExpressions : List (String, Expression)
validExpressions =
  [ (" 1 ", Number 1.0)
  , (" 1 + 2 ", Add (Number 1.0) (Number 2.0))
  , (" 1 + 2 + 3 ", Add (Add (Number 1.0) (Number 2.0)) (Number 3.0))
  , (" 1 + 2 - 3 + 4 ", Add (Sub (Add (Number 1.0) (Number 2.0)) (Number 3.0)) (Number 4.0))
  , (" 1 + 2 * 3 ", Add (Number 1.0) (Mul (Number 2.0) (Number 3.0)))
  , (" 1 + 2 / 3 * 4 ", Add (Number 1.0) (Mul (Div (Number 2.0) (Number 3.0)) (Number 4.0)))
  , (" 1 + 2 / 3 * 4 + 5 ", Add (Add (Number 1.0) (Mul (Div (Number 2.0) (Number 3.0)) (Number 4.0))) (Number 5.0))
  , (" 1 + ( 2 + 3 ) + 4 ", Add (Add (Number 1.0) (Add (Number 2.0) (Number 3.0))) (Number 4.0))
  , (" 1 + ( 2 + ( 3 + 4 )) ", Add (Number 1.0) (Add (Number 2.0) (Add (Number 3.0) (Number 4.0))))
  , (" ( 1 + 2 ) * 3 ", Mul (Add (Number 1.0) (Number 2.0)) (Number 3.0))
  , (" 1 * ( 2 + 3 ) ", Mul (Number 1.0) (Add (Number 2.0) (Number 3.0)))
  , (" - 1 ", Negate (Number 1.0))
  , (" 1 + - 2 ", Add (Number 1.0) (Negate (Number 2.0)))
  , (" 1 * - 2 ", Mul (Number 1.0) (Negate (Number 2.0)))
  , (" sqrt ( 1 ) ", Apply Sqrt (Number 1.0))
  , (" exp ( 1 ) ", Apply Exp (Number 1.0))
  , (" log ( 1 ) ", Apply Log (Number 1.0))
  , (" sin ( 1 ) ", Apply Sin (Number 1.0))
  , (" cos ( 1 ) ", Apply Cos (Number 1.0))
  , (" tan ( 1 ) ", Apply Tan (Number 1.0))
  , (" pow ( 1 , 2 ) ", Apply2 Pow (Number 1.0) (Number 2.0))
  , (" 1 + exp ( 2 ) + 3 ", Add (Add (Number 1.0) (Apply Exp (Number 2.0))) (Number 3.0))
  , (" v ", Variable "v")
  , (" v0 ", Variable "v0")
  , (" abc ", Variable "abc")
  , (" 1 + v + 2 ", Add (Add (Number 1.0) (Variable "v")) (Number 2.0))
  , (" ( 1 ) ", Number 1.0)
  , (" ( ( ( 1 ) ) ) ", Number 1.0)
  , (" 1.5 ", Number 1.5)
  , (" 1.5 + 2.5 ", Add (Number 1.5) (Number 2.5))
  , (" 1 + 2 / 3 * sin ( 4 ) + 5 ", Add (Add (Number 1.0) (Mul (Div (Number 2.0) (Number 3.0)) (Apply Sin (Number 4.0)))) (Number 5.0))
  , ("1+2/3*sin(4)+5", Add (Add (Number 1.0) (Mul (Div (Number 2.0) (Number 3.0)) (Apply Sin (Number 4.0)))) (Number 5.0))
  ]
