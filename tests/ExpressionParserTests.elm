module ExpressionParserTests exposing (tests)

import Expect
import Test exposing (..)
import ExpressionParser exposing (..)

tests : Test
tests =
  describe "ExpressionParser"
    [ describe "parseExpression"
        [ test "it should return an error when given \"error\"" <|
            \_ -> parseExpression "error" |> Expect.equal (Err "error")
        ]
    ]
