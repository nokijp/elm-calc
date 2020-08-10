module ExpressionParser exposing (parseExpression)

import Expression exposing (..)
import Parser exposing (..)

parseExpression : List String -> String -> Result (List DeadEnd) Expression
parseExpression variables = run (spaces |> andThen (\_ -> expression variables |. end))

expression : List String -> Parser Expression
expression = term binaryOperators

term : List (List (Expression -> Expression -> Expression, String)) -> List String -> Parser Expression
term operators variables =
  case operators of
    [] -> factor variables
    (ops :: nextLevelOps) ->
      chainl1 (term nextLevelOps variables) <| oneOf <| List.map (\(op, s) -> constMap op (symbol s) |. spaces) ops

factor : List String -> Parser Expression
factor variables =
  oneOf
    [ number |. spaces
    , succeed Apply1
        |= function1Name |. spaces
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol ")" |. spaces
    , succeed Apply2
        |= function2Name |. spaces
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol "," |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol ")" |. spaces
    , succeed Variable
        |= getChompedString (oneOf <| List.map keyword variables) |. spaces
    , succeed Negate
        |. symbol  "-" |. spaces
        |= lazy (\_ -> expression variables)
    , succeed identity
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol ")" |. spaces
    ]

binaryOperators : List (List (Expression -> Expression -> Expression, String))
binaryOperators =
  [ [(Add, "+"), (Sub, "-")]
  , [(Mul, "*"), (Div, "/"), (Mod, "%")]
  , [(Pow, "^")]
  ]

function1Name : Parser Function1
function1Name =
  oneOf <|
    List.map (\(f, s) -> constMap f (keyword s))
      [ (Sqrt, "sqrt")
      , (Exp, "exp")
      , (Log, "log")
      , (Sin, "sin")
      , (Cos, "cos")
      , (Tan, "tan")
      ]

function2Name : Parser Function2
function2Name =
  oneOf <|
    List.map (\(f, s) -> constMap f (keyword s))
      [ (PowFunc, "pow")
      ]

number : Parser Expression
number = backtrackable <| Parser.map Number float

constMap : a -> Parser b -> Parser a
constMap a = Parser.map (\_ -> a)

chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep =
  succeed (List.foldl (\f e -> f e))
    |= p
    |= many (succeed (\op t e -> op e t) |= sep |= lazy (\_ -> p))

many : Parser a -> Parser (List a)
many p =
  oneOf
    [ succeed (::)
        |= p
        |= lazy (\_ -> many p)
    , succeed []
    ]
