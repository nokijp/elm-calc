module ExpressionParser exposing (parseExpression)

import Expression exposing (..)
import Parser exposing (..)

parseExpression : List String -> String -> Result (List DeadEnd) Expression
parseExpression variables = run (spaces |> andThen (\_ -> expression variables |. end))

expression : List String -> Parser Expression
expression variables = chainl1 (term variables) <| oneOf [operator Add "+", operator Sub "-"]

term : List String -> Parser Expression
term variables = chainl1 (factor variables) <| oneOf [operator Mul "*", operator Div "/", operator Mod "%"]

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
      [ (Pow, "pow")
      ]

number : Parser Expression
number = backtrackable <| Parser.map Number float

operator : a -> String -> Parser a
operator a s = constMap a (symbol s) |. spaces

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
