module ExpressionParser exposing (parseExpression)

import Expression exposing (..)
import Parser exposing (..)

parseExpression : String -> Result (List DeadEnd) Expression
parseExpression = run (spaces |> andThen (\_ -> expression |. end))

expression : Parser Expression
expression = chainl1 term <| oneOf [operator Add "+", operator Sub "-"]

term : Parser Expression
term = chainl1 factor <| oneOf [operator Mul "*", operator Div "/", operator Mod "%"]

factor : Parser Expression
factor =
  oneOf
    [ number |. spaces
    , succeed Apply
        |= functionName |. spaces
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression)
        |. symbol ")" |. spaces
    , succeed Negate
        |. symbol  "-" |. spaces
        |= lazy (\_ -> expression)
    , succeed identity
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression)
        |. symbol ")" |. spaces
    ]

functionName : Parser Function
functionName =
  oneOf <|
    List.map (\(f, s) -> constMap f (keyword s))
      [ (Sqrt, "sqrt")
      , (Exp, "exp")
      , (Log, "log")
      , (Sin, "sin")
      , (Cos, "cos")
      , (Tan, "tan")
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
