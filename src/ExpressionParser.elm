module ExpressionParser exposing (parseExpression)

import Expression exposing (..)
import Parser exposing (..)

parseExpression : List String -> String -> Result (List DeadEnd) Expression
parseExpression variables = run (spaces |> andThen (\_ -> expression variables |. end))

expression : List String -> Parser Expression
expression = term binaryOperators

term : List (Parser (Expression -> Expression -> Expression)) -> List String -> Parser Expression
term operators variables =
  case operators of
    [] -> factorial variables
    op :: nextLevelOps -> chainl1 (term nextLevelOps variables) (op |. spaces)

factorial : List String -> Parser Expression
factorial variables =
  succeed (List.foldl (\_ -> Factorial))
    |= lazy (\_ -> factor variables)
    |= many (symbol "!" |. spaces)

factor : List String -> Parser Expression
factor variables =
  oneOf
    [ number |. spaces
    , succeed identity
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol ")" |. spaces
    , succeed Negate
        |. symbol  "-" |. spaces
        |= lazy (\_ -> expression variables)
    , succeed Apply1
        |= function1 |. spaces
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol ")" |. spaces
    , succeed Apply2
        |= function2 |. spaces
        |. symbol "(" |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol "," |. spaces
        |= lazy (\_ -> expression variables)
        |. symbol ")" |. spaces
    , succeed Variable
        |= getChompedString (oneOf <| List.map keyword variables) |. spaces
    ]

binaryOperators : List (Parser (Expression -> Expression -> Expression))
binaryOperators =
  List.map (oneOf << List.map (\(op, s) -> constMap op (symbol s)))
    [ [(Add, "+"), (Sub, "-")]
    , [(Mul, "*"), (Div, "/"), (Mod, "%")]
    , [(Pow, "^")]
    ]

function1 : Parser Function1
function1 =
  oneOf <| List.map (\(f, s) -> constMap f (keyword s))
    [ (Sqrt, "sqrt")
    , (Exp, "exp")
    , (Log, "log")
    , (Sin, "sin")
    , (Cos, "cos")
    , (Tan, "tan")
    ]

function2 : Parser Function2
function2 =
  oneOf <| List.map (\(f, s) -> constMap f (keyword s))
    [ (PowFunc, "pow")
    ]

number : Parser Expression
number = Parser.map Number (backtrackable float)

constMap : a -> Parser b -> Parser a
constMap a = Parser.map (always a)

chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep =
  succeed (List.foldl (<|))
    |= p
    |= many (succeed (\op t e -> op e t) |= sep |= p)

many : Parser a -> Parser (List a)
many p =
  oneOf
    [ succeed (::)
        |= p
        |= lazy (\_ -> many p)
    , succeed []
    ]
