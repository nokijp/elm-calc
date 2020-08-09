module ExpressionParser exposing (parseExpression)

import Expression exposing (..)

parseExpression : String -> Result String Expression
parseExpression s = if s /= "error" then Ok <| Expression s else Err "error"
