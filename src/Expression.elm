module Expression exposing
  ( Expression(..)
  , showExpression
  )

type Expression = Expression String

showExpression : Expression -> String
showExpression (Expression s) = s
