module Model exposing
  ( Msg(..)
  , NewExpression(..)
  , Model
  )

import Expression exposing (..)

type Msg
  = TryToPush
  | UpdateNewExpression String
  | ClearHistory

type NewExpression
  = ExpressionEmpty
  | ExpressionErr String
  | ExpressionOk String Expression Float

type alias Model =
  { history : List (String, Float)
  , newExpression : NewExpression
  }
