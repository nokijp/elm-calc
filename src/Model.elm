module Model exposing
  ( Msg(..)
  , Input(..)
  , Model
  )

import Dict exposing (Dict)
import Expression exposing (..)

type Msg
  = TryToPush
  | UpdateInput String
  | ClearHistory

type Input
  = InputEmpty
  | InputErr String
  | InputOk String Float

type alias Model =
  { history : List (String, Float, String)
  , input : Input
  , variables : Dict String Float
  }
