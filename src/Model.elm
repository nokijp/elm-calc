module Model exposing
  ( Msg(..)
  , CalcResult(..)
  , Model
  )

import Dict exposing (Dict)
import Expression exposing (..)

type Msg
  = TryToPush
  | UpdateInput String
  | ClearHistory

type CalcResult
  = CalcResultEmpty
  | CalcResultErr String
  | CalcResultOk Float

type alias Model =
  { history : List (String, Float, String)
  , input : String
  , result : CalcResult
  , variables : Dict String Float
  }
