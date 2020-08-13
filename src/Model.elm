module Model exposing
  ( Msg(..)
  , CalcResult(..)
  , Model
  )

import Cursor exposing (..)
import Dict exposing (Dict)
import Expression exposing (..)

type Msg
  = TryToPush
  | UpdateInput String
  | ClearHistory
  | ForwardHistory
  | BackHistory

type CalcResult
  = CalcResultEmpty
  | CalcResultErr String
  | CalcResultOk Float

type alias Model =
  { history : List (String, Float, String)
  , input : String
  , result : CalcResult
  , variables : Dict String Float
  , historyCursor : Cursor String
  }
