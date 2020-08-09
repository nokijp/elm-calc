module Main exposing (main)

import Browser exposing (..)
import Dict exposing (Dict)
import Expression exposing (..)
import ExpressionParser exposing (..)
import Html.Styled exposing (..)
import Model exposing (..)
import View exposing (..)

main : Platform.Program () Model Msg
main =
  Browser.document
    { init = \_ -> (init, Cmd.none)
    , view = view
    , update = \msg model -> (update msg model, Cmd.none)
    , subscriptions = \_ -> Sub.none
    }

init : Model
init =
  { history = []
  , input = ""
  , result = CalcResultEmpty
  , variables = defaultVariables
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    TryToPush -> 
      case model.result of
        CalcResultOk r ->
          let
            newHistory = (model.input, r, newVariableName) :: model.history
            newVariables = Dict.insert newVariableName r model.variables
            newVariableName = "res" ++ String.fromInt (List.length model.history + 1)
          in
            { model
            | history = newHistory
            , input = ""
            , result = CalcResultEmpty
            , variables = newVariables
            }
        _ -> model
    UpdateInput input ->
      let
        newResult =
          if String.isEmpty <| String.trim input
          then CalcResultEmpty
          else resultToNewExpression <| parseExpression (Dict.keys model.variables) input
        resultToNewExpression res =
          case res of
            Ok e -> CalcResultOk <| runExpression model.variables e
            Err _ -> CalcResultErr "invalid expression"
      in
        { model
        | input = input
        , result = newResult
        }
    ClearHistory ->
      { model
      | history = []
      , variables = defaultVariables
      }

view : Model -> Document Msg
view model =
  { title = "Elm Calc"
  , body = List.map toUnstyled <| bodyContent model
  }

defaultVariables : Dict String Float
defaultVariables =
  Dict.fromList
    [ ("pi", pi)
    , ("e", e)
    ]
