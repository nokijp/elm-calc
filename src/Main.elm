module Main exposing (main)

import Browser exposing (..)
import Dict exposing (Dict)
import Expression exposing (..)
import ExpressionParser exposing (..)
import Html.Styled exposing (..)
import Model exposing (..)
import View exposing (..)
import Dict

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
  , input = InputEmpty
  , variables = defaultVariables
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    TryToPush -> 
      case model.input of
        InputOk s r ->
          let
            newHistory = (s, r, newVariableName) :: model.history
            newVariables = Dict.insert newVariableName r model.variables
            newVariableName = "res" ++ String.fromInt (List.length model.history + 1)
          in
            { model
            | history = newHistory
            , variables = newVariables
            }
        _ -> model
    UpdateInput input ->
      let
        newInput =
          if String.isEmpty input
          then InputEmpty
          else resultToNewExpression <| parseExpression (Dict.keys model.variables) input
        resultToNewExpression res =
          case res of
            Ok e -> InputOk input (runExpression model.variables e)
            Err _ -> InputErr "invalid expression"
      in
        { model
        | input = newInput
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
