module Main exposing (main)

import Cursor exposing (Cursor)
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
  , historyCursor = Cursor.fromList "" []
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    TryToPush -> 
      case model.result of
        CalcResultOk r ->
          let
            newHistory = (model.input, r, newVariableName) :: model.history
            newVariableName = "res" ++ String.fromInt (List.length model.history + 1)
          in
            { model
            | history = newHistory
            , input = ""
            , result = CalcResultEmpty
            , variables = Dict.insert newVariableName r model.variables
            , historyCursor = Cursor.reverse <| Cursor.fromList "" <| List.map (\(s, _, _) -> s) newHistory
            }
        _ -> model
    UpdateInput input ->
      { model
      | input = input
      , result = inputToResult model.variables input
      , historyCursor = Cursor.update input model.historyCursor
      }
    ClearHistory ->
      { model
      | history = []
      , variables = defaultVariables
      , historyCursor = Cursor.fromList model.input []
      }
    ForwardHistory -> moveHistory Cursor.forward model
    BackHistory -> moveHistory Cursor.back model

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

inputToResult : Dict String Float -> String -> CalcResult
inputToResult variables input =
  let
    resultToNewExpression res =
      case res of
        Ok e -> CalcResultOk <| runExpression variables e
        Err _ -> CalcResultErr "invalid expression"
  in
    if String.isEmpty <| String.trim input
    then CalcResultEmpty
    else resultToNewExpression <| parseExpression (Dict.keys variables) input

moveHistory : (Cursor String -> Maybe (Cursor String)) -> Model -> Model
moveHistory move model =
  let
    newInput = Cursor.current newCursor
    newCursor = Maybe.withDefault model.historyCursor (move model.historyCursor)
  in
    { model
    | input = newInput
    , result = inputToResult model.variables newInput
    , historyCursor = newCursor
    }
