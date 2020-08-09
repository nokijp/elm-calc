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
  , newExpression = ExpressionEmpty
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    TryToPush -> 
      case model.newExpression of
        ExpressionOk s _ r -> { model | history = (s, r) :: model.history }
        _ -> model
    UpdateNewExpression input ->
      let
        newExpression =
          if String.isEmpty input
          then ExpressionEmpty
          else resultToNewExpression <| parseExpression (Dict.keys defaultVariables) input
        resultToNewExpression res =
          case res of
             Ok e -> ExpressionOk input e (runExpression defaultVariables e)
             Err _ -> ExpressionErr "invalid expression"
      in { model | newExpression = newExpression }
    ClearHistory -> { model | history = [] }

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
