module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Expression exposing (..)
import ExpressionParser exposing (..)

type Msg
  = TryToPush
  | UpdateNewExpression String
  | ClearHistory

type NewExpression = ExpressionEmpty | ExpressionErr String | ExpressionOk Expression
type alias Model =
  { history : List (Expression, Float)
  , newExpression : NewExpression
  }

main : Platform.Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
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
        ExpressionOk e -> { model | history = (e, runExpression e) :: model.history }
        _ -> model
    UpdateNewExpression input ->
      let
        newExpression =
          if String.isEmpty input
          then ExpressionEmpty
          else resultToNewExpression <| parseExpression input
        resultToNewExpression res =
          case res of
             Ok e -> ExpressionOk e
             Err _ -> ExpressionErr "invalid expression"
      in { model | newExpression = newExpression }
    ClearHistory -> { model | history = [] }

view : Model -> Html Msg
view model =
  let
    message =
      case model.newExpression of
         ExpressionEmpty -> ""
         ExpressionErr e -> e
         ExpressionOk e -> "= " ++ String.fromFloat (runExpression e)
  in div []
    [ form [ onSubmit TryToPush ]
      [ input [ onInput UpdateNewExpression, placeholder "enter an expression, e.g. 1 + 1 * 3" ] []
      ]
    , p [] [ text message ]
    , button [ onClick ClearHistory ] [ text "clear" ]
    , div [] [ historyView model.history ]
    ]

historyView : List (Expression, Float) -> Html a
historyView history =
  ul [] <| List.map (\(e, res) -> li [] [text <| showExpression e ++ " = " ++ String.fromFloat res]) history
