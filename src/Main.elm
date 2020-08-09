module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Expression exposing (..)
import ExpressionParser exposing (..)
import Html.Attributes exposing (placeholder)

type Msg
  = TryToPush
  | UpdateNewExpression String
  | ClearHistory

type NewExpression = ExpressionEmpty | ExpressionErr String | ExpressionOk Expression
type alias Model =
  { history : List Expression
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
        ExpressionOk e -> { model | history = e :: model.history }
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
             Err s -> ExpressionErr s
      in { model | newExpression = newExpression }
    ClearHistory -> { model | history = [] }

view : Model -> Html Msg
view model =
  let
    errorMessage =
      case model.newExpression of
         ExpressionErr e -> e
         _ -> ""
  in div []
    [ form [ onSubmit TryToPush ]
      [ input [ onInput UpdateNewExpression, placeholder "enter an expression, e.g. 1 + 1 * 3" ] []
      ]
    , p [] [ text errorMessage ]
    , button [ onClick ClearHistory ] [ text "clear" ]
    , div [] [ historyView model.history ]
    ]

historyView : List Expression -> Html a
historyView expressions = text <| String.join "" <| List.map showExpression expressions
