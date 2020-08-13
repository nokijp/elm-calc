module KeyEvents exposing (onUpDownKeyDown)

import Json.Decode as Json
import Html.Styled.Events exposing (..)
import Html.Styled exposing (..)

onUpDownKeyDown : a -> a -> Attribute a
onUpDownKeyDown upKeyMsg downKeyMsg =
  let
    toCodeDecoder =
      Json.andThen <| \code ->
        case code of
          38 -> Json.succeed upKeyMsg
          40 -> Json.succeed downKeyMsg
          _ -> Json.fail "keycode neither up nor down"
  in
    on "keydown" <| toCodeDecoder keyCode
