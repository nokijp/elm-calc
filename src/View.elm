module View exposing (bodyContent)

import Css exposing (..)
import Css.Reset as Reset
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder)
import Html.Styled.Events exposing (..)
import Model exposing (..)

bodyContent : Model -> List (Html Msg)
bodyContent model =
  let
    headerOuterStyle =
      css
        [ padding2 (rem 3.0) zero
        , backgroundColor (rgb 0x5f 0xab 0xdc)
        , color (rgb 0xff 0xff 0xff)
        ]
    headerInnerStyle =
      css
        [ margin2 zero auto
        , width (px 1000)
        ]
    formSection content = div [headerOuterStyle] [div [headerInnerStyle] content]

    mainTitleStyle =
      css
        [ paddingBottom (rem 0.2)
        , textTransform lowercase
        , fontSize (rem 1.5)
        ]
    mainTitle = h1 [mainTitleStyle] [text "Elm Calc"]

    mainTextInputStyle =
      css
        [ boxSizing borderBox
        , padding2 (rem 0.3) (rem 0.8)
        , width (pct 100)
        , outline zero
        , border zero
        , borderRadius (rem 0.2)
        , backgroundColor (rgb 0x99 0xc7 0xe9)
        , color (rgb 0xff 0xff 0xff)
        , fontSize (rem 2.0)
        , property "-webkit-appearance" "none"
        , pseudoElement "placeholder"
            [ opacity (num 1.0)
            ]
        , focus
            [ backgroundColor (rgb 0xff 0xff 0xff)
            , color (rgb 0x33 0x33 0x33)
            , pseudoElement "placeholder"
                [ opacity (num 0.5)
                ]
            ]
        ]
    messageStyle =
      css
        [ position absolute
        , paddingTop (rem 0.2)
        , color (rgb 0xbd 0xdf 0xff)
        ]

    historySectionStyle =
      css
        [ margin2 zero auto
        , padding2 (rem 1.0) zero
        , width (px 1000)
        , color (rgb 0x33 0x33 0x33)
        ]
    historySectionHeaderStyle =
      css
        [ displayFlex
        ]
    historySectionTitleStyle =
      css
        [ color (rgb 0x99 0x99 0x99)
        , fontSize (rem 1.0)
        ]
    historyClearButtonStyle =
      css
        [ marginLeft auto
        , padding2 (rem 0.1) (rem 0.5)
        , outline zero
        , border3 (px 1.0) solid (rgb 0x99 0x99 0x99)
        , borderRadius (rem 0.1)
        , backgroundColor transparent
        , color (rgb 0x99 0x99 0x99)
        , fontSize (rem 0.8)
        , property "-webkit-appearance" "none"
        , hover
            [ backgroundColor (rgb 0x99 0x99 0x99)
            , color (rgb 0xff 0xff 0xff)
            ]
        ]
    historyListContainerStyle =
      css
        [ fontSize (rem 1.5)
        ]
    historyListItemStyle =
      css
        [ display block
        , margin3 (rem 0.8) zero (rem 2.0)
        ]

    message =
      case model.newExpression of
         ExpressionEmpty -> ""
         ExpressionErr e -> e
         ExpressionOk _ _ r -> "= " ++ String.fromFloat r
  in
    [ Reset.meyerV2
    , formSection
        [ mainTitle
        , form [onSubmit TryToPush]
          [ input [mainTextInputStyle, onInput UpdateNewExpression, placeholder "enter an expression, e.g. (1 + 2 * sin(5 * pi)) / 2"] []
          ]
        , p [messageStyle] [text message]
        ]
    , section [historySectionStyle]
        [ div [historySectionHeaderStyle]
            [ h1 [historySectionTitleStyle] [text "History"]
            , button [historyClearButtonStyle, onClick ClearHistory] [text "Clear"] 
            ]
        , ul [historyListContainerStyle] <|
            List.map (\(s, res) -> li [historyListItemStyle] [text <| s ++ " = " ++ String.fromFloat res]) model.history
        ]
    ]
