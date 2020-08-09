module View exposing (bodyContent)

import Css exposing (..)
import Css.Reset as Reset
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, value)
import Html.Styled.Events exposing (..)
import Model exposing (..)

bodyContent : Model -> List (Html Msg)
bodyContent model =
  let
    pageWidth = px 1000
    primaryColor = rgb 0x33 0x99 0xff
    accentColor = rgb 0xee 0xaa 0x00
    primaryTextColor = rgb 0x55 0x55 0x55
    secondaryTextColor = rgb 0x99 0x99 0x99
    placeholderTextColor = rgb 0xdd 0xdd 0xdd

    headerSectionStyle =
      css
        [ margin3 (rem 6.0) auto (rem 5.0)
        , width pageWidth
        , color primaryTextColor
        ]
    mainTitleStyle =
      css
        [ paddingBottom (rem 0.4)
        , textTransform lowercase
        , fontSize (rem 1.5)
        ]

    mainTextInputStyle =
      css
        [ display block
        , boxSizing borderBox
        , padding2 (rem 0.3) (rem 0.8)
        , width (pct 100)
        , outline zero
        , border3 (px 3.0) solid primaryColor
        , borderRadius (rem 0.3)
        , color inherit
        , fontSize (rem 2.0)
        , property "-webkit-appearance" "none"
        , pseudoElement "placeholder"
            [ color placeholderTextColor
            , opacity (num 1.0)
            ]
        ]
    messageStyle =
      css
        [ position absolute
        , paddingTop (rem 0.4)
        , color secondaryTextColor
        ]

    historySectionStyle =
      css
        [ margin2 zero auto
        , padding2 (rem 1.0) zero
        , width pageWidth
        , color primaryTextColor
        ]
    historySectionHeaderStyle =
      css
        [ displayFlex
        , alignItems center
        ]
    historySectionTitleStyle =
      css
        [ color secondaryTextColor
        , fontSize (rem 1.2)
        ]
    historyClearButtonStyle =
      css
        [ if List.isEmpty model.history then visibility hidden else visibility visible
        , marginLeft (rem 0.5)
        , padding2 (rem 0.1) (rem 0.5)
        , outline zero
        , border3 (px 1.0) solid secondaryTextColor
        , borderRadius (rem 0.1)
        , backgroundColor transparent
        , color secondaryTextColor
        , fontSize (rem 0.8)
        , cursor pointer
        , property "-webkit-appearance" "none"
        , hover
            [ backgroundColor secondaryTextColor
            , color (rgb 0xff 0xff 0xff)
            ]
        ]
    historyListContainerStyle =
      css
        [ fontSize (rem 1.5)
        ]
    historyListItemStyle =
      css
        [ displayFlex
        , alignItems center
        , margin3 (rem 1.0) zero (rem 1.5)
        ]
    variableLabelStyle =
      css
        [ display block
        , marginRight (rem 0.5)
        , height (rem 1.1)
        , width (rem 3.5)
        , lineHeight (rem 1.1)
        , borderRadius (rem 0.1)
        , backgroundColor accentColor
        , textAlign center
        , color (rgb 0xff 0xff 0xff)
        , fontSize (rem 0.8)
        ]
    emptyHistoryMessageStyle =
      css
        [ margin2 (rem 4.0) zero
        , textAlign center
        , color placeholderTextColor
        , fontSize (rem 2.0)
        ]

    message =
      case model.result of
         CalcResultEmpty -> ""
         CalcResultErr e -> e
         CalcResultOk r -> "= " ++ String.fromFloat r
  in
    [ Reset.meyerV2
    , div [headerSectionStyle]
        [ h1 [mainTitleStyle] [text "Elm Calc"]
        , form [onSubmit TryToPush]
          [ input
              [ mainTextInputStyle
              , onInput UpdateInput
              , value model.input
              , placeholder "enter an expression, e.g. (1 + 2 * sin(5 * pi)) / 2"
              ]
              []
          ]
        , p [messageStyle] [text message]
        ]
    , section [historySectionStyle]
        [ div [historySectionHeaderStyle]
            [ h1 [historySectionTitleStyle] [text "History"]
            , button [historyClearButtonStyle, onClick ClearHistory] [text "Clear"] 
            ]
        , if List.isEmpty model.history
          then
            p [emptyHistoryMessageStyle] [text "hit the enter key to push an expression to here"]
          else
            ul [historyListContainerStyle] <|
              let
                historyItem (expr, res, var) =
                  li [historyListItemStyle]
                    [ span [variableLabelStyle] [text var]
                    , text <| expr ++ " = " ++ String.fromFloat res
                    ]
              in List.map historyItem model.history
        ]
    ]
