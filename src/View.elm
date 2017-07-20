module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes

import Types exposing (..)

view : Model -> Svg Msg
view model =
  div
    [ Html.Attributes.style
      [ ("vertical-align", "top")
      , ("overflow", "hidden")
      ]
    ]
    [ Svg.svg
      [ Svg.Attributes.viewBox
        ( "0 0 "
        ++ (toString model.screenWidth)
        ++ " "
        ++ (toString model.screenHeight)
        )
      , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
      , Html.Attributes.style
        [ ("display", "inline-block")
        , ("position", "absolute")
        , ("top", "0")
        , ("left", "0")
        , ("max-height", "100%")
        , ("user-select", "none")
        ]
      ]
      [ Svg.rect
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.fill "lightskyblue"
        ]
        []
      , drawNet model
      ]
    ]

drawNet : Model -> Svg Msg
drawNet {screenWidth, screenHeight, netWidth, netHeight} =
  Svg.rect
    [ Svg.Attributes.x (toString ((screenWidth // 2) - (netWidth // 2)))
    , Svg.Attributes.y (toString (screenHeight - netHeight))
    , Svg.Attributes.width (toString netWidth)
    , Svg.Attributes.height (toString netHeight)
    , Svg.Attributes.fill "black"
    ]
    []
