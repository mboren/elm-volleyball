module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time exposing (Time)

import Vector2 as V2 exposing (Vec2, Float2)

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
      , drawPlayer model.player1
      , drawPlayer model.player2
      , drawBall model.ball
      , svgButton 30 30 130 50 "reset" Reset
      , svgButton 30 90 200 30 "Toggle Player1 AI" TogglePlayer1Ai
      , svgButton 30 130 200 30 "Toggle Player2 AI" TogglePlayer2Ai
      , drawScore model
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

drawPlayer : Player -> Svg Msg
drawPlayer {position, size, alive} =
  let
    fillColor =
      case alive of
        True ->
          "green"
        False ->
          "blue"
  in
  Svg.circle
    [ Svg.Attributes.cx (toString (V2.getX position))
    , Svg.Attributes.cy (toString (V2.getY position))
    , Svg.Attributes.r (toString size)
    , Svg.Attributes.fill fillColor
    ]
    []

drawBall : Explosive (Mover a) -> Svg Msg
drawBall {position, size, status, explosionRadius} =
  let
    (fill, radius) =
      case status of
        Exploded ->
          ("orange", explosionRadius)
        Exploding ->
          ("red", explosionRadius)
        Safe ->
          ("black", size)
  in
    Svg.circle
      [ Svg.Attributes.cx (toString (V2.getX position))
      , Svg.Attributes.cy (toString (V2.getY position))
      , Svg.Attributes.r (toString radius)
      , Svg.Attributes.fill fill
      ]
      []

svgButton : number -> number -> Int -> Int -> String -> Msg -> Svg Msg
svgButton x y w h text onClickEvent =
  let
    transform = "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"
  in
    Svg.g
      [ Svg.Attributes.transform transform
      , Svg.Attributes.cursor "pointer"
      , Svg.Events.onClick onClickEvent
      ]
      [ Svg.rect
        [ Svg.Attributes.width (toString w)
        , Svg.Attributes.height (toString h)
        , Svg.Attributes.fill "black"
        ]
        []
      , Svg.text_
        [ Svg.Attributes.x (toString ((toFloat w)/2.0))
        , Svg.Attributes.y (toString ((toFloat h)/2.0))
        , Svg.Attributes.style
          ( "text-anchor: middle; font-family: sans-serif; font-size: "
          ++ (toString (h - 5))
          ++ "px; alignment-baseline: middle")
        , Svg.Attributes.fill "white"
        ]
        [ Svg.text text
        ]
      ]

drawScore : Model -> Svg Msg
drawScore {player1, player2, screenWidth} =
  let
    size = 50
    middle = screenWidth // 2
    offset = 20
  in
    Svg.g
      [
      ]
      [ Svg.text_
        [ Svg.Attributes.x (toString (middle - offset))
        , Svg.Attributes.y "50"
        , Svg.Attributes.style
          ( "text-anchor: end; font-family: sans-serif; font-size: "
          ++ (toString (size))
          ++ "px; alignment-baseline: middle")
        , Svg.Attributes.fill "white"
        ]
        [ Svg.text (toString player1.score)
        ]
      , Svg.text_
        [ Svg.Attributes.x (toString (middle + offset))
        , Svg.Attributes.y "50"
        , Svg.Attributes.style
          ( "text-anchor: start; font-family: sans-serif; font-size: "
          ++ (toString (size))
          ++ "px; alignment-baseline: middle")
        , Svg.Attributes.fill "white"
        ]
        [ Svg.text (toString player2.score)
        ]
      ]
