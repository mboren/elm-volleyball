module View exposing (view)

import Html exposing (Html, div)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time exposing (Time)

import Vector2 as V2 exposing (Vec2, Float2)

import Types exposing (..)

uiSlope = 2
pauseBlurId = "pauseBlur"
turbulenceId = "turbulenceFilter"


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
      [ Svg.filter
        [ Svg.Attributes.id pauseBlurId ]
        [ Svg.feGaussianBlur
          [ Svg.Attributes.in_ "SourceGraphic"
          , Svg.Attributes.stdDeviation "2"
          ]
          []
        ]
      , Svg.filter
        [ Svg.Attributes.id turbulenceId ]
        [ Svg.feTurbulence
          [ Svg.Attributes.type_ "turbulence"
          , Svg.Attributes.baseFrequency "0.05"
          , Svg.Attributes.numOctaves "2"
          , Svg.Attributes.result "turbulence"
          ]
          []
        , Svg.feDisplacementMap
          [ Svg.Attributes.in2 "turbulence"
          , Svg.Attributes.in_ "SourceGraphic"
          , Svg.Attributes.scale "50"
          , Svg.Attributes.xChannelSelector "R"
          , Svg.Attributes.yChannelSelector "G"
          ]
          []
        ]
      , case model.page of
          Title ->
            titleView model
          Game ->
            if model.paused then
              Svg.g
                []
                [ gameView model
                  |> filter pauseBlurId
                , pauseMenu model
                ]
            else
              gameView model

      ]
    ]

pauseMenuX : Layout a -> Float
pauseMenuX {screenWidth} =
  0.5 * screenWidth - 70

pauseMenu : Layout a -> Svg Msg
pauseMenu layout =
  Svg.g
    []
    [ svgButton (pauseMenuX layout) 80 140 50 "Play" TogglePause
    ]

titleView : Layout a -> Svg Msg
titleView {screenWidth} =
  Svg.g
    []
    [ drawUiBlock (drawCenteredText "xtreme volleyball 2k17" 80) Nothing (-60) (60) 900 95 "gray" screenWidth Left
    , drawUiBlock (drawCenteredText "play" 80) (Just StartGame) (-60) (170) 250 95 "black" screenWidth Left
    ]

filter : String -> Svg Msg -> Svg Msg
filter filterId svg =
  Svg.g
    [ Svg.Attributes.filter ("url(#" ++ filterId ++ ")")
    ]
    [ svg
    ]

gameView : Model -> Svg Msg
gameView model =
  Svg.g
    []
    [ Svg.rect
      [ Svg.Attributes.width "100%"
      , Svg.Attributes.height "100%"
      , Svg.Attributes.fill "lightskyblue"
      ]
      []
    , drawNet model
    , drawPlayer model.player1
    , drawPlayer model.player2
    , if model.warmupTimer > 0 then
        drawTimer (model.warmupTimer + Time.second) (0.5 * model.screenWidth) 140 120
      else
        drawBall model.ball

    , svgButton (pauseMenuX model) 80 140 50 "Pause" TogglePause
    , drawScore model
    , drawTimer model.ball.countdown (0.5 * model.screenWidth) 0 80
    , drawUiBlock (drawCenteredText "" 0) Nothing (190) 0 135 60 "gray" model.screenWidth Left
    , drawUiBlock (drawCenteredText "" 0) Nothing (190) 0 135 60 "gray" model.screenWidth Right
    , drawUiBlock (drawCenteredText "Player 1" (60*5/6)) Nothing (-60/2) 0 220 60 "black" model.screenWidth Left
    , drawUiBlock (drawCenteredText "Player 2" (60*5/6)) Nothing (-60/2) 0 220 60 "black" model.screenWidth Right
    , drawControlToggle model "S" "E" "F" 200 0 120 55 Left
    , drawControlToggle model "J" "I" "L" 200 0 120 55 Right
    ]

drawNet : Layout a -> Svg Msg
drawNet {screenWidth, screenHeight, netWidth, netHeight} =
  Svg.rect
    [ Svg.Attributes.x (toString ((screenWidth - netWidth) / 2))
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

drawTimer : Time -> Float -> Float -> Float -> Svg Msg
drawTimer time x y height =
  Svg.text_
    [ Svg.Attributes.x (toString x)
    , Svg.Attributes.y (toString y)
    , Svg.Attributes.style
      ( "text-anchor: middle; font-family: sans-serif; font-size: "
      ++ (toString height)
      ++ "px; alignment-baseline: before-edge")
    , Svg.Attributes.fill "white"
    ]
    [ Svg.text (toString (floor (Time.inSeconds time)))
    ]

drawBall : Explosive (Mover a) -> Svg Msg
drawBall {position, size, status} =
  case status of
    Exploded ->
      Svg.g [] []
    Exploding ->
      drawCircle position size "red"
        |> filter turbulenceId
    Safe ->
      drawCircle position size "black"

drawCircle : Float2 -> Float -> String -> Svg Msg
drawCircle position radius color =
  Svg.circle
    [ Svg.Attributes.cx (toString (V2.getX position))
    , Svg.Attributes.cy (toString (V2.getY position))
    , Svg.Attributes.r (toString radius)
    , Svg.Attributes.fill color
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

drawScore : Layout (Players a) -> Svg Msg
drawScore {player1, player2, screenWidth} =
  let
    size = 60
    offset = 324
  in
    Svg.g
      [
      ]
      [ drawUiBlock (drawCenteredText (toString player1.score) 60) Nothing offset 0 90 60 "lightcoral" screenWidth Left
      , drawUiBlock (drawCenteredText (toString player2.score) 60) Nothing offset 0 90 60 "lightcoral" screenWidth Right
      ]

drawControlToggle : Layout (Players a) -> String -> String -> String -> Float -> Float -> Float -> Float -> Side -> Svg Msg
drawControlToggle model leftKey jumpKey rightKey sideOffset topOffset w h side =
  let
    labelX = sideOffset + (h / 2) / uiSlope

    (ai, aiToggleMsg) =
      case side of
        Left ->
          (model.player1.ai, TogglePlayer1Ai)
        Right ->
          (model.player2.ai, TogglePlayer2Ai)

    aiFill = getToggleColor ai
    keyboardFill = getToggleColor (not ai)

    drawKeyboardControls = (drawControls leftKey jumpKey rightKey (h/2))
  in
    Svg.g
      []
      [ drawUiBlock (drawCenteredText "Controls" (h/2-5)) Nothing  labelX topOffset w (h/2) "gray" model.screenWidth side
      , drawUiBlock (drawCenteredText "AI" (h/2-5)) (Just aiToggleMsg) sideOffset (topOffset + h/2) (w/2) (h/2) aiFill model.screenWidth side
      , drawUiBlock (drawKeyboardControls) (Just aiToggleMsg) (sideOffset + w/2) (topOffset + h/2) (w/2) (h/2) keyboardFill model.screenWidth side
      ]

getToggleColor : Bool -> String
getToggleColor selected =
  if selected then
    "darkslategray"
  else
    "lightskyblue"

{-
Convert list of ordered pairs into a string suitable for Svg.Attributes.points
-}
pointsListToString : List (number, number) -> String
pointsListToString list =
  list
    |> List.map (\(x,y)->(toString x) ++ " " ++ (toString y))
    |> String.join ", "

parallelogramPoints : Float -> Float -> Float -> Float -> List (Float, Float)
parallelogramPoints x y w h =
  let
    xoffset = h / uiSlope
  in
    [ (x, y + h)
    , (x + w, y + h)
    , (x + w + xoffset, y)
    , (x + xoffset, y)
    ]

{-
Draws a parallelogram, and takes a callback function to draw its contents.
Most of the UI is made up of these blocks.
-}
drawUiBlock : (Float -> Float -> Svg Msg) -> Maybe Msg -> Float -> Float -> Float -> Float -> String -> Float -> Side -> Svg Msg
drawUiBlock contents clickEvent sideOffset topOffset baseWidth height fill screenWidth side =
  let
    points =
      parallelogramPoints sideOffset topOffset baseWidth height
        -- cut off side if it hangs off the screen
        |> List.map (\(x,y)->(max 0 x, y))
        |> pointsListToString

    midpointOffset = sideOffset + (baseWidth + height / uiSlope) / 2
    midpointX =
      case side of
        Left ->
          midpointOffset
        Right ->
          screenWidth - midpointOffset

    -- mirror the background polygon if we're on the right
    transform =
      case side of
        Left ->
          Svg.Attributes.transform ""
        Right ->
          Svg.Attributes.transform
            ( "translate("
            ++ toString screenWidth
            ++ ",0) scale(-1,1)"
            )
  in
    Svg.g
      ( case clickEvent of
         Nothing ->
           []
         Just event ->
           [ Svg.Events.onClick event
           , Svg.Attributes.cursor "pointer"
           ]
      )
      [ Svg.polygon
        [ Svg.Attributes.points points
        , Svg.Attributes.fill fill
        , transform
        ]
        []
      , contents midpointX topOffset
      ]

drawCenteredText : String -> number -> Float -> Float -> Svg Msg
drawCenteredText text size x y =
  Svg.text_
    [ Svg.Attributes.x (toString x)
    , Svg.Attributes.y (toString y)
    , Svg.Attributes.style
      ( "text-anchor: middle; font-family: sans-serif; font-size: "
      ++ (toString size)
      ++ "px; alignment-baseline: before-edge")
    , Svg.Attributes.fill "white"
    ]
    [ Svg.text text
    ]

{-
Compact display of movement keys that is horizontally and vertically
centered on (x,y).

looks like this:
     W
    A D
-}
drawControls : String -> String -> String -> Float -> Float -> Float -> Svg Msg
drawControls leftKey jumpKey rightKey h x y =
  Svg.g
    []
    [ Svg.text_
      [ Svg.Attributes.x (toString x)
      , Svg.Attributes.y (toString (y+h/2))
      , Svg.Attributes.style
        ( "text-anchor: middle; font-family: monospace; font-size: "
        ++ (toString ((h-4)/2))
        ++ "px; alignment-baseline: after-edge")
      , Svg.Attributes.fill "white"
      ]
      [ Svg.text jumpKey
      ]
    , Svg.text_
      [ Svg.Attributes.x (toString x)
      , Svg.Attributes.y (toString (y+h/2))
      , Svg.Attributes.style
        ( "text-anchor: middle; font-family: monospace; font-size: "
        ++ (toString ((h-4)/2))
        ++ "px; alignment-baseline: before-edge")
      , Svg.Attributes.fill "white"
      ]
      [ Svg.text (leftKey ++ " " ++ rightKey)
      ]
    ]
