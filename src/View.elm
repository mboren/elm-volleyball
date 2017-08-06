module View exposing (view)

import Char
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
          , Svg.Attributes.numOctaves "1"
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
          Title maybeMenu ->
            titleView model maybeMenu Nothing model.gameStarted

          Instructions ->
            instructionsView model model.player1

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

          KeyInput side key ->
            titleView model (Just Controls) (Just (side, key)) model.gameStarted
    ]
  ]

drawAnchoredText : number -> number -> number -> String -> String -> Svg Msg
drawAnchoredText x y height textAnchor text =
    Svg.text_
      [ Svg.Attributes.x (toString x)
      , Svg.Attributes.y (toString y)
      , Svg.Attributes.style
        ( "text-anchor: " ++ textAnchor ++ "; "
        ++ "font-family: sans-serif; "
        ++ "font-size: " ++ (toString height) ++ "px; "
        ++ "alignment-baseline: before-edge")
      , Svg.Attributes.fill "black"
      ]
      [ Svg.text text
      ]

instructionsView : Layout a -> Player -> Svg Msg
instructionsView layout player =
  let
    textHeight = 30
    (playerX, playerY) = (90, 210)
    (bombX, bombY) = (770, 120)
    (explodeX, explodeY) = (770, 300)
    (controlsX, controlsY) = (10, playerY + 100)

    mainText =
      [ ( playerX + 90
        , """
          Players will hit the bomb toward the
          other side of the screen when it comes
          near them.
          """
        )
      , ( playerX + 90
        , """
          The bomb will explode when it touches
          the ground or when the time runs out.
          """
        )
      , ( playerX + 90
        , """
          ⇦ Click this to switch between AI
          and keyboard control for either
          player during a match.
          """
        )
      , ( 10
        , """
          You gain points by killing the other player, but the points don't matter
          and there is no end condition. This is probably a metaphor for war
          or something rather than sloppy game design.
          """
        )
      ]

    mainTextTspans =
      mainText
        |> List.map (Tuple.mapSecond (makeLines))
        |> List.concatMap (\(x,lines)-> (makeTspans x lines))

    makeLines : String -> List String
    makeLines str =
      str
        |> String.lines
        |> List.map (String.trim)
        |> List.filter (\s -> (String.length s) > 1)

    makeTspans : Float -> List String -> List (Svg Msg)
    makeTspans x strings =
      strings
        -- give first line of each section a bigger dy multiplier
        -- in order to space them out more
        |> List.indexedMap (\i s->(if i == 0 then 2.3 else 1.0, s))
        |> List.map (Tuple.mapFirst ((*) textHeight))
        |> List.map (uncurry (makeTspan x))

    makeTspan : Float -> Float -> String -> Svg Msg
    makeTspan x0 height text =
      Svg.tspan
        [ Svg.Attributes.x (toString x0)
        , Svg.Attributes.dy (toString height)
        ]
        [ Svg.text text ]

    leftArm = player.leftArm
    rightArm = player.rightArm
    movedPlayer =
      { player
        | position = (playerX, playerY)
        , leftArm = {  leftArm | hand = (-50, 0)}
        , rightArm = { rightArm | hand = (50, 0)}
        , fixedLegX = playerX - 30
        , freeLegX = playerX + 30
      }
  in
    Svg.g
      []
      [ svgButton 10 10 240 50 "Main menu" (GoToPage (Title Nothing))

      , drawPlayer movedPlayer
      , drawAnchoredText playerX (playerY + 50) 20 "middle" "Fig 1: you"

      , drawBomb (bombX, bombY) 50 20
      , drawAnchoredText bombX (bombY + 60) 20 "middle" "Fig 2: a bomb"

      , drawCircle (explodeX, explodeY) 80 "red"
        |> filter turbulenceId
      , drawAnchoredText explodeX (explodeY+100) 20 "middle" "Fig 3: an explosion"

      , drawControlToggle layout player Nothing (controlsX) controlsY 120 55 Left
      , drawAnchoredText (controlsX ) (controlsY+60) (textHeight-10) "start" "Fig 4: AI toggle" -- , "switch"]
      , drawAnchoredText (controlsX + 65) (controlsY+60+20) (textHeight-10) "start" "switch"

      , Svg.text_
        [ Svg.Attributes.x (toString 0)
        , Svg.Attributes.y (toString 20)
        , Svg.Attributes.style
          ( "text-anchor: start; font-family: sans-serif; "
          ++ "font-size: " ++ (toString textHeight) ++ "px; "
          ++ "alignment-baseline: before-edge")
        , Svg.Attributes.fill "black"
        ]
        mainTextTspans
      ]

pauseMenuX : Layout a -> Float
pauseMenuX {screenWidth} =
  10

parallelogramSideOffset : Float -> Float -> Int -> Float -> Int -> Int -> Float
parallelogramSideOffset width height numRows colWidth row col =
  (toFloat (numRows - row - 1)) * height / (uiSlope * (toFloat numRows)) + (toFloat col) * colWidth

parallelogramTopOffset : Float -> Int -> Int -> Float
parallelogramTopOffset height numRows row =
  (toFloat row) * height / (toFloat numRows)

pauseMenu : Layout a -> Svg Msg
pauseMenu layout =
  let
    height = 50
    padding = 10
    y i = 70 + i * (height + padding)
  in
    Svg.g
      []
      [ svgButton (pauseMenuX layout) (y 0) 140 50 "Play" TogglePause
      , svgButton (pauseMenuX layout) (y 1) 220 50 "Main menu" (GoToPage (Title Nothing))
      ]

{-
Convert a keycode into a string.

The only key codes that map directly to the correct unicode symbol
are the letter and digit keys, so for everything else, we just
show the stringified keycode.
-}
keyToString : Char.KeyCode -> String
keyToString key =
  if (key >= Char.toCode 'A' && key <= Char.toCode 'Z')
  || (key >= Char.toCode '0' && key <= Char.toCode '9')
 then
    key
      |> Char.fromCode
      |> toString
      |> String.dropLeft 1
      |> String.dropRight 1
  else
    toString key

drawControlsMenu : Float -> Float -> Float -> Float -> Float -> MovementKeys a -> MovementKeys a -> Maybe (Side, MovementKey) -> Svg Msg
drawControlsMenu screenWidth width height sideOffset topOffset p1Keys p2Keys maybeSelectedKey =
  let
    numRows = 4
    actionColWidth = width / 2
    keyColWidth = width / 4
    rowHeight = height / numRows

    calcTopOffset row =
       topOffset + parallelogramTopOffset height numRows row

    calcSideOffset colWidth row col =
      sideOffset + (parallelogramSideOffset (width) (height) numRows colWidth row col)

    subMenuCell : (Int, Int, Maybe Msg, Float, Float, String, String) -> Svg Msg
    subMenuCell (row, col, msg, width, padding, color, text) =
      let
        textView = drawCenteredText text (rowHeight*(5/6))
        cellSideOffset = calcSideOffset width row col
        cellTopOffset = calcTopOffset row
        paddedWidth = width - padding
        paddedHeight = rowHeight - padding
      in
        drawUiBlock textView msg cellSideOffset cellTopOffset paddedWidth paddedHeight color screenWidth Left

    keyCell (side, movementKey) =
      let
        keyText = keyToString keyCode
        msg = PrepareToChangePlayerKey side movementKey
        colWidth = keyColWidth
        padding = 5

        (col, playerKeys) =
          case side of
            Left ->
              (2, p1Keys)
            Right ->
              (3, p2Keys)

        (row, keyCode) =
          case movementKey of
            LeftKey ->
              (1, playerKeys.leftKey)
            RightKey ->
              (2, playerKeys.rightKey)
            JumpKey ->
              (3, playerKeys.jumpKey)

        color =
          case maybeSelectedKey of
            Nothing ->
              "gray"
            Just selectedKey ->
              if selectedKey == (side, movementKey) then
                "lightcoral"
              else
                "gray"
      in
        (row, col, Just msg, colWidth, padding, color, keyText)

    cells =
      [ (0, 2, Nothing, keyColWidth, 5, "black", "P1")
      , (0, 3, Nothing, keyColWidth, 5, "black", "P2")
      , (1, 0, Nothing, actionColWidth, 0, "black", "left")
      , (2, 0, Nothing, actionColWidth, 0, "black", "right")
      , (3, 0, Nothing, actionColWidth, 0, "black", "jump")
      , keyCell (Left, LeftKey)
      , keyCell (Left, RightKey)
      , keyCell (Left, JumpKey)
      , keyCell (Right, LeftKey)
      , keyCell (Right, RightKey)
      , keyCell (Right, JumpKey)
      ]
  in
    Svg.g
      []
      (List.map (subMenuCell) cells)

titleView : Players (Layout a) -> Maybe SubMenu -> Maybe (Side, MovementKey) -> Bool -> Svg Msg
titleView {screenWidth, player1, player2} maybeSubMenu maybeChangingKey gameStarted =
  let
    titleOffset = 60
    titleWidth = screenWidth + titleOffset - rowHeight / 2

    startOffset = 60
    rowHeight = 95
    padding = 15
    startWidth = 500

    y : Int -> Float
    y i =
      startOffset + (toFloat i) * (rowHeight + padding)

    width : Int -> Float
    width i =
      startWidth - (((y (i - 1)) - startOffset) / uiSlope)

    subMenuSideOffset =
      (width 3) - 60 + padding

    subMenuWidth =
      titleWidth - (width 1) - (rowHeight / 2) - (3/2) * padding

    subMenuHeight =
      3 * rowHeight + 2 * padding

    drawTitleScreenButton : Int -> (String, Maybe Msg) -> Svg Msg
    drawTitleScreenButton i (text, msg) =
      drawUiBlock (drawCenteredText text 80) msg (-60) (y (i + 1)) (width (i + 1)) rowHeight "black" screenWidth Left

    drawTitleScreenSubMenuBackground : String -> String -> Svg Msg
    drawTitleScreenSubMenuBackground text fillColor =
      drawUiBlock (drawCenteredText text 10) Nothing (subMenuSideOffset) (y 1) subMenuWidth subMenuHeight fillColor screenWidth Left

    buttons =
      ( case gameStarted of
          True ->
            [ ("continue", Just (GoToPage Game)) ]
          False ->
            []
      )
      ++
      [ ("new game", Just StartGame)
      , ("controls", Just (ToggleSubMenu Controls))
      , ("help", Just (GoToPage Instructions))
      ]
  in
    Svg.g
      []
      [ drawUiBlock (drawCenteredText "xtreme volleyball 2k17" 80) Nothing (-1*titleOffset) (y 0) (titleWidth) 95 "gray" screenWidth Left
      , Svg.g
        []
        (List.indexedMap (drawTitleScreenButton) buttons)
      , case maybeSubMenu of
          Nothing ->
            drawBomb (2 * screenWidth / 3, 350) 120 30

          Just Controls ->
            Svg.g
              []
              [ drawTitleScreenSubMenuBackground "" "black"
              , drawControlsMenu screenWidth (subMenuWidth - padding) (subMenuHeight - padding) (subMenuSideOffset + (padding / uiSlope)) (y 1) player1 player2 maybeChangingKey
              ]

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

    , svgButton (pauseMenuX model) 70 140 50 "Pause" TogglePause
    , drawScore model
    , drawTimer model.ball.countdown (0.5 * model.screenWidth) 0 80
    , drawUiBlock (drawCenteredText "" 0) Nothing (190) 0 135 60 "gray" model.screenWidth Left
    , drawUiBlock (drawCenteredText "" 0) Nothing (190) 0 135 60 "gray" model.screenWidth Right
    , drawUiBlock (drawCenteredText "Player 1" (60*5/6)) Nothing (-60/2) 0 220 60 "black" model.screenWidth Left
    , drawUiBlock (drawCenteredText "Player 2" (60*5/6)) Nothing (-60/2) 0 220 60 "black" model.screenWidth Right
    , drawControlToggle model model.player1 (Just TogglePlayer1Ai) 200 0 120 55 Left
    , drawControlToggle model model.player2 (Just TogglePlayer2Ai) 200 0 120 55 Right
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

drawArm : String -> Side -> Float2 -> Arm -> Svg Msg
drawArm strokeColor side playerPosition arm  =
  let
    radius = 50
    (sx, sy) = arm.shoulder
    (hx, hy) = arm.hand
    (px, py) = playerPosition

    sweep =
      case side of
        Left ->
          1
        Right ->
          0

    armPath =
      [ ("M", [px, py])
      , ("m", [sx, sy])
      , ("a", [radius, 2*radius, 1, 0, sweep, hx, hy])
      ]
  in
    Svg.path
      [ Svg.Attributes.d (pathString armPath)
      , Svg.Attributes.stroke strokeColor
      , Svg.Attributes.strokeLinecap "round"
      , Svg.Attributes.fillOpacity "0.0"
      , Svg.Attributes.strokeWidth "20"
      ]
      []

drawLegs : String -> Player -> Svg Msg
drawLegs strokeColor player =
  let
    radius = 50

    -- sweep is an svg arc parameter that affects curvature.
    -- 0 makes legs bend like an open paren: '('
    -- 1 makes legs bend like a close paren: ')'
    sweep =
      if V2.getX player.velocity < 0 then
        0 -- player is moving left
      else
        1 -- player is moving right

    (px, py) = player.position

    footY = py + player.waistY + player.legHeight

    moveToWaist = ("M", [px, py + player.waistY])

    legPath =
      [ moveToWaist
      , ("A", [radius, radius, 1, 0, sweep, player.fixedLegX, footY])
      , moveToWaist
      , ("A", [radius, radius, 1, 0, sweep, player.freeLegX, footY])
      ]
  in
    Svg.path
      [ Svg.Attributes.d (pathString legPath)
      , Svg.Attributes.stroke strokeColor
      , Svg.Attributes.fillOpacity "0.0"
      , Svg.Attributes.strokeWidth "20"
      , Svg.Attributes.strokeLinecap "round"
      ]
      []

drawPlayer : Player -> Svg Msg
drawPlayer player =
  let
    fillColor =
      case player.alive of
        True ->
          "green"
        False ->
          "blue"
    (px, py) = player.position
    torsoY = py - 50
    torsoHeight = 70
    headRadius = 25
    headY = -0.8*headRadius + torsoY - torsoHeight / 2
  in
    Svg.g
      []
      [ drawCircle (px, headY) headRadius fillColor
      , drawLegs fillColor player
      , drawArm fillColor Left player.position player.leftArm
      , drawArm fillColor Right player.position player.rightArm
      , Svg.ellipse
        [ Svg.Attributes.cx (toString (px))
        , Svg.Attributes.cy (toString torsoY)
        , Svg.Attributes.rx (toString (player.size))
        , Svg.Attributes.ry (toString (torsoHeight/2))
        , Svg.Attributes.fill fillColor
        ]
        []
      ]

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

{-
Stringify everything and stick it all together in a string
that we can use for attribute "d" of Svg.path
example:
    [(M,50,50),(L,100,100)] -> "M50 50 L100 100"
-}
pathString : List (String, List number) -> String
pathString list =
  list
    |> List.map (Tuple.mapSecond (List.map (toString)))
    |> List.map (Tuple.mapSecond (String.join " "))
    |> List.map (uncurry (++))
    |> String.join " "

drawBomb : Float2 -> Float -> Float -> Svg Msg
drawBomb position size rotation =
  let
    (x, y) = position

    stemW = size
    stemH = 0.7 * size
    stemX =  -0.5 * stemW
    stemY = -1 * (size + stemH / 2)

    wickPath =
      [ ("M", [0, stemY])
      , ("a", [size, 2*size, 0, 0, 1, size, -10])
      ]

    transform =
      "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"
      ++ " rotate(" ++ (toString rotation) ++ ")"
  in
    Svg.g
      [ Svg.Attributes.transform transform ]
      [ drawCircle (0,0) size "black"
      , Svg.rect
        [ Svg.Attributes.width (toString stemW)
        , Svg.Attributes.height (toString stemH)
        , Svg.Attributes.fill "black"
        , Svg.Attributes.x (toString stemX)
        , Svg.Attributes.y (toString stemY)
        ]
        []
      , Svg.path
        [ Svg.Attributes.d (pathString wickPath)
        , Svg.Attributes.stroke "chocolate"
        , Svg.Attributes.fillOpacity "0.0"
        , Svg.Attributes.strokeWidth "3"
        ]
        []
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
      let
        -- degrees per horizontal distance unit
        angularSpeed = 3 * 360 / 1000

        -- Angle is determined completely by the X value.
        -- This looks pretty convincing. It appears to change rotational
        -- direction when it bounces, and rotation appears to slow down
        -- or speed up as the ball itself does.
        angle = (V2.getX position) * angularSpeed
      in
        drawBomb position size angle

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

drawControlToggle : Layout a -> MovementKeys { b | ai : Bool} -> Maybe Msg -> Float -> Float -> Float -> Float -> Side -> Svg Msg
drawControlToggle layout player clickEvent sideOffset topOffset w h side =
  let
    labelX = sideOffset + (h / 2) / uiSlope

    aiFill = getToggleColor player.ai
    keyboardFill = getToggleColor (not player.ai)

    drawKeyboardControls = (drawControls player (h/2))
  in
    Svg.g
      []
      [ drawUiBlock (drawCenteredText "Controls" (h/2-5)) Nothing  labelX topOffset w (h/2) "gray" layout.screenWidth side
      , drawUiBlock (drawCenteredText "AI" (h/2-5)) (clickEvent) sideOffset (topOffset + h/2) (w/2) (h/2) aiFill layout.screenWidth side
      , drawUiBlock (drawKeyboardControls) (clickEvent) (sideOffset + w/2) (topOffset + h/2) (w/2) (h/2) keyboardFill layout.screenWidth side
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
drawControls : MovementKeys a -> Float -> Float -> Float -> Svg Msg
drawControls {leftKey, rightKey, jumpKey} h x y =
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
      [ Svg.text (keyToString jumpKey)
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
      [ Svg.text ((keyToString leftKey) ++ " " ++ (keyToString rightKey))
      ]
    ]
