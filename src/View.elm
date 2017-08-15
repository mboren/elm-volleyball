module View exposing (view)

import Char
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Grid exposing (Grid, insert, markAsStartCol, nextRow, nextSection, setHeight, setWidth)
import Html exposing (Html, div)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time exposing (Time)
import Types exposing (..)
import Vector2 as V2 exposing (Float2, Vec2)


uiSlope =
    2


pauseBlurId =
    "pauseBlur"


turbulenceId =
    "turbulenceFilter"


explosionGradientId =
    "explosionGradient"


explosionGradientFill =
    "url(#" ++ explosionGradientId ++ ")"



-- named CSS colors


gray =
    Color.rgb 128 128 128


green =
    Color.rgb 0 128 0


lightSkyBlue =
    Color.rgb 135 206 250


lightCoral =
    Color.rgb 240 128 128


chocolate =
    Color.rgb 210 105 30


darkSlateGray =
    Color.rgb 47 79 79


uiColor =
    { text = Color.white
    , sky = lightSkyBlue
    , player = green
    , dead = Color.black
    , menuTextBackground = Color.black
    , hudSecondaryBackground = gray
    , hudTertiaryBackground = lightCoral
    , titleBackground = gray
    , bomb = Color.black
    , wick = chocolate
    , net = Color.black
    , toggleSelected = darkSlateGray
    , toggleNotSelected = lightSkyBlue
    }


view : Model -> Svg Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "vertical-align", "top" )
            , ( "overflow", "hidden" )
            ]
        ]
        [ Svg.svg
            [ Svg.Attributes.viewBox
                ("0 0 "
                    ++ toString model.screenWidth
                    ++ " "
                    ++ toString model.screenHeight
                )
            , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
            , Html.Attributes.style
                [ ( "display", "inline-block" )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "max-height", "100%" )
                , ( "user-select", "none" )
                ]
            ]
            [ Svg.defs
                []
                [ Svg.radialGradient
                    [ Svg.Attributes.id explosionGradientId ]
                    [ Svg.stop
                        [ Svg.Attributes.offset "0%"
                        , Svg.Attributes.stopColor "white"
                        ]
                        []
                    , Svg.stop
                        [ Svg.Attributes.offset "70%"
                        , Svg.Attributes.stopColor "yellow"
                        ]
                        []
                    , Svg.stop
                        [ Svg.Attributes.offset "90%"
                        , Svg.Attributes.stopColor "red"
                        ]
                        []
                    , Svg.stop
                        [ Svg.Attributes.offset "100%"
                        , Svg.Attributes.stopColor "red"
                        , Svg.Attributes.stopOpacity "0.1"
                        ]
                        []
                    ]
                ]
            , Svg.filter
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
                Title ->
                    titleView model model.gameStarted

                Instructions ->
                    instructionsView model model.player1

                Game ->
                    if model.paused then
                        Svg.g
                            []
                            [ gameView model
                                |> filter pauseBlurId
                            , pauseMenu
                            ]
                    else
                        gameView model

                Options maybeChangingKey ->
                    optionsView model maybeChangingKey
            ]
        ]


optionsView : Model -> Maybe ( Side, MovementKey ) -> Svg Msg
optionsView model maybeChangingKey =
    let
        getUiSettingState : Side -> MovementKey -> UiSettingState
        getUiSettingState side key =
            case maybeChangingKey of
                Nothing ->
                    NotSelected

                Just ( selectedSide, selectedKey ) ->
                    if side == selectedSide && key == selectedKey then
                        Selected
                    else
                        NotSelected

        boolToUiSettingState bool =
            case bool of
                True ->
                    Selected

                False ->
                    NotSelected

        graphicsButtons =
            [ Fast, Fancy ]
                |> List.map
                    (\qs ->
                        ( qualitySettingToString qs
                        , boolToUiSettingState (qs == model.graphicsQuality)
                        , Just (ChangeSetting (SetQuality qs))
                        )
                    )

        config =
            { rows = 24
            , cols = 12
            , rowPadding = 0
            , width = model.screenWidth
            , height = model.screenHeight
            , xOffset = 0
            , yOffset = 0
            }

        newGrid =
            Grid.create config
                -- page header
                |> setWidth 10
                |> setHeight 4
                |> insert ( "Options", Label, Nothing )
                |> nextSection
                -- controls
                |> createPlayerRow model.player1 (getUiSettingState Left) (PrepareToChangePlayerKey Left)
                |> nextSection
                |> createPlayerRow model.player2 (getUiSettingState Right) (PrepareToChangePlayerKey Right)
                |> nextSection
                -- graphical quality
                |> createToggleRow "Quality" graphicsButtons
                |> nextSection
                -- back button
                |> setHeight 4
                |> setWidth 6
                |> insert ( "Back", Label, Just (GoToPage Title) )
                -- Note about key codes
                |> markAsStartCol
                |> setHeight 1
                |> setWidth 7
                |> insert ( "Note: non-alphanumeric keys will show raw key code, but will work fine.", NotSelected, Nothing )
                |> nextRow
                |> insert ( "Nobody has made a comprehensive keycode->string function for Elm yet.", NotSelected, Nothing )
                |> nextRow
                |> insert ( "I might take a stab at it after I wrap up more interesting features.", NotSelected, Nothing )
                -- change UiSettingStates to Colors
                |> Grid.map (\( text, state, msg ) -> ( text, cellColor state, msg ))
    in
    Svg.g
        [ Svg.Attributes.stroke "white" ]
        (List.map (drawRegion newGrid.config Left) newGrid.data)


createPlayerRow : Player -> (MovementKey -> UiSettingState) -> (MovementKey -> Msg) -> Grid GridData -> Grid GridData
createPlayerRow player getState makeMsg grid =
    grid
        |> setWidth 6
        |> setHeight 4
        |> insert ( player.name, Label, Nothing )
        |> markAsStartCol
        |> setHeight 2
        |> insert ( "Jump: " ++ keyToString player.jumpKey, getState JumpKey, Just (makeMsg JumpKey) )
        |> nextRow
        |> setWidth 3
        |> insert ( "Left: " ++ keyToString player.leftKey, getState LeftKey, Just (makeMsg LeftKey) )
        |> insert ( "Right: " ++ keyToString player.rightKey, getState RightKey, Just (makeMsg RightKey) )
        |> Grid.resetStartCol


createToggleRow : String -> List GridData -> Grid GridData -> Grid GridData
createToggleRow name buttons grid =
    let
        gridWithLabel =
            grid
                |> setWidth 6
                |> setHeight 4
                |> insert ( name, Label, Nothing )
                |> setWidth ((grid.config.cols - 6) // List.length buttons)
    in
    List.foldl insert gridWithLabel buttons


cellColor : UiSettingState -> Color
cellColor setting =
    case setting of
        Label ->
            uiColor.menuTextBackground

        Selected ->
            uiColor.hudTertiaryBackground

        NotSelected ->
            uiColor.hudSecondaryBackground


drawRegion : Grid.Config -> Side -> ( Grid.Region, ( String, Color, Maybe Msg ) ) -> Svg Msg
drawRegion cf side ( region, ( text, state, maybeMsg ) ) =
    let
        skewed =
            region |> Grid.regionToPath cf |> Grid.skewPath -uiSlope

        ( midpointX, textY ) =
            Grid.centroid skewed

        points =
            polygonPoints skewed

        ( textX, transform ) =
            case side of
                Left ->
                    ( midpointX, "" )

                Right ->
                    -- mirror the background polygon and shift text
                    Debug.log "hardcoded screenWidth!"
                        ( 1000 - midpointX
                        , "translate("
                            ++ toString 1000
                            ++ ",0) scale(-1,1)"
                        )
    in
    Svg.g
        (case maybeMsg of
            Nothing ->
                []

            Just msg ->
                [ Svg.Attributes.cursor "pointer"
                , Svg.Events.onClick msg
                ]
        )
        [ Svg.polygon
            [ Svg.Attributes.points points
            , Svg.Attributes.fill (colorToHex state)
            , Svg.Attributes.transform transform
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x (toString textX)
            , Svg.Attributes.y (toString textY)
            , Svg.Attributes.fill "white"
            , Svg.Attributes.strokeWidth "0"
            , Svg.Attributes.style
                ("text-anchor: middle; font-family: sans-serif; font-size: "
                    ++ toString ((5 / 6) * Grid.regionHeight cf region)
                    ++ "px; alignment-baseline: middle"
                )
            ]
            [ Svg.text text
            ]
        ]


polygonPoints : List Float2 -> String
polygonPoints path =
    path
        |> List.map (\( x, y ) -> ( toString x, toString y ))
        |> List.map (\( x, y ) -> x ++ " " ++ y)
        |> String.join ", "


drawAnchoredText : number -> number -> number -> TextAnchor -> Color -> String -> Svg Msg
drawAnchoredText x y height textAnchor color text =
    Svg.text_
        [ Svg.Attributes.x (toString x)
        , Svg.Attributes.y (toString y)
        , Svg.Attributes.style
            ("text-anchor: "
                ++ textAnchorToString textAnchor
                ++ "; "
                ++ "font-family: sans-serif; "
                ++ "font-size: "
                ++ toString height
                ++ "px; "
                ++ "alignment-baseline: before-edge"
            )
        , Svg.Attributes.fill (colorToHex color)
        ]
        [ Svg.text text
        ]


instructionsView : Layout a -> Player -> Svg Msg
instructionsView layout player =
    let
        textHeight =
            30

        ( playerX, playerY ) =
            ( 90, 210 )

        ( bombX, bombY ) =
            ( 770, 120 )

        ( explodeX, explodeY ) =
            ( 770, 300 )

        ( controlsX, controlsY ) =
            ( 10, playerY + 100 )

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
                |> List.map (Tuple.mapSecond makeLines)
                |> List.concatMap (\( x, lines ) -> makeTspans x lines)

        makeLines : String -> List String
        makeLines str =
            str
                |> String.lines
                |> List.map String.trim
                |> List.filter (\s -> String.length s > 1)

        makeTspans : Float -> List String -> List (Svg Msg)
        makeTspans x strings =
            strings
                -- give first line of each section a bigger dy multiplier
                -- in order to space them out more
                |> List.indexedMap
                    (\i s ->
                        ( if i == 0 then
                            2.3
                          else
                            1.0
                        , s
                        )
                    )
                |> List.map (Tuple.mapFirst ((*) textHeight))
                |> List.map (uncurry (makeTspan x))

        makeTspan : Float -> Float -> String -> Svg Msg
        makeTspan x0 height text =
            Svg.tspan
                [ Svg.Attributes.x (toString x0)
                , Svg.Attributes.dy (toString height)
                ]
                [ Svg.text text ]

        leftArm =
            player.leftArm

        rightArm =
            player.rightArm

        movedPlayer =
            { player
                | position = ( playerX, playerY )
                , leftArm = { leftArm | hand = ( -50, 0 ) }
                , rightArm = { rightArm | hand = ( 50, 0 ) }
                , fixedLegX = playerX - 30
                , freeLegX = playerX + 30
            }
    in
    Svg.g
        []
        [ svgButton 10 10 240 50 "Main menu" (GoToPage Title)
        , drawPlayer movedPlayer
        , drawAnchoredText playerX (playerY + 50) 20 Middle Color.black "Fig 1: you"
        , drawBomb ( bombX, bombY ) 50 20
        , drawAnchoredText bombX (bombY + 60) 20 Middle Color.black "Fig 2: a bomb"
        , drawCircle ( explodeX, explodeY ) 80 explosionGradientFill
            |> filter turbulenceId
        , drawAnchoredText explodeX (explodeY + 100) 20 Middle Color.black "Fig 3: an explosion"
        , drawControlToggle layout player Nothing controlsX controlsY 120 55 Left
        , drawAnchoredText controlsX (controlsY + 60) (textHeight - 10) Start Color.black "Fig 4: AI toggle" -- , "switch"]
        , drawAnchoredText (controlsX + 65) (controlsY + 60 + 20) (textHeight - 10) Start Color.black "switch"
        , Svg.text_
            [ Svg.Attributes.x (toString 0)
            , Svg.Attributes.y (toString 20)
            , Svg.Attributes.style
                ("text-anchor: start; font-family: sans-serif; "
                    ++ "font-size: "
                    ++ toString textHeight
                    ++ "px; "
                    ++ "alignment-baseline: before-edge"
                )
            , Svg.Attributes.fill (colorToHex uiColor.menuTextBackground)
            ]
            mainTextTspans
        ]


parallelogramSideOffset : Float -> Float -> Int -> Float -> Int -> Int -> Float
parallelogramSideOffset width height numRows colWidth row col =
    toFloat (numRows - row - 1) * height / (uiSlope * toFloat numRows) + toFloat col * colWidth


parallelogramTopOffset : Float -> Int -> Int -> Float
parallelogramTopOffset height numRows row =
    toFloat row * height / toFloat numRows


pauseMenuX : Float
pauseMenuX =
    10


pauseMenu : Svg Msg
pauseMenu =
    let
        height =
            50

        padding =
            10

        y i =
            70 + i * (height + padding)
    in
    Svg.g
        []
        [ svgButton pauseMenuX (y 0) 140 50 "Play" TogglePause
        , svgButton pauseMenuX (y 1) 220 50 "Main menu" (GoToPage Title)
        ]


{-| Convert a keycode into a string.

The only key codes that map directly to the correct unicode symbol
are the letter and digit keys, so for everything else, we just
show the stringified keycode.

-}
keyToString : Char.KeyCode -> String
keyToString key =
    if
        (key >= Char.toCode 'A' && key <= Char.toCode 'Z')
            || (key >= Char.toCode '0' && key <= Char.toCode '9')
    then
        key
            |> Char.fromCode
            |> toString
            |> String.dropLeft 1
            |> String.dropRight 1
    else
        toString key


titleView : Layout a -> Bool -> Svg Msg
titleView { screenWidth, screenHeight } gameStarted =
    let
        config =
            { rows = 29
            , cols = 20
            , rowPadding = 15
            , width = screenWidth
            , height = screenHeight
            , xOffset = 0
            , yOffset = 60
            }

        mainButtons =
            [ ( "new game", Just StartGame )
            , ( "options", Just (GoToPage (Options Nothing)) )
            , ( "help", Just (GoToPage Instructions) )
            ]

        conditionalButtons =
            case gameStarted of
                True ->
                    [ ( "continue", Just (GoToPage Game) ) ]

                False ->
                    []

        buttons =
            conditionalButtons ++ mainButtons

        gridWithTitle =
            Grid.create config
                |> setWidth config.cols
                |> setHeight 5
                |> insert ( "xtreme volleyball 2k17", uiColor.titleBackground, Nothing )
                |> setWidth 11

        finalGrid =
            buttons
                -- add color
                |> List.map (\( text, msg ) -> ( text, uiColor.menuTextBackground, msg ))
                -- insert in grid
                |> List.foldl (\data grid -> grid |> nextRow |> insert data) gridWithTitle
    in
    Svg.g
        []
        [ Svg.g [] (List.map (drawRegion finalGrid.config Left) finalGrid.data)
        , drawBomb ( 2 * screenWidth / 3, 350 ) 120 30
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
            , Svg.Attributes.fill (colorToHex uiColor.sky)
            ]
            []
        , drawNet model
        , drawPlayer model.player1
        , drawPlayer model.player2
        , if model.warmupTimer > 0 then
            drawTimer (model.warmupTimer + Time.second) (0.5 * model.screenWidth) 140 120
          else
            drawBall model model.ball
        , svgButton pauseMenuX 70 140 50 "Pause" TogglePause
        , drawScore model
        , drawTimer model.ball.countdown (0.5 * model.screenWidth) 0 80
        , drawUiBlock (drawCenteredText "" 0) Nothing 190 0 135 60 uiColor.hudSecondaryBackground model.screenWidth Left
        , drawUiBlock (drawCenteredText "" 0) Nothing 190 0 135 60 uiColor.hudSecondaryBackground model.screenWidth Right
        , drawUiBlock (drawCenteredText model.player1.name (60 * 5 / 6)) Nothing (-60 / 2) 0 220 60 uiColor.menuTextBackground model.screenWidth Left
        , drawUiBlock (drawCenteredText model.player2.name (60 * 5 / 6)) Nothing (-60 / 2) 0 220 60 uiColor.menuTextBackground model.screenWidth Right
        , drawControlToggle model model.player1 (Just TogglePlayer1Ai) 200 0 120 55 Left
        , drawControlToggle model model.player2 (Just TogglePlayer2Ai) 200 0 120 55 Right
        ]


drawNet : Layout a -> Svg Msg
drawNet { screenWidth, screenHeight, netWidth, netHeight } =
    Svg.rect
        [ Svg.Attributes.x (toString ((screenWidth - netWidth) / 2))
        , Svg.Attributes.y (toString (screenHeight - netHeight))
        , Svg.Attributes.width (toString netWidth)
        , Svg.Attributes.height (toString netHeight)
        , Svg.Attributes.fill (colorToHex uiColor.net)
        ]
        []


drawArm : Color -> Side -> Float2 -> Arm -> Svg Msg
drawArm strokeColor side playerPosition arm =
    let
        radius =
            50

        ( sx, sy ) =
            arm.shoulder

        ( hx, hy ) =
            arm.hand

        ( px, py ) =
            playerPosition

        sweep =
            case side of
                Left ->
                    1

                Right ->
                    0

        armPath =
            [ ( "M", [ px, py ] )
            , ( "m", [ sx, sy ] )
            , ( "a", [ radius, 2 * radius, 1, 0, sweep, hx, hy ] )
            ]
    in
    Svg.path
        [ Svg.Attributes.d (pathString armPath)
        , Svg.Attributes.stroke (colorToHex strokeColor)
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.fillOpacity "0.0"
        , Svg.Attributes.strokeWidth "20"
        ]
        []


drawLegs : Color -> Player -> Svg Msg
drawLegs strokeColor player =
    let
        radius =
            50

        -- sweep is an svg arc parameter that affects curvature.
        -- 0 makes legs bend like an open paren: '('
        -- 1 makes legs bend like a close paren: ')'
        sweep =
            if V2.getX player.velocity < 0 then
                0
                -- player is moving left
            else
                1

        -- player is moving right
        ( px, py ) =
            player.position

        footY =
            py + player.waistY + player.legHeight

        moveToWaist =
            ( "M", [ px, py + player.waistY ] )

        legPath =
            [ moveToWaist
            , ( "A", [ radius, radius, 1, 0, sweep, player.fixedLegX, footY ] )
            , moveToWaist
            , ( "A", [ radius, radius, 1, 0, sweep, player.freeLegX, footY ] )
            ]
    in
    Svg.path
        [ Svg.Attributes.d (pathString legPath)
        , Svg.Attributes.stroke (colorToHex strokeColor)
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
                    uiColor.player

                False ->
                    uiColor.dead

        ( px, py ) =
            player.position

        torsoY =
            py - 50

        torsoHeight =
            70

        headRadius =
            25

        headY =
            -0.8 * headRadius + torsoY - torsoHeight / 2
    in
    Svg.g
        []
        [ drawCircle ( px, headY ) headRadius (colorToHex fillColor)
        , drawLegs fillColor player
        , drawArm fillColor Left player.position player.leftArm
        , drawArm fillColor Right player.position player.rightArm
        , Svg.ellipse
            [ Svg.Attributes.cx (toString px)
            , Svg.Attributes.cy (toString torsoY)
            , Svg.Attributes.rx (toString player.size)
            , Svg.Attributes.ry (toString (torsoHeight / 2))
            , Svg.Attributes.fill (colorToHex fillColor)
            ]
            []
        ]


drawTimer : Time -> Float -> Float -> Float -> Svg Msg
drawTimer time x y height =
    Svg.text_
        [ Svg.Attributes.x (toString x)
        , Svg.Attributes.y (toString y)
        , Svg.Attributes.style
            ("text-anchor: middle; font-family: sans-serif; font-size: "
                ++ toString height
                ++ "px; alignment-baseline: before-edge"
            )
        , Svg.Attributes.fill "white"
        ]
        [ Svg.text (toString (floor (Time.inSeconds time)))
        ]


{-| Stringify everything and stick it all together in a string
that we can use for attribute "d" of Svg.path
example:
[(M,50,50),(L,100,100)] -> "M50 50 L100 100"
-}
pathString : List ( String, List number ) -> String
pathString list =
    list
        |> List.map (Tuple.mapSecond (List.map toString))
        |> List.map (Tuple.mapSecond (String.join " "))
        |> List.map (uncurry (++))
        |> String.join " "


drawBomb : Float2 -> Float -> Float -> Svg Msg
drawBomb position size rotation =
    let
        ( x, y ) =
            position

        stemW =
            size

        stemH =
            0.7 * size

        stemX =
            -0.5 * stemW

        stemY =
            -1 * (size + stemH / 2)

        wickPath =
            [ ( "M", [ 0, stemY ] )
            , ( "a", [ size, 2 * size, 0, 0, 1, size, -10 ] )
            ]

        transform =
            "translate("
                ++ toString x
                ++ ","
                ++ toString y
                ++ ")"
                ++ " rotate("
                ++ toString rotation
                ++ ")"
    in
    Svg.g
        [ Svg.Attributes.transform transform ]
        [ drawCircle ( 0, 0 ) size (colorToHex uiColor.bomb)
        , Svg.rect
            [ Svg.Attributes.width (toString stemW)
            , Svg.Attributes.height (toString stemH)
            , Svg.Attributes.fill (colorToHex uiColor.bomb)
            , Svg.Attributes.x (toString stemX)
            , Svg.Attributes.y (toString stemY)
            ]
            []
        , Svg.path
            [ Svg.Attributes.d (pathString wickPath)
            , Svg.Attributes.stroke (colorToHex uiColor.wick)
            , Svg.Attributes.fillOpacity "0.0"
            , Svg.Attributes.strokeWidth "3"
            ]
            []
        ]


drawBall : Settings a -> Explosive (Mover b) -> Svg Msg
drawBall { graphicsQuality } { position, size, status } =
    case status of
        Exploded ->
            Svg.g [] []

        Exploding ->
            case graphicsQuality of
                Fancy ->
                    drawCircle position size explosionGradientFill
                        |> filter turbulenceId

                Fast ->
                    drawCircle position size explosionGradientFill

        Safe ->
            let
                -- degrees per horizontal distance unit
                angularSpeed =
                    3 * 360 / 1000

                -- Angle is determined completely by the X value.
                -- This looks pretty convincing. It appears to change rotational
                -- direction when it bounces, and rotation appears to slow down
                -- or speed up as the ball itself does.
                angle =
                    V2.getX position * angularSpeed
            in
            drawBomb position size angle


drawCircle : Float2 -> Float -> String -> Svg Msg
drawCircle position radius fill =
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
        transform =
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"
    in
    Svg.g
        [ Svg.Attributes.transform transform
        , Svg.Attributes.cursor "pointer"
        , Svg.Events.onClick onClickEvent
        ]
        [ Svg.rect
            [ Svg.Attributes.width (toString w)
            , Svg.Attributes.height (toString h)
            , Svg.Attributes.fill (colorToHex uiColor.menuTextBackground)
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x (toString (toFloat w / 2.0))
            , Svg.Attributes.y (toString (toFloat h / 2.0))
            , Svg.Attributes.style
                ("text-anchor: middle; font-family: sans-serif; font-size: "
                    ++ toString (h - 5)
                    ++ "px; alignment-baseline: middle"
                )
            , Svg.Attributes.fill "white"
            ]
            [ Svg.text text
            ]
        ]


drawScore : Layout (Players a) -> Svg Msg
drawScore { player1, player2, screenWidth } =
    let
        size =
            60

        offset =
            324
    in
    Svg.g
        []
        [ drawUiBlock (drawCenteredText (toString player1.score) 60) Nothing offset 0 90 60 uiColor.hudTertiaryBackground screenWidth Left
        , drawUiBlock (drawCenteredText (toString player2.score) 60) Nothing offset 0 90 60 uiColor.hudTertiaryBackground screenWidth Right
        ]


drawControlToggle : Layout a -> MovementKeys { b | ai : Bool } -> Maybe Msg -> Float -> Float -> Float -> Float -> Side -> Svg Msg
drawControlToggle layout player clickEvent sideOffset topOffset w h side =
    let
        labelX =
            sideOffset + (h / 2) / uiSlope

        aiFill =
            getToggleColor player.ai

        keyboardFill =
            getToggleColor (not player.ai)

        drawKeyboardControls =
            drawControls player (h / 2)
    in
    Svg.g
        []
        [ drawUiBlock (drawCenteredText "Controls" (h / 2 - 5)) Nothing labelX topOffset w (h / 2) uiColor.hudSecondaryBackground layout.screenWidth side
        , drawUiBlock (drawCenteredText "AI" (h / 2 - 5)) clickEvent sideOffset (topOffset + h / 2) (w / 2) (h / 2) aiFill layout.screenWidth side
        , drawUiBlock drawKeyboardControls clickEvent (sideOffset + w / 2) (topOffset + h / 2) (w / 2) (h / 2) keyboardFill layout.screenWidth side
        ]


getToggleColor : Bool -> Color
getToggleColor selected =
    if selected then
        uiColor.toggleSelected
    else
        uiColor.toggleNotSelected


{-| Convert list of ordered pairs into a string suitable for Svg.Attributes.points
-}
pointsListToString : List ( number, number ) -> String
pointsListToString list =
    list
        |> List.map (\( x, y ) -> toString x ++ " " ++ toString y)
        |> String.join ", "


parallelogramPoints : Float -> Float -> Float -> Float -> List ( Float, Float )
parallelogramPoints x y w h =
    let
        xoffset =
            h / uiSlope
    in
    [ ( x, y + h )
    , ( x + w, y + h )
    , ( x + w + xoffset, y )
    , ( x + xoffset, y )
    ]


{-| Draws a parallelogram, and takes a callback function to draw its contents.
Most of the UI is made up of these blocks.
-}
drawUiBlock : (Float -> Float -> Svg Msg) -> Maybe Msg -> Float -> Float -> Float -> Float -> Color -> Float -> Side -> Svg Msg
drawUiBlock contents clickEvent sideOffset topOffset baseWidth height fill screenWidth side =
    let
        points =
            parallelogramPoints sideOffset topOffset baseWidth height
                -- cut off side if it hangs off the screen
                |> List.map (\( x, y ) -> ( max 0 x, y ))
                |> pointsListToString

        midpointOffset =
            sideOffset + (baseWidth + height / uiSlope) / 2

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
                        ("translate("
                            ++ toString screenWidth
                            ++ ",0) scale(-1,1)"
                        )
    in
    Svg.g
        (case clickEvent of
            Nothing ->
                []

            Just event ->
                [ Svg.Events.onClick event
                , Svg.Attributes.cursor "pointer"
                ]
        )
        [ Svg.polygon
            [ Svg.Attributes.points points
            , Svg.Attributes.fill (colorToHex fill)
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
            ("text-anchor: middle; font-family: sans-serif; font-size: "
                ++ toString size
                ++ "px; alignment-baseline: before-edge"
            )
        , Svg.Attributes.fill "white"
        ]
        [ Svg.text text
        ]


{-| Compact display of movement keys that is horizontally and vertically
centered on (x,y).

looks like this:
---W
--A D

-}
drawControls : MovementKeys a -> Float -> Float -> Float -> Svg Msg
drawControls { leftKey, rightKey, jumpKey } h x y =
    Svg.g
        []
        [ Svg.text_
            [ Svg.Attributes.x (toString x)
            , Svg.Attributes.y (toString (y + h / 2))
            , Svg.Attributes.style
                ("text-anchor: middle; font-family: monospace; font-size: "
                    ++ toString ((h - 4) / 2)
                    ++ "px; alignment-baseline: after-edge"
                )
            , Svg.Attributes.fill (colorToHex uiColor.text)
            ]
            [ Svg.text (keyToString jumpKey)
            ]
        , Svg.text_
            [ Svg.Attributes.x (toString x)
            , Svg.Attributes.y (toString (y + h / 2))
            , Svg.Attributes.style
                ("text-anchor: middle; font-family: monospace; font-size: "
                    ++ toString ((h - 4) / 2)
                    ++ "px; alignment-baseline: before-edge"
                )
            , Svg.Attributes.fill (colorToHex uiColor.text)
            ]
            [ Svg.text (keyToString leftKey ++ " " ++ keyToString rightKey)
            ]
        ]
