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
                    Svg.g
                        []
                        [ if model.paused then
                            gameView model
                                |> filter pauseBlurId
                          else
                            gameView model
                        , pauseMenu model.screenWidth model.paused
                        ]

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
                        OptionsMenu (QualityButton (boolToUiSettingState (qs == model.graphicsQuality)) qs)
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
                |> insert (OptionsMenu OptionsTitle)
                |> nextSection
                -- controls
                |> createPlayerRow model.player1 getUiSettingState Left
                |> nextSection
                |> createPlayerRow model.player2 getUiSettingState Right
                |> nextSection
                -- graphical quality
                |> createToggleRow "Quality" graphicsButtons
                |> nextSection
                -- back button
                |> setHeight 4
                |> setWidth 6
                |> insert (OptionsMenu BackButton)
                -- Note about key codes
                |> markAsStartCol
                |> setHeight 1
                |> setWidth 7
                |> insert (OptionsMenu (InfoText "Note: non-alphanumeric keys will show raw key code, but will work fine."))
                |> nextRow
                |> insert (OptionsMenu (InfoText "Nobody has made a comprehensive keycode->string function for Elm yet."))
                |> nextRow
                |> insert (OptionsMenu (InfoText "I might take a stab at it after I wrap up more interesting features."))
    in
    Svg.g
        [ Svg.Attributes.stroke "white" ]
        [ drawGrid model.screenWidth newGrid ]


createPlayerRow : Player -> (Side -> MovementKey -> UiSettingState) -> Side -> Grid GridData -> Grid GridData
createPlayerRow player getState side grid =
    grid
        |> setWidth 6
        |> setHeight 4
        |> insert (OptionsMenu (OptionLabel player.name))
        |> markAsStartCol
        |> setHeight 2
        |> insert (OptionsMenu (KeyChangeButton (getState side JumpKey) player JumpKey side))
        |> nextRow
        |> setWidth 3
        |> insert (OptionsMenu (KeyChangeButton (getState side LeftKey) player LeftKey side))
        |> insert (OptionsMenu (KeyChangeButton (getState side RightKey) player RightKey side))
        |> Grid.resetStartCol


createToggleRow : String -> List GridData -> Grid GridData -> Grid GridData
createToggleRow name buttons grid =
    let
        gridWithLabel =
            grid
                |> setWidth 6
                |> setHeight 4
                |> insert (OptionsMenu (OptionLabel name))
                |> setWidth ((grid.config.cols - 6) // List.length buttons)
    in
    List.foldl insert gridWithLabel buttons


cellColor : UiSettingState -> Color
cellColor setting =
    case setting of
        Selected ->
            uiColor.hudTertiaryBackground

        NotSelected ->
            uiColor.hudSecondaryBackground


hudTransform : Float -> Side -> Svg.Attribute Msg
hudTransform screenWidth side =
    case side of
        Left ->
            -- results in <... transform>, which has no effect
            Svg.Attributes.transform ""

        Right ->
            Svg.Attributes.transform
                -- move to other side of screen
                ("translate("
                    ++ toString screenWidth
                    ++ ",0)"
                    -- flip horizontally
                    ++ " scale(-1,1)"
                )


hudColor : HudElement -> Color
hudColor element =
    case element of
        PlayerName _ _ ->
            uiColor.menuTextBackground

        Score _ _ ->
            uiColor.hudTertiaryBackground

        Controls _ _ ->
            uiColor.hudSecondaryBackground

        Toggle toggleSide player _ ->
            case toggleSide of
                Left ->
                    getToggleColor player.ai

                Right ->
                    getToggleColor (not player.ai)


colorToFill : Color -> Svg.Attribute Msg
colorToFill color =
    Svg.Attributes.fill (colorToHex color)


keyChangeText : MovementKeys a -> MovementKey -> String
keyChangeText { leftKey, rightKey, jumpKey } key =
    case key of
        LeftKey ->
            "Left: " ++ keyToString leftKey

        RightKey ->
            "Right: " ++ keyToString rightKey

        JumpKey ->
            "Jump: " ++ keyToString jumpKey


uiElementToPrimitives : Grid.Config -> Grid.Region -> GridData -> List UiPrimitive
uiElementToPrimitives config region element =
    let
        height =
            Grid.regionHeight config region * 5 / 6

        path =
            region |> Grid.regionToPath config |> Grid.skewPath -uiSlope

        position =
            Grid.centroid path

        leftText : String -> Maybe Msg -> UiPrimitive
        leftText =
            Text height Middle position Left

        leftPoly : Color -> Maybe Msg -> UiPrimitive
        leftPoly =
            Polygon path Left
    in
    case element of
        Main elem ->
            case elem of
                MainTitle ->
                    [ leftPoly uiColor.titleBackground Nothing
                    , leftText "xtreme volleyball 2k17" Nothing
                    ]

                Button text msg ->
                    [ leftPoly uiColor.menuTextBackground (Just msg)
                    , leftText text (Just msg)
                    ]

        OptionsMenu elem ->
            optionsMenuElementToPrimitives leftText leftPoly elem

        Hud elem ->
            hudElementToPrimitives path height position elem


optionsMenuElementToPrimitives : (String -> Maybe Msg -> UiPrimitive) -> (Color -> Maybe Msg -> UiPrimitive) -> OptionsMenuElement -> List UiPrimitive
optionsMenuElementToPrimitives makeText makePoly elem =
    let
        ( color, clickEvent, text ) =
            case elem of
                OptionsTitle ->
                    ( uiColor.titleBackground, Nothing, "Options" )

                OptionLabel text ->
                    ( uiColor.menuTextBackground, Nothing, text )

                KeyChangeButton state player key side ->
                    ( cellColor state
                    , Just (PrepareToChangePlayerKey side key)
                    , keyChangeText player key
                    )

                QualityButton state qualitySetting ->
                    ( cellColor state
                    , Just (ChangeSetting (SetQuality qualitySetting))
                    , qualitySettingToString qualitySetting
                    )

                BackButton ->
                    ( uiColor.menuTextBackground
                    , Just (GoToPage Title)
                    , "Back"
                    )

                InfoText text ->
                    ( cellColor NotSelected, Nothing, text )
    in
    [ makePoly color clickEvent
    , makeText text clickEvent
    ]


hudElementToPrimitives : List Float2 -> Float -> Float2 -> HudElement -> List UiPrimitive
hudElementToPrimitives path height position elem =
    let
        color =
            hudColor elem

        makeText : Side -> String -> Maybe Msg -> UiPrimitive
        makeText =
            Text height Middle position

        makePoly : Side -> Color -> Maybe Msg -> UiPrimitive
        makePoly =
            Polygon path
    in
    case elem of
        PlayerName player side ->
            [ makePoly side color Nothing
            , makeText side player.name Nothing
            ]

        Score player side ->
            [ makePoly side color Nothing
            , makeText side (toString player.score) Nothing
            ]

        Controls player side ->
            [ makePoly side color Nothing
            , makeText side "Controls" Nothing
            ]

        Toggle toggleSide player side ->
            let
                msg =
                    Just (ToggleAi side)

                textPrimitives =
                    case toggleSide of
                        Left ->
                            [ makeText side "AI" msg ]

                        Right ->
                            controlsTextPrimitives position height player side msg
            in
            makePoly side color msg :: textPrimitives


controlsTextPrimitives : Float2 -> Float -> MovementKeys a -> Side -> Maybe Msg -> List UiPrimitive
controlsTextPrimitives ( x, y ) height player side msg =
    let
        quarterHeight =
            height / 4

        jumpPosition =
            ( x, y - quarterHeight )

        leftAndRightPosition =
            ( x, y + quarterHeight )

        jumpText =
            keyToString player.jumpKey

        leftAndRightText =
            keyToString player.leftKey ++ " " ++ keyToString player.rightKey
    in
    [ Text (height / 2) Middle jumpPosition side jumpText msg
    , Text (height / 2) Middle leftAndRightPosition side leftAndRightText msg
    ]


textSideTransform : Float -> Side -> (Float -> Float)
textSideTransform screenWidth side =
    case side of
        Left ->
            identity

        Right ->
            \x -> screenWidth - x


createClickAttributes : Maybe Msg -> List (Svg.Attribute Msg)
createClickAttributes maybeMsg =
    case maybeMsg of
        Nothing ->
            []

        Just msg ->
            [ Svg.Attributes.cursor "pointer"
            , Svg.Events.onClick msg
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

        config =
            { rows = 55
            , cols = 120
            , rowPadding = 0
            , width = 120
            , height = 55
            , xOffset = controlsX + 200
            , yOffset = controlsY
            }

        controlToggle =
            Grid.create config
                |> setWidth 120
                |> setHeight 55
                |> insertControlToggle
                |> Grid.map (\partialElem -> Hud (partialElem player Left))
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
        , drawGrid layout.screenWidth controlToggle
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


pauseMenu : Float -> Bool -> Svg Msg
pauseMenu screenWidth paused =
    let
        config =
            { rows = 2
            , cols = 1
            , rowPadding = 10
            , width = 220
            , height = 100
            , xOffset = 0
            , yOffset = 70
            }
    in
    Grid.create config
        |> setWidth 1
        |> setHeight 1
        |> insertPauseMenu paused
        |> drawGrid screenWidth


insertPauseMenu : Bool -> Grid GridData -> Grid GridData
insertPauseMenu paused grid =
    if paused then
        grid
            |> insert (Main (Button "Resume" TogglePause))
            |> nextRow
            |> insert (Main (Button "Main menu" (GoToPage Title)))
    else
        grid
            |> insert (Main (Button "Pause" TogglePause))


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
            [ Main (Button "new game" StartGame)
            , Main (Button "options" (GoToPage (Options Nothing)))
            , Main (Button "help" (GoToPage Instructions))
            ]

        conditionalButtons =
            case gameStarted of
                True ->
                    [ Main (Button "continue" (GoToPage Game)) ]

                False ->
                    []

        buttons =
            conditionalButtons ++ mainButtons

        gridWithTitle =
            Grid.create config
                |> setWidth config.cols
                |> setHeight 5
                |> insert (Main MainTitle)
                |> setWidth 11

        finalGrid =
            buttons
                |> List.foldl (\data grid -> grid |> nextRow |> insert data) gridWithTitle
    in
    Svg.g
        []
        [ drawGrid screenWidth finalGrid
        , drawBomb ( 2 * screenWidth / 3, 350 ) 120 30
        ]


drawGrid : Float -> Grid GridData -> Svg Msg
drawGrid screenWidth grid =
    grid.data
        |> List.unzip
        |> uncurry (List.map2 (uiElementToPrimitives grid.config))
        |> List.foldl (++) []
        |> List.map (drawUiPrimitive screenWidth)
        |> Svg.g []


filter : String -> Svg Msg -> Svg Msg
filter filterId svg =
    Svg.g
        [ Svg.Attributes.filter ("url(#" ++ filterId ++ ")")
        ]
        [ svg
        ]


insertControlToggle : Grid (Player -> Side -> HudElement) -> Grid (Player -> Side -> HudElement)
insertControlToggle grid =
    let
        -- We want the cursor after this function to act
        -- just like we had called insert a single time.
        -- I think this is a handy convention, but I have
        -- no way to guarantee it, so I have some mixed
        -- feelings about it.
        endingCursor =
            grid
                |> Grid.goRight
                |> .cursor

        newGrid =
            grid
                |> markAsStartCol
                |> setHeight (grid.cursor.h // 2)
                |> insert Controls
                |> nextRow
                |> setWidth (grid.cursor.w // 2)
                |> insert (Toggle Left)
                |> insert (Toggle Right)
                |> Grid.prevRow
                |> setHeight grid.cursor.h
                |> setWidth grid.cursor.w
                |> Grid.goRight
    in
    { newGrid
        | startCol = grid.startCol
        , cursor = endingCursor
    }


gameView : Model -> Svg Msg
gameView model =
    let
        grid =
            { rows = floor model.screenHeight
            , cols = floor model.screenWidth
            , rowPadding = 0
            , width = model.screenWidth
            , height = model.screenHeight
            , xOffset = 0
            , yOffset = 0
            }
                |> Grid.create
                |> setHeight 60
                |> setWidth 220
                |> insert PlayerName
                |> setWidth 135
                |> insertControlToggle
                |> setWidth 90
                |> insert Score

        leftHud =
            Grid.map (\x -> Hud (x model.player1 Left)) grid

        rightHud =
            Grid.map (\x -> Hud (x model.player2 Right)) grid
    in
    Svg.g
        []
        [ Svg.rect
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.fill (colorToHex uiColor.sky)
            ]
            []
        , drawGrid model.screenWidth leftHud
        , drawGrid model.screenWidth rightHud
        , drawNet model
        , drawPlayer model.player1
        , drawPlayer model.player2
        , if model.warmupTimer > 0 then
            drawTimer (model.warmupTimer + Time.second) (0.5 * model.screenWidth) 140 120
          else
            drawBall model model.ball
        , drawTimer model.ball.countdown (0.5 * model.screenWidth) 0 80
        ]


createStyle : Float -> TextAnchor -> String
createStyle height anchor =
    "text-anchor: "
        ++ textAnchorToString anchor
        ++ "; font-family: sans-serif; font-size: "
        ++ toString height
        ++ "px; alignment-baseline: middle"


drawUiPrimitive : Float -> UiPrimitive -> Svg Msg
drawUiPrimitive screenWidth prim =
    case prim of
        Polygon path side color maybeMsg ->
            Svg.polygon
                ([ Svg.Attributes.points (polygonPoints path)
                 , Svg.Attributes.fill (colorToHex color)
                 , hudTransform screenWidth side
                 ]
                    ++ createClickAttributes maybeMsg
                )
                []

        Text height anchor ( x, y ) side text maybeMsg ->
            Svg.text_
                ([ Svg.Attributes.x (toString (textSideTransform screenWidth side x))
                 , Svg.Attributes.y (toString y)
                 , Svg.Attributes.style (createStyle height anchor)
                 , Svg.Attributes.fill "white"
                 , Svg.Attributes.strokeWidth "0"
                 ]
                    ++ createClickAttributes maybeMsg
                )
                [ Svg.text text ]


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


getToggleColor : Bool -> Color
getToggleColor selected =
    if selected then
        uiColor.toggleSelected
    else
        uiColor.toggleNotSelected
