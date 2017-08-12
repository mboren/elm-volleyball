module State exposing (init, subscriptions, update)

import Animation exposing (animation, duration, from, to)
import AnimationFrame
import Keyboard
import Mover
import Player
import Random exposing (float, pair)
import Time exposing (Time)
import Types exposing (..)
import Vector2 as V2 exposing (Float2, Vec2)


-- calculated based on screen dimensions and gravity value


ballVxLimit =
    0.514142842854285


ballVyLimit =
    0.714142842854285


warmupLength : Time
warmupLength =
    3 * Time.second


defaultPlayer1Controls =
    { leftKey = 83
    , rightKey = 70
    , jumpKey = 69
    }


defaultPlayer2Controls =
    { leftKey = 74
    , rightKey = 76
    , jumpKey = 73
    }


defaultBall : Explosive (Mover {})
defaultBall =
    { position = ( 1000 / 2, 600 / 3 )
    , velocity = ( 0, 0 )
    , maxVx = ballVxLimit
    , acceleration = ( 0, 0 )
    , size = 20
    , onGround = False
    , leftWallX = 0
    , rightWallX = 1000
    , countdown = 10 * Time.second
    , status = Safe
    , explosionRadius = 250
    , animation = Animation.static 20
    }


init : ( Model, Cmd Msg )
init =
    let
        layout =
            { screenWidth = 1000
            , screenHeight = 600
            , netWidth = 10
            , netHeight = 250
            }

        p1 =
            Player.create layout defaultPlayer1Controls False Left

        p2 =
            Player.create layout defaultPlayer2Controls True Right
    in
    ( Model
        False
        False
        Title
        0
        warmupLength
        layout.screenWidth
        layout.screenHeight
        layout.netWidth
        layout.netHeight
        p1
        p2
        defaultBall
        Fast
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model
                | page = Game
                , paused = False
                , gameStarted = True
                , warmupTimer = warmupLength
                , player1 = Player.create model model.player1 model.player1.ai Left
                , player2 = Player.create model model.player2 model.player2.ai Right
              }
            , Random.generate NewBallVelocity velocityGenerator
            )

        GoToPage page ->
            let
                cannotGoToPage =
                    (page == Game) && not model.gameStarted
            in
            if cannotGoToPage then
                ( model, Cmd.none )
            else
                ( { model | page = page }
                , Cmd.none
                )

        NewBallVelocity v ->
            let
                newBall =
                    { defaultBall | velocity = v }
            in
            ( { model | ball = newBall }, Cmd.none )

        Tick dt ->
            let
                roundStart =
                    model.warmupTimer > 0 && model.warmupTimer <= dt

                revivePlayersOnRoundStart =
                    if roundStart then
                        mapPlayers Player.revive
                    else
                        identity
            in
            { model
                | ball = ballStep dt model model.ball
                , time = model.time + dt
                , warmupTimer = max 0 (model.warmupTimer - dt)
            }
                |> revivePlayersOnRoundStart
                |> mapPlayers (playerStep dt model.screenHeight model.ball)
                |> resetAtEndOfRound

        Press key ->
            ( model
                |> mapPlayers (Player.handleKey True key)
            , Cmd.none
            )

        Release key ->
            ( { model | paused = xor model.paused (key == 32) }
                |> mapPlayers (Player.handleKey False key)
            , Cmd.none
            )

        TogglePlayer1Ai ->
            ( { model | player1 = Player.toggleAi model.player1 }
            , Cmd.none
            )

        TogglePlayer2Ai ->
            ( { model | player2 = Player.toggleAi model.player2 }
            , Cmd.none
            )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        PrepareToChangePlayerKey side movementKey ->
            ( { model | page = Options (Just ( side, movementKey )) }, Cmd.none )

        ChangePlayerKey side movementKey keyCode ->
            let
                player =
                    case side of
                        Left ->
                            model.player1

                        Right ->
                            model.player2

                newPlayer =
                    case movementKey of
                        LeftKey ->
                            { player | leftKey = keyCode }

                        RightKey ->
                            { player | rightKey = keyCode }

                        JumpKey ->
                            { player | jumpKey = keyCode }

                newModel =
                    case side of
                        Left ->
                            { model | player1 = newPlayer }

                        Right ->
                            { model | player2 = newPlayer }
            in
            ( { newModel
                | page = Options Nothing
              }
            , Cmd.none
            )

        ChangeSetting settingsMsg ->
            case settingsMsg of
                SetQuality setting ->
                    ( { model | graphicsQuality = setting }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Title ->
            Sub.none

        Instructions ->
            Sub.none

        Game ->
            case model.paused of
                True ->
                    Sub.batch
                        [ Keyboard.downs Press
                        , Keyboard.ups Release
                        ]

                False ->
                    Sub.batch
                        [ AnimationFrame.diffs Tick
                        , Keyboard.downs Press
                        , Keyboard.ups Release
                        ]

        Options opt ->
            case opt of
                Just ( side, movementKey ) ->
                    Sub.batch
                        [ Keyboard.ups (ChangePlayerKey side movementKey)
                        ]

                Nothing ->
                    Sub.none


playerStep : Time -> Float -> Explosive (Mover b) -> Player -> Player
playerStep dt screenHeight ball player =
    player
        |> Mover.applyGravity
        |> Player.applyMovementKeys
        |> Player.aiMovement ball
        |> Player.applyJump
        |> Mover.updatePosition screenHeight dt
        |> Mover.stopAtWalls
        |> Mover.stopAtFloor screenHeight
        |> Player.updateLegs
        |> Player.updateArms ball.position
        |> killIfInExplosion ball


ballStep : Time -> Model -> Explosive (Mover {}) -> Explosive (Mover {})
ballStep dt model ball =
    case ball.status of
        Safe ->
            if model.warmupTimer <= 0 then
                ball
                    |> adjustBallBounds model
                    |> Mover.applyGravity
                    |> Mover.bounceOffWalls model.screenHeight 0.9
                    |> applyPlayerCollision model.screenWidth model.player1
                    |> applyPlayerCollision model.screenWidth model.player2
                    |> Mover.updatePosition model.screenHeight dt
                    |> updateCountdown dt
                    |> detectDetonation model.time
            else
                ball

        Exploding ->
            model.ball
                |> handleExplosionAnimation model.time
                |> updateStatus model.time

        Exploded ->
            model.ball


{-| When the ball collides with a player, we take the player's velocity, change
the signs of its components so it points toward the other side, then add
to the ball's velocity.
I like the way this feels a lot. It's intuitive, and it makes a wide variety of
shots possible. Feels a lot better than just normal collision.
-}
applyPlayerCollision : Float -> Player -> Explosive (Mover b) -> Explosive (Mover b)
applyPlayerCollision screenWidth player ball =
    let
        -- This is the butterfingers constant
        -- Smaller values make the player appear to fumble the
        -- ball more frequently.
        -- Large values make it hard to miss.
        handRadius =
            13

        leftDistance =
            player.position
                |> V2.add player.leftArm.shoulder
                |> V2.add player.leftArm.hand
                |> V2.distance ball.position

        rightDistance =
            player.position
                |> V2.add player.rightArm.shoulder
                |> V2.add player.rightArm.hand
                |> V2.distance ball.position

        distance =
            min leftDistance rightDistance

        horizontalSign =
            if V2.getX player.position < screenWidth / 2 then
                1.0
            else
                -1.0

        newVelocity =
            if distance <= handRadius then
                V2.map abs player.velocity
                    |> Tuple.mapFirst ((*) horizontalSign)
                    -- X should point to other side
                    |> Tuple.mapSecond ((*) -1.0)
                    -- Y should always point up or level
                    |> Tuple.mapSecond (min -0.5)
                    |> V2.scale 1.6
                    |> V2.add ball.velocity
                    |> (\( vx, vy ) -> ( vx, clamp -ballVyLimit ballVyLimit vy ))
            else
                ball.velocity
    in
    { ball | velocity = newVelocity }


{-| Split the screen into 3 regions based on the net position,
and change leftWallX and rightWallX depending on which region
the ball is in

            |             |
            |     top     |
            |right | left |
            |______|______|

-}
adjustBallBounds : Layout a -> Explosive (Mover {}) -> Explosive (Mover {})
adjustBallBounds { screenWidth, screenHeight, netWidth, netHeight } ball =
    let
        ( x, y ) =
            ball.position

        netX =
            if x < screenWidth / 2 then
                (screenWidth / 2) - (netWidth / 2)
            else
                (screenWidth / 2) + (netWidth / 2)

        netY =
            screenHeight - netHeight

        ( newLeftWall, newRightWall ) =
            if (y + ball.size) < netY then
                -- above net
                ( 0, screenWidth )
            else if x < screenWidth / 2 then
                -- left of net
                ( 0, netX )
            else if x > screenWidth / 2 then
                -- right of net
                ( netX, screenWidth )
            else
                -- if we're precisely in the middle, crash.
                -- I want to see how common this case is to determine
                -- how much effort to put into resolving it nicely.
                Debug.crash "ball in middle" ( 0, screenWidth )
    in
    { ball
        | leftWallX = newLeftWall
        , rightWallX = newRightWall
    }


updateCountdown : Time -> Explosive a -> Explosive a
updateCountdown dt ball =
    { ball | countdown = max 0 (ball.countdown - dt) }


detectDetonation : Time -> Explosive (Mover a) -> Explosive (Mover a)
detectDetonation time ball =
    if ball.status == Safe && ((ball.countdown <= 0) || ball.onGround) then
        { ball
            | status = Exploding
            , animation =
                animation time
                    |> from defaultBall.size
                    |> to ball.explosionRadius
                    |> duration (0.1 * Time.second)
        }
    else
        ball


velocityGenerator : Random.Generator Float2
velocityGenerator =
    let
        vx =
            float -ballVxLimit ballVxLimit

        vy =
            float -ballVyLimit 0
    in
    pair vx vy


checkCollision : Float2 -> Float -> Float2 -> Float -> Bool
checkCollision center1 radius1 center2 radius2 =
    let
        minimumDistance =
            radius1 + radius2

        distance =
            V2.distance center1 center2
    in
    distance <= minimumDistance


killIfInExplosion : Explosive (Mover a) -> Player -> Player
killIfInExplosion { position, size, status } player =
    if status == Exploding && checkCollision player.position player.size position size then
        Player.kill player
    else
        player


updateStatus : Time -> Explosive (Mover a) -> Explosive (Mover a)
updateStatus time ball =
    case ball.status of
        Exploding ->
            if Animation.isDone time ball.animation then
                { ball | status = Exploded }
            else
                { ball | status = Exploding }

        _ ->
            ball


handleExplosionAnimation : Time -> Explosive (Mover a) -> Explosive (Mover a)
handleExplosionAnimation time ball =
    { ball | size = Animation.animate time ball.animation }


{-| Increment each player's score if they are the sole survivor
of a round.
If both are dead or both are alive, nobody gets a point
-}
updateScores : Players a -> Players a
updateScores model =
    let
        ( p1Points, p2Points ) =
            case ( model.player1.alive, model.player2.alive ) of
                ( True, False ) ->
                    ( 1, 0 )

                ( False, True ) ->
                    ( 0, 1 )

                _ ->
                    ( 0, 0 )
    in
    { model
        | player1 = Player.addPoints p1Points model.player1
        , player2 = Player.addPoints p2Points model.player2
    }


resetAtEndOfRound : Model -> ( Model, Cmd Msg )
resetAtEndOfRound model =
    case model.ball.status of
        Exploded ->
            ( { model | warmupTimer = warmupLength }
                |> updateScores
            , Random.generate NewBallVelocity velocityGenerator
            )

        _ ->
            ( model, Cmd.none )


mapPlayers : (Player -> Player) -> Players a -> Players a
mapPlayers f players =
    { players
        | player1 = f players.player1
        , player2 = f players.player2
    }
