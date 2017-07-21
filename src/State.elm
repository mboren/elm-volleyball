module State exposing (init, update, subscriptions)

import Time exposing (Time)
import Task
import Keyboard

import Vector2 as V2 exposing (Vec2, Float2)
import Types exposing (..)

friction = 0.6
speedLimit = 0.7
playerAccelX = 0.05
frameTime = 10 * Time.millisecond

init : (Model, Cmd Msg)
init =
  let
    p1 =
      { position = (1000/4, 600/3)
      , velocity = (0, 0)
      , acceleration = (0, 0)
      , size = 50
      , onGround = False
      }
    ball =
      { position = (1000/2, 600/3)
      , velocity = (-0.1, 0)
      , acceleration = (0, 0)
      , size = 20
      , onGround = False
      , countdown = 10 * Time.second
      , exploding = False
      }
  in
    (Model True 0 0 1000 600 10 250 p1 ball False False, Task.perform Resume Time.now)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Resume startTime ->
      ({ model
        | paused = False
        , time = startTime
        , delta = 0
       }
      , Cmd.none)

    Tick newTime ->
      let
        dt = newTime - model.time
        player_ =
          model.player
            |> applyGravity
            |> applyMovementKeys model.leftPressed model.rightPressed
            |> updatePosition model.screenHeight dt
            |> clampPosition (0, 0) (toFloat model.screenWidth, toFloat model.screenHeight)
        ball_ =
          model.ball
            |> applyGravity
            |> bounce model 0.9
            |> updatePosition model.screenHeight dt
            |> updateCountdown dt
      in
        ( { model
            | time = newTime
            , delta = dt
            , player = player_
            , ball = ball_
          }
        , Cmd.none
        )

    Press key ->
      let
        newModel =
          case key of
            37 ->
              { model | leftPressed = True }
            39 ->
              { model | rightPressed = True }
            _ ->
              model
      in
        (newModel, Cmd.none)

    Release key ->
      let
        newModel =
          case key of
            37 ->
              { model | leftPressed = False }
            39 ->
              { model | rightPressed = False }
            _ ->
              model
      in
        (newModel, Cmd.none)
    _ ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.paused of
    True ->
      Sub.none
    False ->
      Sub.batch
        [ Time.every frameTime Tick
        , Keyboard.downs Press
        , Keyboard.ups Release
        ]

addAcceleration : Float2 -> Mover a -> Mover a
addAcceleration a player =
  { player
    | acceleration = (V2.add a player.acceleration)
  }

applyGravity : Mover a -> Mover a
applyGravity player =
  let
    -- choice of g is essentially arbitrary and should be set
    -- to whatever makes falling look good
    g = (0, 0.001)
  in
    if not player.onGround then
      player
        |> addAcceleration g
    else
      player

bounce : Model -> Float -> Mover a -> Mover a
bounce model bounciness ball =
  let
    (x, y) = ball.position
    (ax, ay) = ball.acceleration
    (vx, vy) = ball.velocity
    bounceX =
      if x + ball.size > (toFloat model.screenWidth) then
        -1.0 * (abs vx) * bounciness
      else if x - ball.size < 0 then
        (abs vx) * bounciness
      else
        vx

    bounceY =
      if y + ball.size > (toFloat model.screenHeight) then
        -1.0 * (abs vy) * bounciness
      else if y - ball.size < 0 then
        (abs vy) * bounciness
      else
        vy
  in
    { ball | velocity = (bounceX, bounceY) }

clampPosition : Float2 -> Float2 -> Mover a -> Mover a
clampPosition low high player =
  let
    newPosition =
      player.position
        |> clampX ((V2.getX low) + player.size) ((V2.getX high) - player.size)
        |> clampY ((V2.getY low) + player.size) ((V2.getY high) - player.size)
  in
    { player | position = newPosition }


applyMovementKeys : Bool -> Bool -> Mover a -> Mover a
applyMovementKeys leftPressed rightPressed player =
  case (leftPressed, rightPressed) of
    (False, False) ->
      -- if left/right are not pressed, then we apply friction
      -- to x velocity
      let
        oldXVelocity = V2.getX player.velocity
        newXVelocity = friction * oldXVelocity
        newVelocity = V2.setX newXVelocity player.velocity
      in
        { player | velocity = newVelocity }

    -- right
    (False, True) ->
      player |> addAcceleration (playerAccelX, 0)

    -- left
    (True, False) ->
      player |> addAcceleration (-1 * playerAccelX, 0)

    (True, True) ->
      player

clampX : Float -> Float -> Float2 -> Float2
clampX low high vector =
  let
    newX =
      vector
        |> V2.getX
        |> clamp low high
  in
    vector
      |> V2.setX newX

clampY : Float -> Float -> Float2 -> Float2
clampY low high vector =
  let
    newY =
      vector
        |> V2.getY
        |> clamp low high
  in
    vector
      |> V2.setY newY

{- Calculate change in position, velocity, and acceleration for this frame.
   Vertical position is capped to keep player on screen.
   Acceleration is zeroed after it is applied.
-}
updatePosition : Int -> Time -> Mover a -> Mover a
updatePosition screenHeight dt player =
  let
    -- v = v0 + a * t
    newVelocity =
      player.acceleration
        |> V2.scale dt
        |> V2.add player.velocity
        |> clampX (-1 * speedLimit) speedLimit

    -- r = r0 + 0.5 * t * (v + v0)
    newPosition =
      player.velocity
        |> V2.add newVelocity
        |> V2.scale (0.5 * dt)
        |> V2.add player.position

    newOnGround = (V2.getY player.position) + player.size >= toFloat screenHeight

    newAcceleration = (0, 0)
  in
    { player
      | position = newPosition
      , velocity = newVelocity
      , acceleration = newAcceleration
      , onGround = newOnGround
    }

updateCountdown : Time -> Explosive a -> Explosive a
updateCountdown dt ball =
  let
    remainingTime = ball.countdown - dt
    timerFinished = remainingTime <= 0
  in
    { ball
      | countdown = ball.countdown - dt
      , exploding = timerFinished
    }
