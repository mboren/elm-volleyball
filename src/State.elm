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
    p1 = Player (1000/4,600/3) (0,0) (0,0) 50
  in
    (Model True 0 0 1000 600 10 250 p1 False False, Task.perform Resume Time.now)

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
      in
        ( { model
            | time = newTime
            , delta = dt
            , player = player_
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

addAcceleration : Float2 -> Player -> Player
addAcceleration a player =
  { player
    | acceleration = (V2.add a player.acceleration)
  }

applyGravity : Player -> Player
applyGravity player =
  let
    -- choice of g is essentially arbitrary and should be set
    -- to whatever makes falling look good
    g = (0, 0.001)
  in
    player
      |> addAcceleration g

applyMovementKeys : Bool -> Bool -> Player -> Player
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
updatePosition : Int -> Time -> Player -> Player
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
        -- keep in vertical bounds
        |> clampY 0 ((toFloat screenHeight) - player.size)

    newAcceleration = (0, 0)
  in
    { player
      | position = newPosition
      , velocity = newVelocity
      , acceleration = newAcceleration
    }
