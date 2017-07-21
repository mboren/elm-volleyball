module State exposing (init, update, subscriptions)

import Time exposing (Time)
import Task

import Vector2 as V2 exposing (Vec2, Float2)
import Types exposing (..)

init : (Model, Cmd Msg)
init =
  let
    p1 = Player (1000/4,600/3) (0,0) (0,0) 50
  in
    (Model True 0 0 1000 600 10 250 p1, Task.perform Resume Time.now)

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
            |> updatePosition model.screenHeight dt
      in
        ( { model
            | time = newTime
            , delta = dt
            , player = player_
          }
        , Cmd.none
        )

    _ ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.paused of
    True ->
      Sub.none
    False ->
      Time.every (10 * Time.millisecond) Tick

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
