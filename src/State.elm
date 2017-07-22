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
jumpSpeed = -0.7

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
    (Model True 0 0 1000 600 10 250 p1 ball False False False, Task.perform Resume Time.now)

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
            |> applyJump model.jumpPressed
            |> updatePosition model.screenHeight dt
            |> handleFloor (toFloat model.screenHeight)
        ball_ =
          model.ball
            |> applyGravity
            |> bounce model 0.9
            |> applyPlayerCollision (toFloat model.screenWidth) model.player
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
            32 ->
              { model | jumpPressed = True }
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
            32 ->
              { model | jumpPressed = False }
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

{-
When the ball collides with a player, we take the player's velocity, change
the signs of its components so it points toward the other side, then add
to the ball's velocity.
I like the way this feels a lot. It's intuitive, and it makes a wide variety of
shots possible. Feels a lot better than just normal collision.
-}
applyPlayerCollision : Float -> Mover a -> Explosive (Mover a) -> Explosive (Mover a)
applyPlayerCollision screenWidth player ball =
  let
    minimumDistance = player.size + ball.size
    distance = V2.distance player.position ball.position

    horizontalSign =
      if (V2.getX player.position) < screenWidth / 2 then
        1.0
      else
        -1.0

    newVelocity =
      if distance <= minimumDistance then
        V2.map abs player.velocity
          |> Tuple.mapFirst ((*) horizontalSign) -- X should point to other side
          |> Tuple.mapSecond ((*) -1.0) -- Y should always point up or level
          |> V2.scale 1.0
          |> V2.add ball.velocity
      else
        ball.velocity
  in
    { ball | velocity = newVelocity }

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

applyJump : Bool -> Mover a -> Mover a
applyJump jumpPressed player =
  if player.onGround && jumpPressed then
    { player | velocity = player.velocity |> V2.setY jumpSpeed }
  else
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

handleFloor : Float -> Mover a -> Mover a
handleFloor floorY mover =
  if (V2.getY mover.position) + mover.size >= floorY then
    let
      newPosition = mover.position |> V2.setY (floorY - mover.size)

      newVelocity =
        if (V2.getY mover.velocity) < 0 then
          mover.velocity |> V2.setY 0
        else
          mover.velocity

      newAcceleration =
        if (V2.getY mover.acceleration) < 0 then
          mover.acceleration |> V2.setY 0
        else
          mover.acceleration
    in
      { mover
        | position = newPosition
        , velocity = newVelocity
        , acceleration = newAcceleration
        , onGround = True
      }
  else
    mover

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
