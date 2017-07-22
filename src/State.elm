module State exposing (init, update, subscriptions)

import Time exposing (Time)
import Task
import Keyboard

import Vector2 as V2 exposing (Vec2, Float2)
import Types exposing (..)

friction = 0.6
speedLimit = 0.3
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
      , leftWallX = 0
      , rightWallX = 1000/2
      , leftPressed = False
      , rightPressed = False
      , jumpPressed = False
      }
    p2 =
      { position = (3*1000/4, 600/3)
      , velocity = (0, 0)
      , acceleration = (0, 0)
      , size = 50
      , onGround = False
      , leftWallX = 1000/2
      , rightWallX = 1000
      , leftPressed = False
      , rightPressed = False
      , jumpPressed = False
      }
    ball =
      { position = (1000/2, 600/3)
      , velocity = (-0.1, 0)
      , acceleration = (0, 0)
      , size = 20
      , onGround = False
      , leftWallX = 0
      , rightWallX = 1000
      , countdown = 10 * Time.second
      , exploding = False
      }
  in
    (Model True 0 0 1000 600 10 250 p1 p2 ball, Task.perform Resume Time.now)

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
        ball_ =
          model.ball
            |> applyGravity
            |> bounce (toFloat model.screenHeight) 0.9
            |> applyPlayerCollision (toFloat model.screenWidth) model.player1
            |> applyPlayerCollision (toFloat model.screenWidth) model.player2
            |> updatePosition model.screenHeight dt
            |> updateCountdown dt
      in
        ( { model
            | time = newTime
            , delta = dt
            , player1 = playerStep dt (toFloat model.screenHeight) model.player1
            , player2 = playerStep dt (toFloat model.screenHeight) model.player2
            , ball = ball_
          }
        , Cmd.none
        )

    Press key ->
      ( { model
          | player1 = handleKey 83 70 69 True key model.player1
          , player2 = handleKey 74 76 73 True key model.player2
        }
      , Cmd.none
      )

    Release key ->
      ( { model
          | player1 = handleKey 83 70 69 False key model.player1
          , player2 = handleKey 74 76 73 False key model.player2
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
      Sub.batch
        [ Time.every frameTime Tick
        , Keyboard.downs Press
        , Keyboard.ups Release
        ]

playerStep : Time -> Float -> Controlled (Mover a) -> Controlled (Mover a)
playerStep dt screenHeight player =
  player
    |> applyGravity
    |> applyMovementKeys
    |> applyJump
    |> updatePosition (floor screenHeight) dt
    |> handleWalls
    |> handleFloor screenHeight

handleKey : Keyboard.KeyCode -> Keyboard.KeyCode -> Keyboard.KeyCode -> Bool -> Keyboard.KeyCode -> Controlled a -> Controlled a
handleKey leftKey rightKey jumpKey pressed key player =
  if key == leftKey then
    { player | leftPressed = pressed }
  else if key == rightKey then
    { player | rightPressed = pressed }
  else if key == jumpKey then
    { player | jumpPressed = pressed }
  else
    player

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

bounce : Float -> Float -> Mover a -> Mover a
bounce screenHeight bounciness ball =
  let
    (x, y) = ball.position
    (vx, vy) = ball.velocity
    bounceX =
      if x + ball.size > (ball.rightWallX) then
        -1.0 * (abs vx) * bounciness
      else if x - ball.size < ball.leftWallX then
        (abs vx) * bounciness
      else
        vx

    bounceY =
      if y + ball.size > screenHeight then
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
applyPlayerCollision : Float -> Controlled (Mover a) -> Explosive (Mover a) -> Explosive (Mover a)
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

applyMovementKeys : Controlled (Mover a) -> Controlled (Mover a)
applyMovementKeys player =
  case (player.leftPressed, player.rightPressed) of
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

applyJump : Controlled (Mover a) -> Controlled (Mover a)
applyJump player =
  if player.onGround && player.jumpPressed then
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

handleFloor : Float -> Mover a -> Mover a
handleFloor floorY mover =
  let
    (x, y) = mover.position
    (ax, ay) = mover.acceleration
    (vx, vy) = mover.velocity
    upperBoundY = floorY - mover.size
  in
    if y >= upperBoundY then
      { mover
        | position = (x, upperBoundY)
        , velocity = (vx, Basics.min 0 vy)
        , acceleration = (ax, Basics.min 0 ay)
        , onGround = True
      }
    else
      mover

handleWalls : Mover a -> Mover a
handleWalls mover =
  let
    (x, y) = mover.position
    (ax, ay) = mover.acceleration
    (vx, vy) = mover.velocity
    leftBoundX = mover.leftWallX + mover.size
    rightBoundX = mover.rightWallX - mover.size
  in
    if x >= rightBoundX then
      { mover
        | position = (rightBoundX, y)
        , velocity = (Basics.min 0 vx, vy)
        , acceleration = (Basics.min 0 ax, ay)
      }
    else if x <= leftBoundX then
      { mover
        | position = (leftBoundX, y)
        , velocity = (Basics.max 0 vx, vy)
        , acceleration = (Basics.max 0 ax, ay)
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
