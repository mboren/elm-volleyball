module State exposing (init, update, subscriptions)

import Time exposing (Time)
import AnimationFrame
import Task
import Keyboard
import Random exposing (pair, float)
import Animation exposing (animation, from, to, duration)

import Vector2 as V2 exposing (Vec2, Float2)
import Types exposing (..)

-- calculated based on screen dimensions and gravity value
speedLimit = 0.41739935579996074
jumpSpeed = -0.6708203932499369
ballVyLimit = 0.714142842854285

friction = 0.6
playerAccelX = 0.05

defaultBall : Explosive (Mover {})
defaultBall =
  { position = (1000/2, 600/3)
  , velocity = (0, 0)
  , acceleration = (0, 0)
  , size = 20
  , onGround = False
  , leftWallX = 0
  , rightWallX = 1000
  , countdown = 10 * Time.second
  , status = Safe
  , explosionRadius = 100
  , animation = Animation.static 20
  }

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
      , alive = True
      , score = 0
      , ai = False
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
      , alive = True
      , score = 0
      , ai = True
      }
  in
    (Model False 0 1000 600 10 250 p1 p2 defaultBall)
    ! [ Random.generate NewBallVelocity velocityGenerator ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      (model |> updateScores |> revivePlayers, Random.generate NewBallVelocity velocityGenerator)

    NewBallVelocity v ->
      let
        newBall = { defaultBall | velocity = v }
      in
        ({ model | ball = newBall }, Cmd.none)

    Tick dt ->
      { model
          | player1 = playerStep dt (toFloat model.screenHeight) (aiMovement model model.player1)
          , player2 = playerStep dt (toFloat model.screenHeight) (aiMovement model model.player2)
          , ball = ballStep dt model model.ball
          , time = model.time + dt
        }
        |> handleExplosionCasualties
        |> resetAtEndOfRound

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

    TogglePlayer1Ai ->
      let
        p = model.player1
      in
        ({ model
          | player1 =
            { p
            | ai = not p.ai
            , leftPressed = False
            , rightPressed = False
            , jumpPressed = False
            }
         }, Cmd.none)

    TogglePlayer2Ai ->
      let
        p = model.player2
      in
        ({ model
          | player2 =
            { p
            | ai = not p.ai
            , leftPressed = False
            , rightPressed = False
            , jumpPressed = False
            }
         }, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
  case model.paused of
    True ->
      Sub.none
    False ->
      Sub.batch
        [ AnimationFrame.diffs Tick
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

ballStep : Time -> Model -> Explosive (Mover {}) -> Explosive (Mover {})
ballStep dt model ball =
  case ball.status of
    Safe ->
      ball
        |> applyGravity
        |> bounce (toFloat model.screenHeight) 0.9
        |> applyPlayerCollision (toFloat model.screenWidth) model.player1
        |> applyPlayerCollision (toFloat model.screenWidth) model.player2
        |> updatePosition model.screenHeight dt
        |> handleNet model
        |> updateCountdown dt
        |> detectDetonation model.time
    Exploding ->
      model.ball
        |> handleExplosionAnimation model.time
    Exploded ->
      model.ball

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
applyPlayerCollision : Float -> Controlled (Mover a) -> Explosive (Mover b) -> Explosive (Mover b)
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
          |> V2.scale 1.6
          |> V2.add ball.velocity
          |> \(vx,vy)->(vx, clamp (-ballVyLimit) ballVyLimit vy)
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
      if player.onGround then
        let
          (vx, vy) = player.velocity
        in
          { player | velocity = (friction * vx, vy) }
      else
        player

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

handleNet : Model -> Explosive (Mover a) -> Explosive (Mover a)
handleNet {screenWidth, screenHeight, netWidth, netHeight} ball =
  let
    (x,y) = ball.position
    (vx, vy) = ball.velocity
    nx = toFloat ((screenWidth // 2) - (netWidth // 2))
    ny = toFloat (screenHeight - netHeight)
  in
    -- This just checks if the center of the ball is within the net.
    -- I think more sophisticated collision detection would not look
    -- sufficiently goofy.
    if (y >= ny) && (x >= nx) && (x <= nx + (toFloat netWidth)) then
      { ball
        -- vx is reflected based on which side of the screen it's on.
        -- This keeps the ball from getting stuck.
        | velocity =
          if x <= toFloat (screenWidth // 2) then
            (-vx, -vy)
          else
            (vx, -vy)
      }
    else
      ball

{- Calculate change in position, velocity, and acceleration for this frame.
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

aiMovement : Model -> Controlled (Mover a) -> Controlled (Mover a)
aiMovement {ball} player =
  if player.ai then
    let
      (px, py) = player.position
      (bx, by) = ball.position
    in
      { player
        | leftPressed = px > bx
        , rightPressed = px < bx
        , jumpPressed = py < by
      }
  else
    player

velocityGenerator : Random.Generator Float2
velocityGenerator =
  let
    component = float (-speedLimit) speedLimit
  in
    (pair component component)

checkCollision : Float2 -> Float -> Float2 -> Float -> Bool
checkCollision center1 radius1 center2 radius2 =
  let
    minimumDistance = radius1 + radius2
    distance = V2.distance center1 center2
  in
    distance <= minimumDistance

{- Kill player if they are within explosion radius,
   otherwise, increment their score
-}
explosionCasualtyHelper : Mover a -> Player -> Player
explosionCasualtyHelper {position, size} player =
  if checkCollision player.position player.size position size then
    kill player
  else
    player

handleExplosionCasualties : Model -> Model
handleExplosionCasualties model =
  case model.ball.status of
    Exploding ->
      let
        ball = model.ball
        newStatus =
          if Animation.isDone model.time model.ball.animation then
            Exploded
          else
            Exploding

        newBall = { ball | status = newStatus }
      in
        { model
          | player1 = explosionCasualtyHelper ball model.player1
          , player2 = explosionCasualtyHelper ball model.player2
          , ball = newBall
        }

    _ ->
      model
handleExplosionAnimation : Time -> Explosive (Mover a) -> Explosive (Mover a)
handleExplosionAnimation time ball =
  { ball | size = Animation.animate time ball.animation }

kill : Player -> Player
kill player =
  if player.alive then
    { player | alive = False }
  else
    player

revive : Player -> Player
revive player =
  { player | alive = True }

revivePlayers : Model -> Model
revivePlayers model =
  { model
    | player1 = revive model.player1
    , player2 = revive model.player2
  }

addPoints : Int -> Player -> Player
addPoints points player =
  { player | score = player.score + points }

{-
Increment each player's score if they are the sole survivor
of a round.
If both are dead or both are alive, nobody gets a point
-}
updateScores : Model -> Model
updateScores model =
  let
    (p1Points, p2Points) =
      case (model.player1.alive, model.player2.alive) of
        (True, False) ->
          (1, 0)
        (False, True) ->
          (0, 1)
        _ ->
          (0, 0)
  in
    { model
      | player1 = (addPoints p1Points model.player1)
      , player2 = (addPoints p2Points model.player2)
    }

resetAtEndOfRound : Model -> (Model, Cmd Msg)
resetAtEndOfRound model =
  case model.ball.status of
    Exploded ->
      (model
        |> updateScores
        |> revivePlayers, Random.generate NewBallVelocity velocityGenerator)
    _ ->
      (model, Cmd.none)
