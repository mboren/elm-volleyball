module State exposing (init, update, subscriptions)

import Time exposing (Time)
import AnimationFrame
import Task
import Keyboard
import Random exposing (pair, float)
import Animation exposing (animation, from, to, duration)

import Vector2 as V2 exposing (Vec2, Float2)
import Types exposing (..)

gravity = 0.001
friction = 0.6
playerAccelX = 0.05

-- calculated based on screen dimensions and gravity value
speedLimit = 0.41739935579996074
jumpSpeed = -0.6708203932499369
ballVxLimit = 0.514142842854285
ballVyLimit = 0.714142842854285


warmupLength : Time
warmupLength = 3 * Time.second

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
  { position = (1000/2, 600/3)
  , velocity = (0, 0)
  , maxVx = ballVxLimit
  , acceleration = (0, 0)
  , size = 20
  , onGround = False
  , leftWallX = 0
  , rightWallX = 1000
  , countdown = 10 * Time.second
  , status = Safe
  , explosionRadius = 250
  , animation = Animation.static 20
  }

constructPlayer : Layout a -> MovementKeys a -> Bool -> Side -> Player
constructPlayer {screenWidth, screenHeight, netWidth} {leftKey, rightKey, jumpKey} ai side =
  let
    (leftWallX, rightWallX) =
      case side of
        Left ->
          (0, (screenWidth - netWidth) / 2)
        Right ->
          ((screenWidth + netWidth) / 2, screenWidth)

    x = (leftWallX + rightWallX) / 2
    y = screenHeight / 3
  in
    { position = (x, y)
    , velocity = (0, 0)
    , maxVx = speedLimit
    , acceleration = (0, 0)
    , size = 50
    , onGround = False
    , leftWallX = leftWallX
    , rightWallX = rightWallX
    , leftPressed = False
    , rightPressed = False
    , jumpPressed = False
    , leftKey = leftKey
    , rightKey = rightKey
    , jumpKey = jumpKey
    , alive = True
    , score = 0
    , ai = ai
    }

init : (Model, Cmd Msg)
init =
  let
    layout =
      { screenWidth = 1000
      , screenHeight = 600
      , netWidth = 10
      , netHeight = 250
      }
    p1 = constructPlayer layout defaultPlayer1Controls False Left
    p2 = constructPlayer layout defaultPlayer2Controls True Right
  in
    ( ( Model
        False
        Title
        0
        warmupLength
        layout.screenWidth
        layout.screenHeight
        layout.netWidth
        layout.netHeight
        p1 p2
        defaultBall
      )
    , Random.generate NewBallVelocity velocityGenerator
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartGame ->
        ( { model | page = Game, paused = False }
            |> mapPlayers revive
            |> mapPlayers resetScore
        , Random.generate NewBallVelocity velocityGenerator
        )

    NewBallVelocity v ->
      let
        newBall = { defaultBall | velocity = v }
      in
        ({ model | ball = newBall }, Cmd.none)

    Tick dt ->
      { model
        | ball = ballStep dt model model.ball
        , time = model.time + dt
        , warmupTimer = max 0 (model.warmupTimer - dt)
      }
        |> mapPlayers (playerStep dt model.screenHeight model.ball)
        |> resetAtEndOfRound

    Press key ->
      ( model
        |> mapPlayers (handleKey True key)
      , Cmd.none
      )

    Release key ->
      ( { model | paused = (xor model.paused (key == 32)) }
          |> mapPlayers (handleKey False key)
      , Cmd.none
      )

    TogglePlayer1Ai ->
      ( { model | player1 = toggleAi model.player1 }
      , Cmd.none
      )

    TogglePlayer2Ai ->
      ( { model | player2 = toggleAi model.player2 }
      , Cmd.none
      )

    TogglePause ->
      ({ model | paused = not model.paused }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.page of
    Title ->
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

playerStep : Time -> Float -> Explosive (Mover b) -> Player -> Player
playerStep dt screenHeight ball player =
  player
    |> applyGravity
    |> applyMovementKeys
    |> aiMovement ball
    |> applyJump
    |> updatePosition screenHeight dt
    |> handleWalls
    |> handleFloor screenHeight
    |> killIfInExplosion ball

ballStep : Time -> Model -> Explosive (Mover {}) -> Explosive (Mover {})
ballStep dt model ball =
  case ball.status of
    Safe ->
      if model.warmupTimer <= 0 then
        ball
          |> adjustBallBounds model
          |> applyGravity
          |> bounce model.screenHeight 0.9
          |> applyPlayerCollision model.screenWidth model.player1
          |> applyPlayerCollision model.screenWidth model.player2
          |> updatePosition model.screenHeight dt
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

handleKey : Bool -> Keyboard.KeyCode -> MovementKeys (Controlled a) -> MovementKeys (Controlled a)
handleKey pressed key player =
  if key == player.leftKey then
    { player | leftPressed = pressed }
  else if key == player.rightKey then
    { player | rightPressed = pressed }
  else if key == player.jumpKey then
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
  if not player.onGround then
    player
      |> addAcceleration (0, gravity)
  else
    player

bounce : Float -> Float -> Mover a -> Mover a
bounce screenHeight bounciness ball =
  let
    (x, y) = ball.position
    (vx, vy) = ball.velocity
    (bounceVx, newX) =
      if x + ball.size > (ball.rightWallX) then
        ( -1.0 * (abs vx) * bounciness
        , ball.rightWallX - ball.size
        )
      else if x - ball.size < ball.leftWallX then
        ( (abs vx) * bounciness
        , ball.leftWallX + ball.size
        )
      else
        (vx, x)

    bounceVy =
      if y + ball.size > screenHeight then
        -1.0 * (abs vy) * bounciness
      else if y - ball.size < 0 then
        (abs vy) * bounciness
      else
        vy
  in
    { ball | velocity = (bounceVx, bounceVy), position = (newX, y) }

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

{-
Split the screen into 3 regions based on the net position,
and change leftWallX and rightWallX depending on which region
the ball is in
          |             |
          |     top     |
          |right | left |
          |______|______|
-}
adjustBallBounds : Layout a -> Explosive (Mover {}) -> Explosive (Mover {})
adjustBallBounds {screenWidth, screenHeight, netWidth, netHeight} ball =
  let
    (x, y) = ball.position

    netX =
      if x < screenWidth / 2 then
        (screenWidth / 2) - (netWidth / 2)
      else
        (screenWidth / 2) + (netWidth / 2)

    netY = screenHeight - netHeight

    (newLeftWall, newRightWall) =
      if (y + ball.size) < netY then
        -- above net
        (0, screenWidth)
      else
        if x < screenWidth / 2 then
          -- left of net
          (0, netX)
        else
          if x > screenWidth / 2 then
            -- right of net
            (netX, screenWidth)
          else
            -- if we're precisely in the middle, crash.
            -- I want to see how common this case is to determine
            -- how much effort to put into resolving it nicely.
            Debug.crash "ball in middle" (0, screenWidth)
  in
    { ball
      | leftWallX = newLeftWall
      , rightWallX = newRightWall
    }


{- Calculate change in position, velocity, and acceleration for this frame.
   Acceleration is zeroed after it is applied.
-}
updatePosition : Float -> Time -> Mover a -> Mover a
updatePosition screenHeight dt player =
  let
    -- v = v0 + a * t
    newVelocity =
      player.acceleration
        |> V2.scale dt
        |> V2.add player.velocity
        |> clampX (-1 * player.maxVx) player.maxVx

    -- r = r0 + 0.5 * t * (v + v0)
    newPosition =
      player.velocity
        |> V2.add newVelocity
        |> V2.scale (0.5 * dt)
        |> V2.add player.position

    newOnGround = (V2.getY player.position) + player.size >= screenHeight

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

aiMovement : Mover b -> Controlled (Mover a) -> Controlled (Mover a)
aiMovement ball player =
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

toggleAi : Player -> Player
toggleAi player =
  { player
    | ai = not player.ai
    , leftPressed = False
    , rightPressed = False
    , jumpPressed = False
  }

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

killIfInExplosion : Explosive (Mover a) -> Player -> Player
killIfInExplosion {position, size, status} player =
  if status == Exploding && checkCollision player.position player.size position size then
    kill player
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

kill : Player -> Player
kill player =
  if player.alive then
    { player | alive = False }
  else
    player

revive : Player -> Player
revive player =
  { player | alive = True }

resetScore : Player -> Player
resetScore player =
  { player | score = 0 }

addPoints : Int -> Player -> Player
addPoints points player =
  { player | score = player.score + points }

{-
Increment each player's score if they are the sole survivor
of a round.
If both are dead or both are alive, nobody gets a point
-}
updateScores : Players a -> Players a
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
      ( { model | warmupTimer = warmupLength }
          |> updateScores
          |> mapPlayers (revive)
      , Random.generate NewBallVelocity velocityGenerator
      )
    _ ->
      (model, Cmd.none)

mapPlayers : (Player -> Player) -> Players a -> Players a
mapPlayers f players =
  { players
    | player1 = f players.player1
    , player2 = f players.player2
  }
