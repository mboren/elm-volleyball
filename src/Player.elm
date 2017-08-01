module Player exposing (..)

import Keyboard
import Vector2 as V2 exposing (Float2)

import Types exposing (..)
import Mover

friction = 0.6
playerAccelX = 0.05
speedLimit = 0.41739935579996074
jumpSpeed = -0.6708203932499369


create : Layout a -> MovementKeys a -> Bool -> Side -> Player
create {screenWidth, screenHeight, netWidth} {leftKey, rightKey, jumpKey} ai side =
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
    , onGround= False
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

toggleAi : Player -> Player 
toggleAi player =
  { player 
    | ai = not player.ai
    , leftPressed = False
    , rightPressed = False
    , jumpPressed = False
  }

-- MOVEMENT
aiMovement : Mover a -> Player -> Player
aiMovement ball player =
  if player.ai then
    let
      (px, py) = player.position
      (bx, by) = ball.position
    in
      { player
        | leftPressed = px > bx
        , rightPressed = px < bx
        , jumpPressed = True
      }
  else
    player

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

applyMovementKeys : Player -> Player
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
      player |> Mover.addAcceleration (playerAccelX, 0)

    -- left
    (True, False) ->
      player |> Mover.addAcceleration (-1 * playerAccelX, 0)

    (True, True) ->
      player

applyJump : Player -> Player
applyJump player =
  if player.onGround && player.jumpPressed then
    { player | velocity = player.velocity |> V2.setY jumpSpeed }
  else
    player
