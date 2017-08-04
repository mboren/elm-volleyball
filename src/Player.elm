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
    playerSize = 50
    waistYOffset = playerSize

    defaultLeftArm =
      { shoulder = (-0.7 * playerSize, 0)
      , resting = (-playerSize, playerSize / 2)
      , hand = (0,0)
      , length = 1.5 * playerSize
      , activationRange = 3 * playerSize
      }

    defaultRightArm =
      { defaultLeftArm
        | shoulder = (0.7 * playerSize, 0)
        , resting = (playerSize, playerSize / 2)
      }
  in
    { position = (x, y)
    , velocity = (0, 0)
    , maxVx = speedLimit
    , acceleration = (0, 0)
    , size = playerSize
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
    , waistY = waistYOffset
    , legHeight = playerSize / 2
    , fixedLegX = 0
    , freeLegX = 0
    , leftArm = defaultLeftArm
    , rightArm = defaultRightArm
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
      dist = abs (px - bx)
    in
      { player
        | leftPressed = px > bx && dist > player.size
        , rightPressed = px < bx && dist > player.size
        , jumpPressed = dist < 200 && by < 200
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

{-
Update leg X positions based on current player position
-}
updateLegs : Player -> Player
updateLegs player =
  let
    (px, _) = player.position

    wallToWallDistance = player.rightWallX - player.leftWallX
    numStridesFromWallToWall = 4
    strideLength = wallToWallDistance / numStridesFromWallToWall

    -- The player will only put their foot down at multiples of strideLength
    previousStrideX =
      strideLength * (toFloat (floor (px / strideLength)))

    nextStrideX =
      strideLength * (toFloat (ceiling (px / strideLength)))

    previousDistance = px - previousStrideX
    nextDistance = nextStrideX - px

    -- The fixed leg sticks to the closest X value which is a
    -- multiple of strideLength,
    -- and the free leg moves proportionally to the distance from
    -- this X value.
    (newFixedLegX, newFreeLegX) =
      if player.onGround then
        if previousDistance < nextDistance then
          (previousStrideX, previousStrideX + 2 * previousDistance)
        else
          (nextStrideX, nextStrideX - 2 * nextDistance)
      else
        (px - strideLength/2, px + strideLength/2)
  in
    { player
      | fixedLegX = newFixedLegX
      , freeLegX = newFreeLegX
    }

updateArms : Float2 -> Player -> Player
updateArms target player =
  let
    newLeft = updateHandPosition target player player.leftArm
    newRight = updateHandPosition target player player.rightArm
  in
    { player
      | leftArm = newLeft
      , rightArm = newRight
    }

updateHandPosition : Float2 -> Player -> Arm -> Arm
updateHandPosition target player arm =
  let
    shoulderToTarget =
      player.position
        |> V2.add arm.shoulder
        |> V2.sub target

    restingPositionToTarget =
      player.position
        |> V2.add arm.shoulder
        |> V2.add arm.resting
        |> V2.sub target

    distance = V2.length restingPositionToTarget

    targetDelta = V2.sub target player.position

    (absoluteHandX, absoluteHandY) =
      if distance < arm.length then
        target
      else
        {- We move the arm toward the ball before the ball gets close enough
           to hit, because otherwise the movement appears really abrupt.
           An alternative approach would be to give the hands velocity/acceleration
           toward the ball, but this is much simpler and looks pretty good
           (and by good i mean really goofy)
        -}
        if distance < arm.activationRange then
          shoulderToTarget
            |> V2.normalize
            |> V2.scale arm.length
            |> V2.add arm.shoulder
            |> V2.add player.position
        else
          player.position
            |> V2.add arm.shoulder
            |> V2.add arm.resting

    -- when the player moves next to a wall, this makes the hand scoot up the wall
    -- a bit instead of just disappearing. Makes the player feel more lively.
    wallCompensatedHandPosition =
      if absoluteHandX <= player.leftWallX then
        (player.leftWallX + 5, absoluteHandY - 2 * (player.leftWallX - absoluteHandX))
      else
        if absoluteHandX >= player.rightWallX then
          (player.rightWallX + 5, absoluteHandY - 2 * (absoluteHandX - player.rightWallX))
        else
          (absoluteHandX, absoluteHandY)

    absoluteShoulderPosition =
      V2.add arm.shoulder player.position

    shoulderToHand =
      V2.sub wallCompensatedHandPosition absoluteShoulderPosition
  in
    { arm | hand = shoulderToHand }
