module Mover exposing (..)

import Time exposing (Time)
import Vector2 as V2 exposing (Vec2, Float2)

import Types exposing (Mover)

gravity = 0.001

addAcceleration : Float2 -> Mover a -> Mover a
addAcceleration a mover =
  { mover
    | acceleration = (V2.add a mover.acceleration)
  }

applyGravity : Mover a -> Mover a
applyGravity mover =
  if not mover.onGround then
    mover
      |> addAcceleration (0, gravity)
  else
    mover

stopAtFloor : Float -> Mover a -> Mover a
stopAtFloor floorY mover =
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

bounceOffWalls : Float -> Float -> Mover a -> Mover a
bounceOffWalls screenHeight bounciness mover =
  let
    (x, y) = mover.position
    (vx, vy) = mover.velocity
    (bounceVx, newX) =
      if x + mover.size > (mover.rightWallX) then
        ( -1.0 * (abs vx) * bounciness
        , mover.rightWallX - mover.size
        )
      else if x - mover.size < mover.leftWallX then
        ( (abs vx) * bounciness
        , mover.leftWallX + mover.size
        )
      else
        (vx, x)
  in
    { mover | velocity = (bounceVx, vy), position = (newX, y) }

stopAtWalls : Mover a -> Mover a
stopAtWalls mover =
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