module Types exposing (..)

import Time exposing (Time)
import Keyboard
import Vector2 as V2 exposing (Vec2, Float2)

type alias Player =
  Controlled (Mover { alive: Bool, score: Int })

type alias Controlled a =
  { a
  | leftPressed : Bool
  , rightPressed : Bool
  , jumpPressed : Bool
  }
type alias Mover a =
  { a
  | position : Float2
  , velocity : Float2
  , acceleration : Float2
  , size : Float
  , onGround : Bool
  , leftWallX : Float
  , rightWallX : Float
  }

type alias Explosive a =
  { a
  | countdown : Time
  , exploding : Bool
  , explosionRadius : Float
  }

type alias Model =
  { paused : Bool
  , time : Time
  , delta : Time
  , screenWidth : Int
  , screenHeight : Int
  , netWidth : Int
  , netHeight : Int
  , player1 : Player
  , player2 : Player
  , ball : Explosive (Mover {})
  -- data for each player: score
  }

type Msg
  = Tick Time
  | Pause
  | Resume Time
  | Press Keyboard.KeyCode
  | Release Keyboard.KeyCode
  | Reset
  | NewBallVelocity Float2
