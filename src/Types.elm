module Types exposing (..)

import Time exposing (Time)
import Keyboard
import Vector2 as V2 exposing (Vec2, Float2)

type alias Player =
  { position : Float2
  , velocity : Float2
  }

type alias Model =
  { paused : Bool
  , time : Time
  , delta : Time
  , screenWidth : Int
  , screenHeight : Int
  , netWidth : Int
  , netHeight : Int
  , player : Player
  -- data for each player: kinematic info, score
  -- data for ball: kinematic info, timer
  -- pressed keys
  }

type Msg
  = Tick Time
  | Pause
  | Resume Time
  | Press Keyboard.KeyCode
  | Release Keyboard.KeyCode
