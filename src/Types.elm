module Types exposing (..)

import Time exposing (Time)
import Keyboard


type alias Model =
  { paused : Bool
  , time : Time
  , delta : Time
  -- data for each player: kinematic info, score
  -- data for ball: kinematic info, timer
  -- size/position info for playing field and net
  -- pressed keys
  -- 
  }

type Msg
  = Tick Time
  | Pause
  | Resume Time
  | Press Keyboard.KeyCode
  | Release Keyboard.KeyCode
