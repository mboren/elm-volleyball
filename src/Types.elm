module Types exposing (..)

import Time exposing (Time)
import Keyboard
import Vector2 as V2 exposing (Vec2, Float2)
import Animation exposing (Animation)

type alias Player =
  Controlled (Mover { alive: Bool, score: Int })

type alias Controlled a =
  { a
  | leftPressed : Bool
  , rightPressed : Bool
  , jumpPressed : Bool
  , ai : Bool
  }
type alias Mover a =
  { a
  | position : Float2
  , velocity : Float2
  , maxVx : Float
  , acceleration : Float2
  , size : Float
  , onGround : Bool
  , leftWallX : Float
  , rightWallX : Float
  }

type ExplosiveStatus = Safe | Exploding | Exploded
type alias Explosive a =
  { a
  | countdown : Time
  , status : ExplosiveStatus
  , explosionRadius : Float
  , animation : Animation
  }

type Page = Title | Game

type alias Model =
  { paused : Bool
  , page : Page
  , time : Time
  , screenWidth : Float
  , screenHeight : Float
  , netWidth : Float
  , netHeight : Float
  , player1 : Player
  , player2 : Player
  , ball : Explosive (Mover {})
  }

type Msg
  = Tick Time
  | StartGame
  | Press Keyboard.KeyCode
  | Release Keyboard.KeyCode
  | NewBallVelocity Float2
  | TogglePlayer1Ai
  | TogglePlayer2Ai
  | TogglePause

type Side = Left | Right
