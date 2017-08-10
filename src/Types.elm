module Types exposing (..)

import Animation exposing (Animation)
import Keyboard
import Time exposing (Time)
import Vector2 as V2 exposing (Float2, Vec2)


type alias Player =
    Controlled
        (Mover
            (MovementKeys
                { alive : Bool
                , score : Int
                , waistY : Float
                , legHeight : Float
                , fixedLegX : Float
                , freeLegX : Float
                , leftArm : Arm
                , rightArm : Arm
                }
            )
        )


type alias MovementKeys a =
    { a
        | leftKey : Keyboard.KeyCode
        , rightKey : Keyboard.KeyCode
        , jumpKey : Keyboard.KeyCode
    }


type alias Arm =
    { shoulder : Float2
    , hand : Float2
    , resting : Float2
    , length : Float
    , activationRange : Float
    }


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


type ExplosiveStatus
    = Safe
    | Exploding
    | Exploded


type alias Explosive a =
    { a
        | countdown : Time
        , status : ExplosiveStatus
        , explosionRadius : Float
        , animation : Animation
    }


type alias Layout a =
    { a
        | screenWidth : Float
        , screenHeight : Float
        , netWidth : Float
        , netHeight : Float
    }


type Page
    = Title
    | Instructions
    | Game
    | Options (Maybe ( Side, MovementKey ))


type alias Players a =
    { a
        | player1 : Player
        , player2 : Player
    }


type alias Settings a =
    { a
        | useFancyExplosion : Bool
    }


type alias Model =
    { paused : Bool
    , gameStarted : Bool
    , page : Page
    , time : Time
    , warmupTimer : Time
    , screenWidth : Float
    , screenHeight : Float
    , netWidth : Float
    , netHeight : Float
    , player1 : Player
    , player2 : Player
    , ball : Explosive (Mover {})
    , useFancyExplosion : Bool
    }


type SettingsMsg
    = ToggleFancyExplosion


type Msg
    = Tick Time
    | StartGame
    | GoToPage Page
    | Press Keyboard.KeyCode
    | Release Keyboard.KeyCode
    | NewBallVelocity Float2
    | TogglePlayer1Ai
    | TogglePlayer2Ai
    | TogglePause
    | PrepareToChangePlayerKey Side MovementKey
    | ChangePlayerKey Side MovementKey Keyboard.KeyCode
    | ChangeSetting SettingsMsg


type MovementKey
    = LeftKey
    | RightKey
    | JumpKey


type Side
    = Left
    | Right
