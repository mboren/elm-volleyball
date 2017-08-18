module Types exposing (..)

import Animation exposing (Animation)
import Color
import Keyboard
import Time exposing (Time)
import Vector2 as V2 exposing (Float2, Vec2)


type alias Player =
    Controlled
        (Mover
            (MovementKeys
                { alive : Bool
                , score : Int
                , name : String
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
    | Options (Maybe ( PlayerSide, MovementKey ))


type alias Players a =
    { a
        | player1 : Player
        , player2 : Player
    }


type alias Settings a =
    { a
        | graphicsQuality : QualitySetting
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
    , graphicsQuality : QualitySetting
    }


type SettingsMsg
    = SetQuality QualitySetting


type Msg
    = Tick Time
    | StartGame
    | GoToPage Page
    | Press Keyboard.KeyCode
    | Release Keyboard.KeyCode
    | NewBallVelocity Float2
    | ToggleAi PlayerSide
    | TogglePause
    | PrepareToChangePlayerKey PlayerSide MovementKey
    | ChangePlayerKey Side MovementKey Keyboard.KeyCode
    | ChangeSetting SettingsMsg


type MovementKey
    = LeftKey
    | RightKey
    | JumpKey


type UiPrimitive
    = Polygon (List Float2) Side Color.Color (Maybe Msg)
    | Text Float TextAnchor ( Float, Float ) Side String (Maybe Msg)


type UiSettingState
    = Selected
    | NotSelected


type GridData
    = Main MainMenuElement
    | OptionsMenu OptionsMenuElement
    | Hud HudElement


type MainMenuElement
    = MainTitle
    | Button String Msg


type OptionsMenuElement
    = OptionsTitle
    | OptionLabel String
    | KeyChangeButton UiSettingState Player MovementKey PlayerSide
    | QualityButton UiSettingState QualitySetting
    | BackButton
    | InfoText String


type HudElement
    = PlayerName Player Side
    | Score Player Side
    | Controls Player Side
    | ControlsBackground Player Side
    | Toggle Side Player Side


type QualitySetting
    = Fancy
    | Fast


{-| indicates that we're referring specifically to the player on the right
-}
type alias PlayerSide =
    Side


type Side
    = Left
    | Right


type TextAnchor
    = Start
    | Middle
    | End


textAnchorToString : TextAnchor -> String
textAnchorToString textAnchor =
    case textAnchor of
        Start ->
            "start"

        Middle ->
            "middle"

        End ->
            "end"


qualitySettingToString : QualitySetting -> String
qualitySettingToString quality =
    case quality of
        Fancy ->
            "Fancy"

        Fast ->
            "Fast"
