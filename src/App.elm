module App exposing (main)

import Html
import Types
import View
import State

main : Program Never Types.Model Types.Msg
main =
  Html.program
    { init = State.init
    , update = State.update
    , subscriptions = State.subscriptions
    , view = View.view
    }