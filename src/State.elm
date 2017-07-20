module State exposing (init, update, subscriptions)

import Types exposing (..)

init : (Model, Cmd Msg)
init =
  (Model True 0 0 1000 600, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none