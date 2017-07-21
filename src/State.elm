module State exposing (init, update, subscriptions)

import Types exposing (..)

init : (Model, Cmd Msg)
init =
  let
    p1 = Player (1000/4,600/3) (0,0)
  in
    (Model True 0 0 1000 600 10 250 p1, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none