module State exposing (init, update, subscriptions)

import Time exposing (Time)
import Task

import Types exposing (..)

init : (Model, Cmd Msg)
init =
  let
    p1 = Player (1000/4,600/3) (0,0)
  in
    (Model True 0 0 1000 600 10 250 p1, Task.perform Resume Time.now)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Resume startTime ->
      ({ model
        | paused = False
        , time = startTime
        , delta = 0
       }
      , Cmd.none)
    Tick newTime ->
      let
        dt = newTime - model.time
      in
        ( { model
            | time = newTime
            , delta = dt
          }
        , Cmd.none
        )

    _ ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.paused of
    True ->
      Sub.none
    False ->
      Time.every (100 * Time.millisecond) Tick
