import Time exposing (Time, millisecond, second)

import Html.App as App
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (width, viewBox, cx, cy, r, fill)

import Animator exposing
    ( Animator, animator, animate
    , start, isStartSet
    , from, to, duration
    )

type alias Model =
    { x : Float
    , animator : Animator
    }

type Msg = Tick Time

main = App.program
    { init = (initialModel, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

initialModel : Model
initialModel =
    { x = 0
    , animator = animator |> from 100 |> to 300 |> duration (2 * second)
    }

update : Msg -> Model -> (Model, Cmd Msg)
update (Tick time) model =
    let
        model' =
            if (not <| isStartSet model.animator) then
                { model | animator = start time model.animator }
            else
                model
    in
        ({ model' | x = animate time model.animator }
        , Cmd.none
        )

view : Model -> Svg Msg
view model =
    if (not <| isStartSet model.animator) then
        svg [] []
    else
        let
            x = toString model.x
        in
            svg
            [ width "500px"
            , viewBox "0 0 500 500"
            ]
            [ circle [ cx x, cy "200", r "20", fill "#aa0000" ] []
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (30 * millisecond) Tick
