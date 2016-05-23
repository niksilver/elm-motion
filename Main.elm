import Time exposing (Time, millisecond, second)

import Html.App as App
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (width, viewBox, cx, cy, r, fill)

import Animation exposing
    ( Animation, animation, animate
    , from, to, duration
    )

type alias Circle =
    { x : Float
    }

type Motion
    = Cued (Time -> Animation)
    | Running Animation

type alias Model =
    { c : Circle
    , motion : Motion
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
    { c = { x = 300 }
    , motion = Cued (\t -> animation t |> from 100 |> to 300 |> duration (2 * second))
    }

update : Msg -> Model -> (Model, Cmd Msg)
update (Tick time) model =
    let
        ani' =
            case model.motion of
                Cued fn -> fn time
                Running ani -> ani
        c = model.c
        c' = { c | x = animate time ani' }
        model' = { c = c', motion = Running ani' }
    in
        (model', Cmd.none)

isRunning : Motion -> Bool
isRunning motion =
    case motion of
        Running _ -> True
        Cued _ -> False

view : Model -> Svg Msg
view model =
    if (not <| isRunning model.motion) then
        svg [] []
    else
        let
            x = toString model.c.x
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

