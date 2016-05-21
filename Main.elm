import Time exposing (Time, millisecond, second)

import Html.App as App
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (width, viewBox, cx, cy, r, fill)

import Animation exposing
    ( Animation, animation, animate
    , from, to, duration
    )

type alias Model =
    Maybe
        { x : Float
        , animation : Animation
        }

type Msg = Tick Time

main = App.program
    { init = (Nothing, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

update : Msg -> Model -> (Model, Cmd Msg)
update (Tick time) model =
    case model of
        Nothing ->
            let
                anim =
                    animation time |> from 100 |> to 300 |> duration (2 * second)
            in
                (Just {x = animate time anim, animation = anim}, Cmd.none)
        Just struct ->
            (Just
                { x = animate time struct.animation
                , animation = struct.animation
                }
            , Cmd.none
            )

view : Model -> Svg Msg
view model =
    case model of
        Nothing -> svg [] []
        Just struct ->
            let
                x = toString struct.x
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
