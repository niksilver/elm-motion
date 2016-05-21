import Time exposing (Time, millisecond, second)

import Html.App as App
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (width, viewBox, cx, cy, r, fill)

import Animation exposing
    ( Animation, animation, animate
    , from, to, duration
    )

type alias Model =
    { x : Float
    , animation : Maybe Animation
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
    , animation = Nothing
    }

update : Msg -> Model -> (Model, Cmd Msg)
update (Tick time) model =
    case model.animation of
        Nothing ->
            let
                anim =
                    animation time |> from 100 |> to 300 |> duration (2 * second)
            in
                (Model time (Just anim), Cmd.none)
        Just anim ->
            (Model (animate time anim) (Just anim), Cmd.none)

view : Model -> Svg Msg
view model =
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
