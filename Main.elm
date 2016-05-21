import Time exposing (Time, millisecond)

import Html.App as App
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (width, viewBox, cx, cy, r, fill)

type alias Model = Time

type Msg = Tick Time

main = App.program
    { init = (0, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time -> (time, Cmd.none)

view : Model -> Svg Msg
view model =
    svg
    [ width "500px"
    , viewBox "0 0 500 500"
    ]
    [ circle [ cx "200", cy "200", r "20", fill "#aa0000" ] []
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (30 * millisecond) Tick
