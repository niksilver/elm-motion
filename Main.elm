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

type alias Model c =
    { circ : c
    , motion : Motion
    }

type Msg = Tick Time

main = App.program
    { init = (initialModel, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

initialModel : Model Circle
initialModel =
    Model
        { x = 300 }
        (Cued (\t -> animation t |> from 100 |> to 300 |> duration (2 * second)))

update : Msg -> Model Circle -> (Model Circle, Cmd Msg)
update (Tick time) model =
    (update' time model, Cmd.none)

update' : Time -> Model Circle -> Model Circle
update' =
    makeUpdate (\newX circ -> { circ | x = newX })

makeUpdate : (Float -> c -> c) -> Time -> Model c -> Model c
makeUpdate updateVal time model =
    let
        -- update' is a function that really takes two arguments,
        -- but we omit them here because they're recorded for
        -- type-checking purposes in the higher-level signature
        --
        -- update' : Time -> Model c -> Model c

        update' =
            let
                ani' =
                    case model.motion of
                        Cued fn -> fn time
                        Running ani -> ani
                newVal = animate time ani'
                oldC = model.circ
                newC = updateVal newVal oldC
            in
                { circ = newC, motion = Running ani' }
    in
        update'

isRunning : Motion -> Bool
isRunning motion =
    case motion of
        Running _ -> True
        Cued _ -> False

view : Model Circle -> Svg Msg
view model =
    if (not <| isRunning model.motion) then
        svg [] []
    else
        let
            x = toString model.circ.x
        in
            svg
            [ width "500px"
            , viewBox "0 0 500 500"
            ]
            [ circle [ cx x, cy "200", r "20", fill "#aa0000" ] []
            ]

subscriptions : Model c -> Sub Msg
subscriptions model =
    Time.every (30 * millisecond) Tick

