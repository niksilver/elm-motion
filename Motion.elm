module Motion exposing (Motion, Model, makeUpdate, cue, isRunning)

import Time exposing (Time)

import Animation exposing (Animation, animation, animate)


type Motion
    = Cued (Time -> Animation)
    | Running Animation

type alias Model c =
    { circ : c
    , motion : Motion
    }

-- Return a function that will take a time and a model and update the model.
-- As an argument it needs to be given a function which takes a float
-- (which is in the animation from/to range) and the subject and
-- returns the updated subject

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

-- Create an animation that is ready to receive the first time tick

cue : (Animation -> Animation) -> Motion
cue aniFn =
    Cued (\time -> animation time |> aniFn)

isRunning : Motion -> Bool
isRunning motion =
    case motion of
        Running _ -> True
        Cued _ -> False

