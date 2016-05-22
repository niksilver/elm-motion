module Animator exposing (..)

import Time exposing (Time)
import Animation as Ani2

type alias Animator =
    { startSet: Bool
    , animation : Ani2.Animation
    }

animator : Animator
animator =
    { startSet = False
    , animation = Ani2.animation 0
    }

isStartSet : Animator -> Bool
isStartSet ator =
    ator.startSet

start : Time -> Animator -> Animator
start time ator =
    let
        ation = ator.animation
        ation' =
            Ani2.animation time
                |> Ani2.duration (Ani2.getDuration ation)
                |> Ani2.speed (Ani2.getSpeed ation)
                |> Ani2.delay (Ani2.getDelay ation)
                |> Ani2.ease (Ani2.getEase ation)
                |> Ani2.from (Ani2.getFrom ation)
                |> Ani2.to (Ani2.getTo ation)
    in
        { startSet = True
        , animation = ation'
        }

animate : Time -> Animator -> Float
animate time ator =
    Ani2.animate time ator.animation

duration : Time -> Animator -> Animator
duration time ator =
    { ator | animation = Ani2.duration time ator.animation }

from : Time -> Animator -> Animator
from time ator =
    { ator | animation = Ani2.from time ator.animation }

to : Time -> Animator -> Animator
to time ator =
    { ator | animation = Ani2.to time ator.animation }

