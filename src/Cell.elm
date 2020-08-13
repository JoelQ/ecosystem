module Cell exposing (Cell(..), Fox, Rabbit, generator, isEmpty, isFox, isRabbit)

import Energy exposing (Energy)
import Random exposing (Generator)


type Cell
    = FoxCell Fox
    | RabbitCell Rabbit
    | Empty



-- QUERIES


isRabbit : Cell -> Bool
isRabbit cell =
    case cell of
        RabbitCell _ ->
            True

        _ ->
            False


isFox : Cell -> Bool
isFox cell =
    case cell of
        FoxCell _ ->
            True

        _ ->
            False


isEmpty : Cell -> Bool
isEmpty cell =
    case cell of
        Empty ->
            True

        _ ->
            False



-- FOX


type alias Fox =
    { energy : Energy }


initialFox : Fox
initialFox =
    { energy = Energy.fromInt 5 }



-- RABBIT


type alias Rabbit =
    { energy : Energy }


initialRabbit : Rabbit
initialRabbit =
    { energy = Energy.fromInt 5 }



-- RANDOM


generator : Generator Cell
generator =
    Random.weighted ( 1, FoxCell initialFox ) [ ( 2, RabbitCell initialRabbit ), ( 3, Empty ) ]
