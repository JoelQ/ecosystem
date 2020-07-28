module Energy exposing
    ( Energy
    , add
    , fromInt
    , isGreaterThan
    , isPositive
    , subtract
    , sum
    , toInt
    )


type Energy
    = Energy Int



-- CONSTRUCTORS AND DESTRUCTORS


toInt : Energy -> Int
toInt (Energy e) =
    e


fromInt : Int -> Energy
fromInt =
    Energy



-- QUERIES


isPositive : Energy -> Bool
isPositive (Energy e) =
    e > 0


{-| Is the second number greater than the first. Meant to be used in a pipeline
like:

    (Energy 3) |> Energy.isGreaterThan (Energy 2)
    -- True

-}
isGreaterThan : Energy -> Energy -> Bool
isGreaterThan (Energy e1) (Energy e2) =
    e2 > e1



-- MATH


add : Energy -> Energy -> Energy
add (Energy e1) (Energy e2) =
    Energy (e1 + e2)


sum : List Energy -> Energy
sum energies =
    List.foldl add (Energy 0) energies


{-| Subtract the first number from the second one. Meant to be used in a
pipeline like:

    (Energy 3) |> Energy.subtract (Energy 2)
     -- (Energy 1)

-}
subtract : Energy -> Energy -> Energy
subtract (Energy e1) (Energy e2) =
    Energy (e2 - e1)
