module CellGrid.Extra exposing
    ( foldlWithPosition
    , neighborsWithPositions
    , withPositions
    )

import CellGrid exposing (CellGrid, Position)


foldlWithPosition : (Position -> a -> b -> b) -> b -> CellGrid a -> b
foldlWithPosition stepFunc initial grid =
    grid
        |> withPositions
        |> CellGrid.foldl (\( pos, item ) acc -> stepFunc pos item acc) initial


neighborsWithPositions : Position -> CellGrid a -> List ( Position, a )
neighborsWithPositions position grid =
    grid
        |> withPositions
        |> CellGrid.neighbors position


withPositions : CellGrid a -> CellGrid ( Position, a )
withPositions grid =
    CellGrid.indexedMap (\x y item -> ( Position x y, item )) grid
