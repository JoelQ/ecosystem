module Grid exposing
    ( EnergyStat
    , Grid
    , empty
    , energyStats
    , generator
    , isSafe
    , move
    , nearbyEmpties
    , nearbyRabbits
    , nearbySafeEmpties
    , populations
    , setEmpty
    )

import Cell exposing (Cell(..))
import CellGrid exposing (CellGrid, Position)
import CellGrid.Extra as CellGrid
import Energy exposing (Energy)
import Random exposing (Generator)


type alias Grid =
    CellGrid Cell



-- CONSTRUCTORS


empty : Grid
empty =
    CellGrid.repeat dimensions Empty


fromList : List Cell -> Grid
fromList =
    Maybe.withDefault empty << CellGrid.fromList dimensions


dimensions : CellGrid.Dimensions
dimensions =
    { rows = 10, columns = 10 }



-- RANDOM


generator : Generator Grid
generator =
    Random.list (dimensions.columns * dimensions.rows) Cell.generator
        |> Random.map fromList



-- OPERATIONS


{-| Moving an entity requires two operations on a grid:

1.  Setting the original position to "Empty"
2.  Inserting the entity at the new position

Wrapping this up in a move function makes it harder to accidentally delete or
duplicate the entity by forgetting either of the required steps

-}
move : { from : Position, to : Position } -> Cell -> Grid -> Grid
move { from, to } cell grid =
    grid
        |> setEmpty from
        |> CellGrid.set to cell


setEmpty : Position -> Grid -> Grid
setEmpty position grid =
    CellGrid.set position Empty grid



-- QUERIES


nearbyRabbits : Position -> Grid -> List ( Position, Cell )
nearbyRabbits =
    neighborsWhere (Cell.isRabbit << Tuple.second)


nearbyFoxes : Position -> Grid -> List ( Position, Cell )
nearbyFoxes =
    neighborsWhere (Cell.isFox << Tuple.second)


nearbyEmpties : Position -> Grid -> List ( Position, Cell )
nearbyEmpties =
    neighborsWhere (Cell.isEmpty << Tuple.second)


nearbySafeEmpties : Position -> Grid -> List ( Position, Cell )
nearbySafeEmpties position grid =
    neighborsWhere (isSafeEmpty grid) position grid


neighborsWhere : (( Position, Cell ) -> Bool) -> Position -> Grid -> List ( Position, Cell )
neighborsWhere filterFunc position grid =
    grid
        |> CellGrid.neighborsWithPositions position
        |> List.filter filterFunc


isSafeEmpty : Grid -> ( Position, Cell ) -> Bool
isSafeEmpty grid ( position, cell ) =
    Cell.isEmpty cell && isSafe position grid


isSafe : Position -> Grid -> Bool
isSafe position grid =
    grid
        |> nearbyFoxes position
        |> List.isEmpty



-- STATS


populations : Grid -> { rabbits : Int, foxes : Int }
populations grid =
    CellGrid.foldl
        (\cell count ->
            case cell of
                Empty ->
                    count

                RabbitCell _ ->
                    { count | rabbits = count.rabbits + 1 }

                FoxCell _ ->
                    { count | foxes = count.foxes + 1 }
        )
        { rabbits = 0, foxes = 0 }
        grid


type alias EnergyStat =
    { rabbits : Energy, foxes : Energy }


energyStats : Grid -> EnergyStat
energyStats grid =
    CellGrid.foldl
        (\cell stats ->
            case cell of
                Empty ->
                    stats

                FoxCell fox ->
                    { stats | foxes = Energy.add stats.foxes fox.energy }

                RabbitCell rabbit ->
                    { stats | rabbits = Energy.add stats.rabbits rabbit.energy }
        )
        { rabbits = Energy.zero, foxes = Energy.zero }
        grid
