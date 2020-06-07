module CellGrid exposing
    ( CellGrid(..), Dimensions, Position
    , empty, initialize, repeat, fromList
    , map, indexedMap
    , foldl, foldr, transform
    , get, set, update
    , adjacent, neighbors
    , CellType(..), classifyCell
    , arrayIndex, matrixIndex, matrixIndices
    , toLists, incrementing
    )

{-| The CellGrid package provides a type for representing
a rectangular grid of cells. CellGrids can be created,
transformed, and rendered with either SVG or WebGL.


## Type

@docs CellGrid, Dimensions, Position


## Constructing and rendering CellGrids

@docs empty, initialize, repeat, fromList


## Array-like functions

@docs map, indexedMap
@docs foldl, foldr, transform


## Individual cell manipulation

@docs get, set, update
@docs adjacent, neighbors


## Cell types

@docs CellType, classifyCell


## Index helpers

@docs arrayIndex, matrixIndex, matrixIndices


## Documentation helpers

@docs toLists, incrementing

-}

import Array exposing (Array)


{-| A value of type `CellGrid a` is a rectangular array
of values of type a.
-}
type CellGrid a
    = CellGrid Dimensions (Array a)


{-| The size of a cell grid
-}
type alias Dimensions =
    { columns : Int
    , rows : Int
    }


{-| A position in a cell grid
-}
type alias Position =
    { column : Int
    , row : Int
    }


{-| A cell can be in the interior of the grid,
on an edge, on in a corner.
-}
type CellType
    = Corner
    | Edge
    | Interior


{-| The empty cell grid. Useful in conjunction with `Maybe.withDefault`
-}
empty : CellGrid a
empty =
    CellGrid (Dimensions 0 0) (Array.fromList [])


{-| Construct cell grid from a list of values.
Grid construction fails when an amount of values incompatible with the dimensions is given.

    import Array

    fromList (Dimensions 2 2 ) [ 1.0, 2.0, 3.0, 4.0 ]
        --> Just (CellGrid (Dimensions 2 2) (Array.fromList [1,2,3,4]))

    fromList (Dimensions 2 2 ) [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
        --> Nothing

-}
fromList : Dimensions -> List a -> Maybe (CellGrid a)
fromList dimensions data =
    if List.length data == dimensions.rows * dimensions.columns then
        Just <| CellGrid dimensions (Array.fromList data)

    else
        Nothing


{-| Map a function over a cell grid.

    grid : CellGrid Int
    grid = CellGrid.incrementing (Dimensions 2 2)

    toLists grid
    --> [ [0,1]
    --> , [2,3]
    --> ]

    doubled : CellGrid Int
    doubled = CellGrid.map (\v -> v * 2) grid

    toLists doubled
    --> [ [0,2]
    --> , [4,6]
    --> ]

-}
map : (a -> b) -> CellGrid a -> CellGrid b
map f (CellGrid dimensions cells) =
    CellGrid dimensions (Array.map f cells)


{-| Fold a function of type `a -> b -> b` over a `CellGrid a`, associating to the left.
-}
foldl : (a -> b -> b) -> b -> CellGrid a -> b
foldl reducer initialValue (CellGrid _ cells) =
    Array.foldl reducer initialValue cells


{-| Fold a reducer `a -> b -> b` over a `CellGrid a`, associating to the right.

    toList : CellGrid a -> List a
    toList grid =
        CellGrid.foldr (::) [] grid

-}
foldr : (a -> b -> b) -> b -> CellGrid a -> b
foldr reducer initialValue (CellGrid _ cells) =
    Array.foldr reducer initialValue cells


{-| Transform a cell grid while having access to a cell's position

    indexedMap (\i j value -> value + i * j) cellGrid

-}
indexedMap : (Int -> Int -> a -> b) -> CellGrid a -> CellGrid b
indexedMap f (CellGrid dimensions cells) =
    let
        indexedCellTransformer index value =
            let
                position =
                    matrixIndex { rows = dimensions.rows, columns = dimensions.columns } index
            in
            f position.column position.row value
    in
    CellGrid dimensions (Array.indexedMap indexedCellTransformer cells)


{-| Modify a cell while having access to the full cell grid.
The supplied cell grid is always the original input. Useful for simulations that need neighbor information,
like [Conway's game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
-}
transform : (Int -> Int -> CellGrid a -> b) -> CellGrid a -> CellGrid b
transform newCellValue ((CellGrid dimensions elements) as grid) =
    let
        folder _ ( i, j, acc ) =
            let
                ( nextR, nextC ) =
                    if j < dimensions.columns - 1 then
                        ( i, j + 1 )

                    else
                        ( i + 1, 0 )
            in
            ( nextR, nextC, Array.push (newCellValue i j grid) acc )
    in
    Array.foldl folder ( 0, 0, Array.empty ) elements
        |> (\( _, _, value ) -> value)
        |> CellGrid dimensions


{-| Get the list of cell values of the eight neighboring cells.

    grid : CellGrid Int
    grid = CellGrid.incrementing (Dimensions 3 3)

    toLists grid
    --> [ [0,1,2]
    --> , [3,4,5]
    --> , [6,7,8]
    --> ]

    CellGrid.neighbors ( Position 1 1 )  grid
        --> [5,2,1,0,3,6,7,8]

-}
neighbors : Position -> CellGrid a -> List a
neighbors position grid =
    let
        x =
            position.row

        y =
            position.column
    in
    List.filterMap (\index_ -> get index_ grid)
        [ { row = x, column = y + 1 }
        , { row = x - 1, column = y + 1 }
        , { row = x - 1, column = y }
        , { row = x - 1, column = y - 1 }
        , { row = x, column = y - 1 }
        , { row = x + 1, column = y - 1 }
        , { row = x + 1, column = y }
        , { row = x + 1, column = y + 1 }
        ]


{-| Get the list of cell values of the four adjacent cells.

    grid : CellGrid Int
    grid = CellGrid.incrementing (Dimensions 3 3)

    toLists grid
    --> [ [0,1,2]
    --> , [3,4,5]
    --> , [6,7,8]
    --> ]

    CellGrid.adjacent (Position 1 1) grid
        --> [5,1,3,7]

-}
adjacent : Position -> CellGrid a -> List a
adjacent position grid =
    let
        x =
            position.row

        y =
            position.column
    in
    List.filterMap (\index_ -> get index_ grid)
        [ { row = x, column = y + 1 }
        , { row = x - 1, column = y }
        , { row = x, column = y - 1 }
        , { row = x + 1, column = y }
        ]


{-| Convert a `Position` into an index into the flat array.

    arrayIndex (Dimensions 3 2) (Position 0 1) --> 1

    arrayIndex (Dimensions 3 2) (Position 2 1) --> 5

-}
arrayIndex : { a | columns : Int } -> Position -> Int
arrayIndex { columns } position =
    columns * position.row + position.column


{-| Convert an index into the flat array into a `Position`.

    matrixIndex (Dimensions 3 2) 1 --> (Position 0 1)

    matrixIndex (Dimensions 3 2) 5 --> (Position 2 1)

-}
matrixIndex : { a | columns : Int } -> Int -> Position
matrixIndex dimensions n =
    Position (modBy dimensions.columns n) (n // dimensions.columns)


{-| Get a value.

    grid : CellGrid Int
    grid = CellGrid.incrementing (Dimensions 2 2)

    toLists grid
    --> [ [0, 1]
    --> , [2, 3]
    --> ]

    CellGrid.get (Position 1 1) grid
        --> Just 3

-}
get : Position -> CellGrid a -> Maybe a
get position (CellGrid dimensions array) =
    if outOfBounds position dimensions then
        Nothing

    else
        Array.get (arrayIndex dimensions position) array


outOfBounds : Position -> Dimensions -> Bool
outOfBounds position dimensions =
    (position.row < 0)
        || (position.row > dimensions.rows - 1)
        || (position.column < 0)
        || (position.column > dimensions.columns - 1)


{-| Set a value.

    grid : CellGrid Int
    grid = CellGrid.repeat (Dimensions 2 2) 42

    toLists grid
    --> [ [42, 42]
    --> , [42, 42]
    --> ]

    new : CellGrid Int
    new = CellGrid.set (Position 1 1) 84 grid

    toLists new
    --> [ [42, 42]
    --> , [42, 84]
    --> ]

-}
set : Position -> a -> CellGrid a -> CellGrid a
set position value (CellGrid dimensions values) =
    let
        k =
            arrayIndex dimensions position
    in
    CellGrid dimensions (Array.set k value values)


{-| Update a value.

    grid : CellGrid Int
    grid = CellGrid.repeat (Dimensions 2 2) 42

    toLists grid
    --> [ [42, 42]
    --> , [42, 42]
    --> ]

    new : CellGrid Int
    new = CellGrid.update (Position 1 1) (\v -> 2 * v) grid

    toLists new
    --> [ [42, 42]
    --> , [42, 84]
    --> ]

-}
update : Position -> (a -> a) -> CellGrid a -> CellGrid a
update position updater ((CellGrid dimensions array) as cellGrid) =
    let
        index =
            arrayIndex dimensions position
    in
    case Array.get index array of
        Nothing ->
            cellGrid

        Just value ->
            CellGrid dimensions (Array.set index (updater value) array)


{-| return the type of the cell.

    grid : CellGrid (Int, Int)
    grid = CellGrid.initialize (Dimensions 3 3) Tuple.pair

    toLists grid
    --> [[(0,0),(0,1),(0,2)]
    --> ,[(1,0),(1,1),(1,2)]
    --> ,[(2,0),(2,1),(2,2)]
    --> ]

    CellGrid.classifyCell (Position 0 0) grid --> Corner

    CellGrid.classifyCell (Position 0 1) grid --> Edge

    CellGrid.classifyCell (Position 1 1) grid --> Interior

-}
classifyCell : Position -> CellGrid a -> CellType
classifyCell position (CellGrid dimensions _) =
    let
        i =
            position.row

        j =
            position.column

        mri =
            dimensions.rows - 1

        mci =
            dimensions.columns - 1
    in
    if not (i == 0 || j == 0 || i == mri || j == mci) then
        Interior

    else if i == 0 && j == 0 then
        Corner

    else if i == 0 && j == mci then
        Corner

    else if i == mri && j == 0 then
        Corner

    else if i == mri && j == mci then
        Corner

    else
        Edge


{-| Return a list of all matrix indices (i,j) of a grid. Useful for mapping.

    grid : CellGrid Bool
    grid =
        CellGrid.repeat (Dimensions 2 3) True

    CellGrid.matrixIndices grid
    --> [ (Position 0 0)
    --> , (Position 0 1)
    --> , (Position 0 2)
    --> , (Position 1 0)
    --> , (Position 1 1)
    --> , (Position 1 2)
    --> ]

-}
matrixIndices : CellGrid a -> List Position
matrixIndices (CellGrid dimensions _) =
    let
        -- because `List` is used, we must move from the bottom right corner to the top-left
        -- so intuitively everything is reversed.
        maxRow =
            dimensions.rows - 1

        maxColumn =
            dimensions.columns - 1

        go row column accum =
            if row < 0 then
                -- row is outside of range: stop
                accum

            else if column == 0 then
                -- last column of the row: decrement row, reset column
                go (row - 1) maxColumn (Position column row :: accum)

            else
                -- somewhere in the interior: decrement column
                go row (column - 1) (Position column row :: accum)
    in
    go maxRow maxColumn []


{-| Initialize a cell grid. `initialize (Dimensions row column) f` creates a
cell grid of size `(Dimensions row column)` with the element at `(Position i j)` set to the result of `f i j`.

    import Array

    grid1 : CellGrid Int
    grid1 = CellGrid.initialize (Dimensions 2 2) (\i j -> i)

    toLists grid1
    --> [ [0,0]
    --> , [1,1]
    --> ]

    CellGrid.initialize (Dimensions 2 3) (\i j -> toFloat (i + j))
        --> CellGrid (Dimensions 2 3) (Array.fromList [ 0, 1, 2, 1, 2, 3 ])

-}
initialize : Dimensions -> (Int -> Int -> a) -> CellGrid a
initialize dimensions temperatureMap =
    let
        helper index =
            let
                row =
                    index // dimensions.columns

                column =
                    remainderBy dimensions.columns index
            in
            temperatureMap column row
    in
    CellGrid dimensions (Array.initialize (dimensions.rows * dimensions.columns) helper)


{-| Fill a cell grid with a constant value

    import Array

    CellGrid.repeat (Dimensions 2 2) 42
        --> CellGrid (Dimensions 2 2) (Array.fromList [ 42, 42, 42, 42 ])

-}
repeat : Dimensions -> a -> CellGrid a
repeat dimensions value =
    let
        n =
            dimensions.rows * dimensions.columns
    in
    CellGrid dimensions (Array.repeat n value)


{-| Convert a cell grid to a list of rows of values
-}
toLists : CellGrid a -> List (List a)
toLists (CellGrid dimensions array) =
    let
        folder value ( column, rowAccum, rowsAccum ) =
            if column == dimensions.columns then
                ( 1, [ value ], rowAccum :: rowsAccum )

            else
                ( column + 1, value :: rowAccum, rowsAccum )
    in
    let
        ( _, rowAccum, rowsAccum ) =
            Array.foldr folder ( 0, [], [] ) array

        rows =
            rowAccum :: rowsAccum
    in
    rows


{-| An incrementing integer cell grid

    toLists (incrementing (Dimensions 3 3))
    --> [[0,1,2]
    --> ,[3,4,5]
    --> ,[6,7,8]
    --> ]

Defined as

    incrementing : Dimensions -> CellGrid Int
    incrementing dimensions =
        let
            initializer : Int -> Int -> Int
            initializer i j =
                arrayIndex dimensions (Position i j)
        in
        initialize dimensions initializer

-}
incrementing : Dimensions -> CellGrid Int
incrementing dimensions =
    let
        initializer : Int -> Int -> Int
        initializer i j =
            arrayIndex dimensions (Position i j)
    in
    initialize dimensions initializer
