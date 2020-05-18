module Main exposing (main)

import Browser
import CellGrid exposing (CellGrid, Position)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Random exposing (Generator)
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Random.generate GridGenerated gridGenerator )



-- MODEL


type alias Model =
    { speed : SimSpeed
    , grid : Grid
    }


initialModel : Model
initialModel =
    { speed = Pause
    , grid = emptyGrid
    }


type SimSpeed
    = Play
    | Pause
    | DoubleSpeed


type alias Grid =
    CellGrid Cell


emptyGrid : Grid
emptyGrid =
    CellGrid.repeat gridDimensions Empty


gridDimensions : CellGrid.Dimensions
gridDimensions =
    { rows = 10, columns = 10 }


type Cell
    = Fox
    | Rabbit
    | Empty


gridGenerator : Generator Grid
gridGenerator =
    Random.list (gridDimensions.columns * gridDimensions.rows) cellGenerator
        |> Random.map gridFromList


gridFromList : List Cell -> Grid
gridFromList =
    Maybe.withDefault emptyGrid << CellGrid.fromList gridDimensions


cellGenerator : Generator Cell
cellGenerator =
    Random.weighted ( 1, Fox ) [ ( 2, Rabbit ), ( 3, Empty ) ]



-- UPDATE


type Msg
    = GridGenerated Grid
    | Tick
    | SimSpeedChanged SimSpeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GridGenerated newGrid ->
            ( { model | grid = newGrid }, Cmd.none )

        Tick ->
            ( { model | grid = step model.grid }, Cmd.none )

        SimSpeedChanged newSpeed ->
            ( { model | speed = newSpeed }, Cmd.none )


{-| Rules:

  - Foxes eat nearby rabbits
  - Rabbits move if next to fox

-}
step : Grid -> Grid
step grid =
    cellGridFoldlWithPosition stepAnimal grid grid


stepAnimal : Position -> Cell -> Grid -> Grid
stepAnimal position cell grid =
    case cell of
        Fox ->
            stepFox position grid

        Rabbit ->
            stepRabbit position grid

        Empty ->
            grid


stepFox : Position -> Grid -> Grid
stepFox position grid =
    case nearbyRabbits position grid of
        [] ->
            grid

        ( rabbitPos, _ ) :: rest ->
            CellGrid.set rabbitPos Empty grid


stepRabbit : Position -> Grid -> Grid
stepRabbit position grid =
    if isSafe position grid then
        grid

    else
        moveToSafetyFrom position grid


moveToSafetyFrom : Position -> Grid -> Grid
moveToSafetyFrom position grid =
    case nearbySafeEmpties position grid of
        [] ->
            grid

        ( safePos, _ ) :: rest ->
            move { from = position, to = safePos } Rabbit grid


{-| Moving an entity requires two operations on a grid:

1.  Setting the original position to "Empty"
2.  Inserting the entity at the new position

Wrapping this up in a move function makes it harder to accidentally delete or
duplicate the entity by forgetting either of the required steps

-}
move : { from : Position, to : Position } -> Cell -> Grid -> Grid
move { from, to } cell grid =
    grid
        |> CellGrid.set from Empty
        |> CellGrid.set to cell


nearbyRabbits : Position -> Grid -> List ( Position, Cell )
nearbyRabbits =
    neighborsWhere (isRabbit << Tuple.second)


nearbyFoxes : Position -> Grid -> List ( Position, Cell )
nearbyFoxes =
    neighborsWhere (isFox << Tuple.second)


nearbySafeEmpties : Position -> Grid -> List ( Position, Cell )
nearbySafeEmpties position grid =
    neighborsWhere (isSafeEmpty grid) position grid


neighborsWhere : (( Position, Cell ) -> Bool) -> Position -> Grid -> List ( Position, Cell )
neighborsWhere filterFunc position grid =
    grid
        |> neighborsWithPositions position
        |> List.filter filterFunc


isSafeEmpty : Grid -> ( Position, Cell ) -> Bool
isSafeEmpty grid ( position, cell ) =
    isEmpty cell && isSafe position grid


isSafe : Position -> Grid -> Bool
isSafe position grid =
    grid
        |> nearbyFoxes position
        |> List.isEmpty


isRabbit : Cell -> Bool
isRabbit cell =
    case cell of
        Rabbit ->
            True

        _ ->
            False


isFox : Cell -> Bool
isFox cell =
    case cell of
        Fox ->
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.speed of
        Play ->
            Time.every 1000 (\_ -> Tick)

        DoubleSpeed ->
            Time.every 500 (\_ -> Tick)

        Pause ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ header
        , gameBoard model.grid
        , controls model.speed
        ]


header : Html a
header =
    Html.h1 [] [ Html.text "Ecosystem" ]


controls : SimSpeed -> Html Msg
controls currentSpeed =
    Html.section [ Html.Attributes.class "controls" ]
        [ speedControls currentSpeed
        ]


speedControls : SimSpeed -> Html Msg
speedControls currentSpeed =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Speed" ]
        , multiToggle
            { selectedItem = currentSpeed
            , tagger = SimSpeedChanged
            , toLabel = speedLabel
            , groupName = "speed"
            }
            [ Pause, Play, DoubleSpeed ]
        ]


speedLabel : SimSpeed -> String
speedLabel speed =
    case speed of
        Play ->
            "Play"

        DoubleSpeed ->
            "Fast"

        Pause ->
            "Pause"


gameBoard : Grid -> Html a
gameBoard grid =
    Html.section [ Html.Attributes.class "game-board" ]
        [ Html.table [] <| List.map viewRow <| CellGrid.toLists grid
        ]


viewRow : List Cell -> Html a
viewRow cells =
    Html.tr [] <| List.map viewCell cells


viewCell : Cell -> Html a
viewCell cell =
    Html.td [] [ Html.text (cellSymbol cell) ]


cellSymbol : Cell -> String
cellSymbol cell =
    case cell of
        Fox ->
            "\u{1F98A}"

        Rabbit ->
            "🐰"

        Empty ->
            ""



-- FORM HELPERS


type alias RadioConfig a msg =
    { selectedItem : a
    , tagger : a -> msg
    , toLabel : a -> String
    , groupName : String
    }


multiToggle : RadioConfig a msg -> List a -> Html msg
multiToggle config items =
    Html.div
        [ Html.Attributes.class "multi-toggle" ]
        (radioGroup config items)


radioGroup : RadioConfig a msg -> List a -> List (Html msg)
radioGroup config items =
    List.concat <| List.map (radio config) items


radio : RadioConfig a msg -> a -> List (Html msg)
radio { selectedItem, tagger, toLabel, groupName } item =
    let
        id =
            toLabel item
                |> String.toLower
                |> String.replace " " "-"
    in
    [ Html.input
        [ Html.Attributes.type_ "radio"
        , Html.Attributes.name groupName
        , Html.Events.onClick (tagger item)
        , Html.Attributes.checked (selectedItem == item)
        , Html.Attributes.id id
        ]
        []
    , Html.label [ Html.Attributes.for id ] [ Html.text (toLabel item) ]
    ]



-- CELL GRID HELPERS


cellGridFoldlWithPosition : (Position -> a -> b -> b) -> b -> CellGrid a -> b
cellGridFoldlWithPosition stepFunc initial grid =
    grid
        |> cellGridWithPositions
        |> CellGrid.foldl (\( pos, item ) acc -> stepFunc pos item acc) initial


neighborsWithPositions : Position -> CellGrid a -> List ( Position, a )
neighborsWithPositions position grid =
    grid
        |> cellGridWithPositions
        |> CellGrid.neighbors position


cellGridWithPositions : CellGrid a -> CellGrid ( Position, a )
cellGridWithPositions grid =
    CellGrid.indexedMap (\x y item -> ( Position x y, item )) grid
