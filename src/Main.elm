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
    ( initialModel, generateNewGameState )



-- MODEL


type alias Model =
    Game


type Game
    = Loading
    | Playing GameState
    | Lost GameState


type alias GameState =
    { speed : SimSpeed
    , grid : Grid
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    Loading


generateNewGameState : Cmd Msg
generateNewGameState =
    Random.generate GameStateGenerated gameStateGenerator


gameStateGenerator : Generator GameState
gameStateGenerator =
    Random.map2 (\grid seed -> GameState Pause grid seed)
        gridGenerator
        Random.independentSeed


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
    = FoxCell Fox
    | RabbitCell Rabbit
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
    Random.weighted ( 1, FoxCell initialFox ) [ ( 2, RabbitCell initialRabbit ), ( 3, Empty ) ]



-- UPDATE


type Msg
    = GameStateGenerated GameState
    | Tick
    | SimSpeedChanged SimSpeed
    | ResetClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, GameStateGenerated newState ) ->
            ( Playing newState, Cmd.none )

        ( Playing state, Tick ) ->
            let
                ( newGrid, newSeed ) =
                    step state.seed state.grid

                newState =
                    { state | grid = newGrid, seed = newSeed }

                { rabbits, foxes } =
                    populations newGrid
            in
            if foxes > 0 && rabbits > 0 then
                ( Playing newState, Cmd.none )

            else
                ( Lost newState, Cmd.none )

        ( Playing state, SimSpeedChanged newSpeed ) ->
            ( Playing { state | speed = newSpeed }, Cmd.none )

        ( _, ResetClicked ) ->
            ( model, generateNewGameState )

        ( _, _ ) ->
            ( model, Cmd.none )


{-| Rules:

  - Foxes eat nearby rabbits
  - Foxes reproduce if they have enough food
  - Foxes move if can't do anything else
  - Rabbits move if next to fox
  - Rabbits reproduce if they have enough food
  - Rabbits eat if can't do anything else

-}
step : Random.Seed -> Grid -> ( Grid, Random.Seed )
step initialSeed initialGrid =
    cellGridFoldlWithPosition
        (\position cell ( grid, seed ) ->
            Random.step (stepAnimal position cell grid) seed
        )
        ( initialGrid, initialSeed )
        initialGrid


stepAnimal : Position -> Cell -> Grid -> Generator Grid
stepAnimal position cell grid =
    case cell of
        FoxCell fox ->
            stepFox position fox grid

        RabbitCell rabbit ->
            stepRabbit position rabbit grid

        Empty ->
            Random.constant grid



-- FOX


type alias Fox =
    { food : Int }


initialFox : Fox
initialFox =
    { food = 5 }


babyFox : Fox
babyFox =
    { food = foxBirthCost }


foxCostOfLiving : Int
foxCostOfLiving =
    1


foxBirthCost : Int
foxBirthCost =
    3


stepFox : Position -> Fox -> Grid -> Generator Grid
stepFox position fox grid =
    if fox.food > 0 then
        foxActions position { fox | food = fox.food - foxCostOfLiving } grid

    else
        -- Fox starved to death
        Random.constant <| setEmpty position grid


foxActions : Position -> Fox -> Grid -> Generator Grid
foxActions foxPos fox grid =
    case positions (nearbyRabbits foxPos grid) of
        [] ->
            case foxValidBirthPositions foxPos fox grid of
                Just birthPositions ->
                    birthFoxAtRandomPosition birthPositions foxPos fox grid

                Nothing ->
                    moveToEmptyFrom foxPos (FoxCell fox) grid

        firstRabbitPos :: otherRabbitsPos ->
            eatRandomRabbit ( firstRabbitPos, otherRabbitsPos ) foxPos fox grid


birthFox : { babyPos : Position, parentPos : Position } -> Fox -> Grid -> Grid
birthFox { babyPos, parentPos } parent grid =
    grid
        |> CellGrid.set parentPos (FoxCell { parent | food = parent.food - foxBirthCost })
        |> CellGrid.set babyPos (FoxCell { food = foxBirthCost })


eatRabbit : { rabbitPos : Position, foxPos : Position } -> Fox -> Grid -> Grid
eatRabbit { rabbitPos, foxPos } fox grid =
    grid
        |> setEmpty rabbitPos
        |> CellGrid.set foxPos (FoxCell { fox | food = fox.food + rabbitNutrition })


foxValidBirthPositions : Position -> Fox -> Grid -> Maybe ( Position, List Position )
foxValidBirthPositions position fox grid =
    if fox.food > foxBirthCost + foxCostOfLiving then
        case positions (nearbyEmpties position grid) of
            [] ->
                Nothing

            first :: rest ->
                Just ( first, rest )

    else
        Nothing


positions : List ( Position, a ) -> List Position
positions =
    List.map Tuple.first



-- RABBIT


type alias Rabbit =
    { food : Int }


initialRabbit : Rabbit
initialRabbit =
    { food = 5 }


rabbitCostOfLiving : Int
rabbitCostOfLiving =
    1


rabbitNutrition : Int
rabbitNutrition =
    5


grassNutrition : Int
grassNutrition =
    3


rabbitBirthCost : Int
rabbitBirthCost =
    2


stepRabbit : Position -> Rabbit -> Grid -> Generator Grid
stepRabbit position rabbit grid =
    if rabbit.food > 0 then
        rabbitActions position { rabbit | food = rabbit.food - rabbitCostOfLiving } grid

    else
        -- Rabbit starved to death
        Random.constant <| setEmpty position grid


rabbitActions : Position -> Rabbit -> Grid -> Generator Grid
rabbitActions rabbitPos rabbit grid =
    if isSafe rabbitPos grid then
        case rabbitValidBirthPosition rabbitPos rabbit grid of
            Just birthPositions ->
                birthRabbitAtRandomPosition birthPositions rabbitPos rabbit grid

            Nothing ->
                Random.constant <| eatGrass rabbitPos rabbit grid

    else
        moveToSafetyFrom rabbitPos rabbit grid


eatGrass : Position -> Rabbit -> Grid -> Grid
eatGrass position rabbit grid =
    let
        postMealRabbit =
            { rabbit | food = rabbit.food + grassNutrition }
    in
    CellGrid.set position (RabbitCell postMealRabbit) grid


birthRabbit : { babyPos : Position, parentPos : Position } -> Rabbit -> Grid -> Grid
birthRabbit { babyPos, parentPos } parent grid =
    grid
        |> CellGrid.set parentPos (RabbitCell { parent | food = parent.food - rabbitBirthCost })
        |> CellGrid.set babyPos (RabbitCell { food = rabbitBirthCost })


moveToSafetyFrom : Position -> Rabbit -> Grid -> Generator Grid
moveToSafetyFrom position rabbit grid =
    case positions (nearbySafeEmpties position grid) of
        [] ->
            Random.constant grid

        first :: rest ->
            moveToRandomPosition first rest position (RabbitCell rabbit) grid


rabbitValidBirthPosition : Position -> Rabbit -> Grid -> Maybe ( Position, List Position )
rabbitValidBirthPosition position rabbit grid =
    if rabbit.food > rabbitBirthCost + rabbitCostOfLiving then
        case positions (nearbySafeEmpties position grid) of
            [] ->
                Nothing

            first :: rest ->
                Just ( first, rest )

    else
        Nothing



-- GENERIC GAME GRID ACTIONS


moveToEmptyFrom : Position -> Cell -> Grid -> Generator Grid
moveToEmptyFrom position cell grid =
    case positions (nearbyEmpties position grid) of
        [] ->
            Random.constant grid

        first :: rest ->
            moveToRandomPosition first rest position cell grid


moveToRandomPosition : Position -> List Position -> Position -> Cell -> Grid -> Generator Grid
moveToRandomPosition first rest position cell grid =
    Random.uniform first rest
        |> Random.map
            (\newPos ->
                move { from = position, to = newPos } cell grid
            )


birthFoxAtRandomPosition : ( Position, List Position ) -> Position -> Fox -> Grid -> Generator Grid
birthFoxAtRandomPosition ( first, rest ) foxPos fox grid =
    Random.uniform first rest
        |> Random.map
            (\babyPos ->
                birthFox { babyPos = babyPos, parentPos = foxPos } fox grid
            )


birthRabbitAtRandomPosition : ( Position, List Position ) -> Position -> Rabbit -> Grid -> Generator Grid
birthRabbitAtRandomPosition ( first, rest ) rabbitPos rabbit grid =
    Random.uniform first rest
        |> Random.map
            (\babyPos ->
                birthRabbit { babyPos = babyPos, parentPos = rabbitPos } rabbit grid
            )


eatRandomRabbit : ( Position, List Position ) -> Position -> Fox -> Grid -> Generator Grid
eatRandomRabbit ( first, rest ) foxPos fox grid =
    Random.uniform first rest
        |> Random.map
            (\rabbitPos ->
                eatRabbit { rabbitPos = rabbitPos, foxPos = foxPos } fox grid
            )


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


nearbyRabbits : Position -> Grid -> List ( Position, Cell )
nearbyRabbits =
    neighborsWhere (isRabbit << Tuple.second)


nearbyFoxes : Position -> Grid -> List ( Position, Cell )
nearbyFoxes =
    neighborsWhere (isFox << Tuple.second)


nearbyEmpties : Position -> Grid -> List ( Position, Cell )
nearbyEmpties =
    neighborsWhere (isEmpty << Tuple.second)


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


populations : Grid -> { rabbits : Int, foxes : Int }
populations grid =
    CellGrid.foldl
        (\cell count ->
            if isRabbit cell then
                { count | rabbits = count.rabbits + 1 }

            else if isFox cell then
                { count | foxes = count.foxes + 1 }

            else
                count
        )
        { rabbits = 0, foxes = 0 }
        grid


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing state ->
            case state.speed of
                Play ->
                    Time.every 1000 (\_ -> Tick)

                DoubleSpeed ->
                    Time.every 500 (\_ -> Tick)

                Pause ->
                    Sub.none

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            loadingView

        Playing state ->
            playingView state

        Lost state ->
            lostView state


loadingView : Html Msg
loadingView =
    Html.main_ []
        [ header
        , Html.div [] [ Html.text "Loading" ]
        ]


playingView : GameState -> Html Msg
playingView state =
    Html.main_ []
        [ header
        , gameBoard state.grid
        , controls state.speed
        ]


lostView : GameState -> Html Msg
lostView state =
    Html.main_ []
        [ header
        , gameBoard state.grid
        , lostControls
        ]


header : Html a
header =
    Html.h1 [] [ Html.text "Ecosystem" ]


lostControls : Html Msg
lostControls =
    Html.section [ Html.Attributes.class "controls" ]
        [ Html.h2 [] [ Html.text "Ecosystem collapsed!" ]
        , resetControl
        ]


controls : SimSpeed -> Html Msg
controls currentSpeed =
    Html.section [ Html.Attributes.class "controls" ]
        [ speedControls currentSpeed
        , resetControl
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


resetControl : Html Msg
resetControl =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Reset" ]
        , Html.button [ Html.Events.onClick ResetClicked ] [ Html.text "Reset" ]
        ]


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
    Html.td [] [ cellSymbol cell ]


cellSymbol : Cell -> Html a
cellSymbol cell =
    case cell of
        FoxCell fox ->
            hungerAwareValue fox.food (Html.text "\u{1F98A}")

        RabbitCell rabbit ->
            hungerAwareValue rabbit.food (Html.text "🐰")

        Empty ->
            Html.text ""


hungerAwareValue : Int -> Html a -> Html a
hungerAwareValue food content =
    Html.span [ Html.Attributes.class <| hungerClass food ] [ content ]


hungerClass : Int -> String
hungerClass food =
    if food > 3 then
        "full"

    else if food > 1 then
        "hungry"

    else
        "starving"



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
