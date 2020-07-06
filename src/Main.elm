module Main exposing (main)

import Browser
import CellGrid exposing (CellGrid, Position)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
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
    , foxConfig : FoxConfig
    , rabbitConfig : RabbitConfig
    }


initialModel : Model
initialModel =
    Loading


generateNewGameState : Cmd Msg
generateNewGameState =
    Random.generate GameStateGenerated gameStateGenerator


gameStateGenerator : Generator GameState
gameStateGenerator =
    Random.map2 (\grid seed -> GameState Pause grid seed initialFoxConfig initialRabbitConfig)
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
    | FoxConfigChanged FoxField Energy
    | RabbitConfigChanged RabbitField Energy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, GameStateGenerated newState ) ->
            ( Playing newState, Cmd.none )

        ( Playing state, Tick ) ->
            let
                ( newGrid, newSeed ) =
                    step state

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

        ( Playing ({ foxConfig } as state), FoxConfigChanged field newCost ) ->
            let
                newFoxConfig =
                    setFoxConfigField field newCost state.foxConfig
            in
            ( Playing { state | foxConfig = newFoxConfig }, Cmd.none )

        ( Playing ({ rabbitConfig } as state), RabbitConfigChanged field newCost ) ->
            let
                newRabbitConfig =
                    setRabbitConfigField field newCost state.rabbitConfig
            in
            ( Playing { state | rabbitConfig = newRabbitConfig }, Cmd.none )

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
step : GameState -> ( Grid, Random.Seed )
step state =
    cellGridFoldlWithPosition
        (\position cell ( newGrid, newSeed ) ->
            Random.step (stepAnimal state position cell newGrid) newSeed
        )
        ( state.grid, state.seed )
        state.grid


stepAnimal : GameState -> Position -> Cell -> Grid -> Generator Grid
stepAnimal { foxConfig, rabbitConfig } position cell grid =
    case cell of
        FoxCell fox ->
            stepFox foxConfig position fox grid

        RabbitCell rabbit ->
            stepRabbit rabbitConfig position rabbit grid

        Empty ->
            Random.constant grid



-- ENERGY


type Energy
    = Energy Int


energyToInt : Energy -> Int
energyToInt (Energy e) =
    e


hasEnergy : { a | energy : Energy } -> Bool
hasEnergy animal =
    let
        (Energy energy) =
            animal.energy
    in
    energy > 0


consumeEnergy : Energy -> { a | energy : Energy } -> { a | energy : Energy }
consumeEnergy (Energy cost) animal =
    let
        (Energy animalEnergy) =
            animal.energy
    in
    { animal | energy = Energy (animalEnergy - cost) }


gainEnergy : Energy -> { a | energy : Energy } -> { a | energy : Energy }
gainEnergy (Energy newEnergy) animal =
    let
        (Energy animalEnergy) =
            animal.energy
    in
    { animal | energy = Energy (animalEnergy + newEnergy) }


canSupportCosts : List Energy -> { a | energy : Energy } -> Bool
canSupportCosts costs animal =
    let
        (Energy totalCost) =
            sumEnergies costs

        (Energy animalEnergy) =
            animal.energy
    in
    animalEnergy > totalCost


addEnergy : Energy -> Energy -> Energy
addEnergy (Energy e1) (Energy e2) =
    Energy (e1 + e2)


sumEnergies : List Energy -> Energy
sumEnergies energies =
    List.foldl addEnergy (Energy 0) energies



-- FOX


type alias FoxConfig =
    { costOfLiving : Energy
    , birthCost : Energy
    , rabbitNutrition : Energy
    }


initialFoxConfig : FoxConfig
initialFoxConfig =
    { costOfLiving = Energy 1
    , birthCost = Energy 3
    , rabbitNutrition = Energy 5
    }


type FoxField
    = FoxCostOfLiving
    | FoxBirthCost
    | RabbitNutrition


setFoxConfigField : FoxField -> Energy -> FoxConfig -> FoxConfig
setFoxConfigField field energy config =
    case field of
        FoxCostOfLiving ->
            { config | costOfLiving = energy }

        FoxBirthCost ->
            { config | birthCost = energy }

        RabbitNutrition ->
            { config | rabbitNutrition = energy }


type alias Fox =
    { energy : Energy }


initialFox : Fox
initialFox =
    { energy = Energy 5 }


stepFox : FoxConfig -> Position -> Fox -> Grid -> Generator Grid
stepFox ({ costOfLiving } as foxConfig) position fox grid =
    if hasEnergy fox then
        foxActions foxConfig position (consumeEnergy costOfLiving fox) grid

    else
        -- Fox starved to death
        Random.constant <| setEmpty position grid


foxActions : FoxConfig -> Position -> Fox -> Grid -> Generator Grid
foxActions foxConfig foxPos fox grid =
    case positions (nearbyRabbits foxPos grid) of
        [] ->
            case foxValidBirthPositions foxConfig foxPos fox grid of
                Just birthPositions ->
                    birthFoxAtRandomPosition birthPositions foxConfig foxPos fox grid

                Nothing ->
                    moveToEmptyFrom foxPos (FoxCell fox) grid

        firstRabbitPos :: otherRabbitsPos ->
            eatRandomRabbit ( firstRabbitPos, otherRabbitsPos ) foxConfig foxPos fox grid


birthFox : FoxConfig -> { babyPos : Position, parentPos : Position } -> Fox -> Grid -> Grid
birthFox { birthCost } { babyPos, parentPos } parent grid =
    grid
        |> CellGrid.set parentPos (FoxCell (consumeEnergy birthCost parent))
        |> CellGrid.set babyPos (FoxCell { energy = birthCost })


eatRabbit : FoxConfig -> { rabbitPos : Position, foxPos : Position } -> Fox -> Grid -> Grid
eatRabbit { rabbitNutrition } { rabbitPos, foxPos } fox grid =
    grid
        |> setEmpty rabbitPos
        |> CellGrid.set foxPos (FoxCell (gainEnergy rabbitNutrition fox))


foxValidBirthPositions : FoxConfig -> Position -> Fox -> Grid -> Maybe ( Position, List Position )
foxValidBirthPositions { birthCost, costOfLiving } position fox grid =
    if canSupportCosts [ birthCost, costOfLiving ] fox then
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
    { energy : Energy }


initialRabbit : Rabbit
initialRabbit =
    { energy = Energy 5 }


type alias RabbitConfig =
    { costOfLiving : Energy
    , grassNutrition : Energy
    , birthCost : Energy
    }


type RabbitField
    = RabbitCostOfLiving
    | GrassNutrition
    | RabbitBirthCost


setRabbitConfigField : RabbitField -> Energy -> RabbitConfig -> RabbitConfig
setRabbitConfigField field energy config =
    case field of
        RabbitCostOfLiving ->
            { config | costOfLiving = energy }

        GrassNutrition ->
            { config | grassNutrition = energy }

        RabbitBirthCost ->
            { config | birthCost = energy }


initialRabbitConfig : RabbitConfig
initialRabbitConfig =
    { costOfLiving = Energy 1
    , grassNutrition = Energy 3
    , birthCost = Energy 2
    }


stepRabbit : RabbitConfig -> Position -> Rabbit -> Grid -> Generator Grid
stepRabbit ({ costOfLiving } as rabbitConfig) position rabbit grid =
    if hasEnergy rabbit then
        rabbitActions rabbitConfig position (consumeEnergy costOfLiving rabbit) grid

    else
        -- Rabbit starved to death
        Random.constant <| setEmpty position grid


rabbitActions : RabbitConfig -> Position -> Rabbit -> Grid -> Generator Grid
rabbitActions rabbitConfig rabbitPos rabbit grid =
    if isSafe rabbitPos grid then
        case rabbitValidBirthPosition rabbitConfig rabbitPos rabbit grid of
            Just birthPositions ->
                birthRabbitAtRandomPosition birthPositions rabbitConfig rabbitPos rabbit grid

            Nothing ->
                Random.constant <| eatGrass rabbitConfig rabbitPos rabbit grid

    else
        moveToSafetyFrom rabbitPos rabbit grid


eatGrass : RabbitConfig -> Position -> Rabbit -> Grid -> Grid
eatGrass { grassNutrition } position rabbit grid =
    let
        postMealRabbit =
            gainEnergy grassNutrition rabbit
    in
    CellGrid.set position (RabbitCell postMealRabbit) grid


birthRabbit : RabbitConfig -> { babyPos : Position, parentPos : Position } -> Rabbit -> Grid -> Grid
birthRabbit { birthCost } { babyPos, parentPos } parent grid =
    grid
        |> CellGrid.set parentPos (RabbitCell (consumeEnergy birthCost parent))
        |> CellGrid.set babyPos (RabbitCell { energy = birthCost })


moveToSafetyFrom : Position -> Rabbit -> Grid -> Generator Grid
moveToSafetyFrom position rabbit grid =
    case positions (nearbySafeEmpties position grid) of
        [] ->
            Random.constant grid

        first :: rest ->
            moveToRandomPosition first rest position (RabbitCell rabbit) grid


rabbitValidBirthPosition : RabbitConfig -> Position -> Rabbit -> Grid -> Maybe ( Position, List Position )
rabbitValidBirthPosition { birthCost, costOfLiving } position rabbit grid =
    if canSupportCosts [ birthCost, costOfLiving ] rabbit then
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


birthFoxAtRandomPosition : ( Position, List Position ) -> FoxConfig -> Position -> Fox -> Grid -> Generator Grid
birthFoxAtRandomPosition ( first, rest ) foxConfig foxPos fox grid =
    Random.uniform first rest
        |> Random.map
            (\babyPos ->
                birthFox foxConfig { babyPos = babyPos, parentPos = foxPos } fox grid
            )


birthRabbitAtRandomPosition : ( Position, List Position ) -> RabbitConfig -> Position -> Rabbit -> Grid -> Generator Grid
birthRabbitAtRandomPosition ( first, rest ) rabbitConfig rabbitPos rabbit grid =
    Random.uniform first rest
        |> Random.map
            (\babyPos ->
                birthRabbit rabbitConfig { babyPos = babyPos, parentPos = rabbitPos } rabbit grid
            )


eatRandomRabbit : ( Position, List Position ) -> FoxConfig -> Position -> Fox -> Grid -> Generator Grid
eatRandomRabbit ( first, rest ) foxConfig foxPos fox grid =
    Random.uniform first rest
        |> Random.map
            (\rabbitPos ->
                eatRabbit foxConfig { rabbitPos = rabbitPos, foxPos = foxPos } fox grid
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
        , controls state
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


controls : GameState -> Html Msg
controls state =
    Html.section [ Html.Attributes.class "controls" ]
        [ speedControls state.speed
        , resetControl
        , foxConfigControls state.foxConfig
        , rabbitConfigControls state.rabbitConfig
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


foxConfigControls : FoxConfig -> Html Msg
foxConfigControls config =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Fox Config" ]
        , range
            { label = "Metabolism"
            , value = energyToInt config.costOfLiving
            , tagger = FoxConfigChanged FoxCostOfLiving << Energy
            }
        , range
            { label = "Birth Cost"
            , value = energyToInt config.birthCost
            , tagger = FoxConfigChanged FoxBirthCost << Energy
            }
        , range
            { label = "Rabbit Nutrition"
            , value = energyToInt config.rabbitNutrition
            , tagger = FoxConfigChanged RabbitNutrition << Energy
            }
        ]


rabbitConfigControls : RabbitConfig -> Html Msg
rabbitConfigControls config =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Rabbit Config" ]
        , range
            { label = "Metabolism"
            , value = energyToInt config.costOfLiving
            , tagger = RabbitConfigChanged RabbitCostOfLiving << Energy
            }
        , range
            { label = "Birth Cost"
            , value = energyToInt config.birthCost
            , tagger = RabbitConfigChanged RabbitBirthCost << Energy
            }
        , range
            { label = "Grass Nutrition"
            , value = energyToInt config.grassNutrition
            , tagger = RabbitConfigChanged GrassNutrition << Energy
            }
        ]


range : { label : String, value : Int, tagger : Int -> msg } -> Html msg
range config =
    Html.div []
        [ Html.label []
            [ Html.span [] [ Html.text <| String.fromInt config.value ]
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "5"
                , Html.Attributes.value <| String.fromInt config.value
                , onIntInput config.tagger
                ]
                []
            , Html.text config.label
            ]
        ]


onIntInput : (Int -> msg) -> Html.Attribute msg
onIntInput tagger =
    Html.Events.on "input" (Decode.map tagger targetInt)


targetInt : Decoder Int
targetInt =
    Html.Events.targetValue
        |> Decode.andThen (fromMaybe << String.toInt)


fromMaybe : Maybe a -> Decoder a
fromMaybe maybe =
    case maybe of
        Just value ->
            Decode.succeed value

        Nothing ->
            Decode.fail "was Nothing"


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
            hungerAwareValue fox.energy (Html.text "\u{1F98A}")

        RabbitCell rabbit ->
            hungerAwareValue rabbit.energy (Html.text "🐰")

        Empty ->
            Html.text ""


hungerAwareValue : Energy -> Html a -> Html a
hungerAwareValue energy content =
    Html.span [ Html.Attributes.class <| hungerClass energy ] [ content ]


hungerClass : Energy -> String
hungerClass (Energy energy) =
    if energy > 3 then
        "full"

    else if energy > 1 then
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
