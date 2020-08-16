module Main exposing (main)

import Browser
import Cell exposing (Cell(..), Fox, Rabbit)
import CellGrid exposing (CellGrid, Position)
import CellGrid.Extra as CellGrid
import Energy exposing (Energy)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Random exposing (Generator)
import Svg exposing (Svg)
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
    , daysElapsed : Int
    , grid : Grid
    , seed : Random.Seed
    , foxConfig : FoxConfig
    , rabbitConfig : RabbitConfig
    , energyHistory : NonEmpty Grid.EnergyStat
    }


initialModel : Model
initialModel =
    Loading


generateNewGameState : Cmd Msg
generateNewGameState =
    Random.generate GameStateGenerated gameStateGenerator


gameStateGenerator : Generator GameState
gameStateGenerator =
    Random.map2
        (\grid seed ->
            { speed = Pause
            , daysElapsed = 0
            , grid = grid
            , seed = seed
            , foxConfig = initialFoxConfig
            , rabbitConfig = initialRabbitConfig
            , energyHistory = NonEmpty.singleton (Grid.energyStats grid)
            }
        )
        Grid.generator
        Random.independentSeed


type SimSpeed
    = Play
    | Pause
    | DoubleSpeed



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
                    { state
                        | grid = newGrid
                        , seed = newSeed
                        , daysElapsed = state.daysElapsed + 1
                        , energyHistory =
                            NonEmpty.cons
                                (Grid.energyStats newGrid)
                                state.energyHistory
                    }

                { rabbits, foxes } =
                    Grid.populations newGrid
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
    CellGrid.foldlWithPosition
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


hasEnergy : { a | energy : Energy } -> Bool
hasEnergy animal =
    Energy.isPositive animal.energy


consumeEnergy : Energy -> { a | energy : Energy } -> { a | energy : Energy }
consumeEnergy cost animal =
    { animal | energy = animal.energy |> Energy.subtract cost }


gainEnergy : Energy -> { a | energy : Energy } -> { a | energy : Energy }
gainEnergy newEnergy animal =
    { animal | energy = Energy.add animal.energy newEnergy }


canSupportCosts : List Energy -> { a | energy : Energy } -> Bool
canSupportCosts costs animal =
    animal.energy
        |> Energy.isGreaterThan (Energy.sum costs)



-- FOX


type alias FoxConfig =
    { costOfLiving : Energy
    , birthCost : Energy
    , rabbitNutrition : Energy
    }


initialFoxConfig : FoxConfig
initialFoxConfig =
    { costOfLiving = Energy.fromInt 1
    , birthCost = Energy.fromInt 3
    , rabbitNutrition = Energy.fromInt 5
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


stepFox : FoxConfig -> Position -> Fox -> Grid -> Generator Grid
stepFox ({ costOfLiving } as foxConfig) position fox grid =
    if hasEnergy fox then
        foxActions foxConfig position (consumeEnergy costOfLiving fox) grid

    else
        -- Fox starved to death
        Random.constant <| Grid.setEmpty position grid


foxActions : FoxConfig -> Position -> Fox -> Grid -> Generator Grid
foxActions foxConfig foxPos fox grid =
    case NonEmpty.fromList <| positions (Grid.nearbyRabbits foxPos grid) of
        Nothing ->
            case NonEmpty.fromList <| foxValidBirthPositions foxConfig foxPos fox grid of
                Just birthPositions ->
                    birthFoxAtRandomPosition birthPositions foxConfig foxPos fox grid

                Nothing ->
                    moveToEmptyFrom foxPos (FoxCell fox) grid

        Just rabbitPositions ->
            eatRandomRabbit rabbitPositions foxConfig foxPos fox grid


birthFox : FoxConfig -> { babyPos : Position, parentPos : Position } -> Fox -> Grid -> Grid
birthFox { birthCost } { babyPos, parentPos } parent grid =
    grid
        |> CellGrid.set parentPos (FoxCell (consumeEnergy birthCost parent))
        |> CellGrid.set babyPos (FoxCell { energy = birthCost })


eatRabbit : FoxConfig -> { rabbitPos : Position, foxPos : Position } -> Fox -> Grid -> Grid
eatRabbit { rabbitNutrition } { rabbitPos, foxPos } fox grid =
    grid
        |> Grid.setEmpty rabbitPos
        |> CellGrid.set foxPos (FoxCell (gainEnergy rabbitNutrition fox))


foxValidBirthPositions : FoxConfig -> Position -> Fox -> Grid -> List Position
foxValidBirthPositions { birthCost, costOfLiving } position fox grid =
    if canSupportCosts [ birthCost, costOfLiving ] fox then
        positions (Grid.nearbyEmpties position grid)

    else
        []


positions : List ( Position, a ) -> List Position
positions =
    List.map Tuple.first



-- RABBIT


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
    { costOfLiving = Energy.fromInt 1
    , grassNutrition = Energy.fromInt 3
    , birthCost = Energy.fromInt 2
    }


stepRabbit : RabbitConfig -> Position -> Rabbit -> Grid -> Generator Grid
stepRabbit ({ costOfLiving } as rabbitConfig) position rabbit grid =
    if hasEnergy rabbit then
        rabbitActions rabbitConfig position (consumeEnergy costOfLiving rabbit) grid

    else
        -- Rabbit starved to death
        Random.constant <| Grid.setEmpty position grid


rabbitActions : RabbitConfig -> Position -> Rabbit -> Grid -> Generator Grid
rabbitActions rabbitConfig rabbitPos rabbit grid =
    if Grid.isSafe rabbitPos grid then
        case NonEmpty.fromList <| rabbitValidBirthPosition rabbitConfig rabbitPos rabbit grid of
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
    case NonEmpty.fromList <| positions (Grid.nearbySafeEmpties position grid) of
        Nothing ->
            Random.constant grid

        Just movePositions ->
            moveToRandomPosition movePositions position (RabbitCell rabbit) grid


rabbitValidBirthPosition : RabbitConfig -> Position -> Rabbit -> Grid -> List Position
rabbitValidBirthPosition { birthCost, costOfLiving } position rabbit grid =
    if canSupportCosts [ birthCost, costOfLiving ] rabbit then
        positions (Grid.nearbySafeEmpties position grid)

    else
        []



-- GENERIC GAME GRID ACTIONS


moveToEmptyFrom : Position -> Cell -> Grid -> Generator Grid
moveToEmptyFrom position cell grid =
    case NonEmpty.fromList <| positions (Grid.nearbyEmpties position grid) of
        Nothing ->
            Random.constant grid

        Just movePositions ->
            moveToRandomPosition movePositions position cell grid


moveToRandomPosition : NonEmpty Position -> Position -> Cell -> Grid -> Generator Grid
moveToRandomPosition movePositions position cell grid =
    uniform movePositions
        |> Random.map
            (\newPos ->
                Grid.move { from = position, to = newPos } cell grid
            )


birthFoxAtRandomPosition : NonEmpty Position -> FoxConfig -> Position -> Fox -> Grid -> Generator Grid
birthFoxAtRandomPosition birthPositions foxConfig foxPos fox grid =
    uniform birthPositions
        |> Random.map
            (\babyPos ->
                birthFox foxConfig { babyPos = babyPos, parentPos = foxPos } fox grid
            )


birthRabbitAtRandomPosition : NonEmpty Position -> RabbitConfig -> Position -> Rabbit -> Grid -> Generator Grid
birthRabbitAtRandomPosition birthPositions rabbitConfig rabbitPos rabbit grid =
    uniform birthPositions
        |> Random.map
            (\babyPos ->
                birthRabbit rabbitConfig { babyPos = babyPos, parentPos = rabbitPos } rabbit grid
            )


eatRandomRabbit : NonEmpty Position -> FoxConfig -> Position -> Fox -> Grid -> Generator Grid
eatRandomRabbit rabbits foxConfig foxPos fox grid =
    uniform rabbits
        |> Random.map
            (\rabbitPos ->
                eatRabbit foxConfig { rabbitPos = rabbitPos, foxPos = foxPos } fox grid
            )


uniform : NonEmpty a -> Generator a
uniform ( first, rest ) =
    Random.uniform first rest



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
        [ gameTitle
        , Html.div [] [ Html.text "Loading" ]
        ]


playingView : GameState -> Html Msg
playingView state =
    Html.main_ []
        [ header state.energyHistory
        , gameBoard state.grid
        , controls state
        ]


lostView : GameState -> Html Msg
lostView state =
    Html.main_ []
        [ header state.energyHistory
        , gameBoard state.grid
        , lostControls state
        ]


gameTitle : Html a
gameTitle =
    Html.h1 [] [ Html.text "Ecosystem" ]


header : NonEmpty Grid.EnergyStat -> Html a
header history =
    Html.header []
        [ gameTitle
        , statsGraph history
        ]


lostControls : GameState -> Html Msg
lostControls state =
    Html.section [ Html.Attributes.class "controls" ]
        [ Html.h2 [] [ Html.text "Ecosystem collapsed!" ]
        , gameEndControls state
        ]


controls : GameState -> Html Msg
controls state =
    Html.section [ Html.Attributes.class "controls" ]
        [ speedControls state
        , foxConfigControls state.foxConfig
        , rabbitConfigControls state.rabbitConfig
        , rules
        ]


speedControls : GameState -> Html Msg
speedControls state =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Game Controls" ]
        , Html.div []
            [ resetButton
            , score state.daysElapsed
            ]
        , multiToggle
            { selectedItem = state.speed
            , tagger = SimSpeedChanged
            , toLabel = speedLabel
            , groupName = "speed"
            }
            [ Pause, Play, DoubleSpeed ]
        ]


resetButton : Html Msg
resetButton =
    Html.button [ Html.Events.onClick ResetClicked ] [ Html.text "Reset" ]


score : Int -> Html a
score daysElapsed =
    Html.span [ Html.Attributes.class "score" ]
        [ Html.text <| String.fromInt daysElapsed ++ " days" ]


speedLabel : SimSpeed -> String
speedLabel speed =
    case speed of
        Play ->
            "Play"

        DoubleSpeed ->
            "Fast"

        Pause ->
            "Pause"


gameEndControls : GameState -> Html Msg
gameEndControls state =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Game Over" ]
        , resetButton
        , score state.daysElapsed
        ]


foxConfigControls : FoxConfig -> Html Msg
foxConfigControls config =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Fox Config" ]
        , range
            { label = "Metabolism"
            , value = Energy.toInt config.costOfLiving
            , tagger = FoxConfigChanged FoxCostOfLiving << Energy.fromInt
            }
        , range
            { label = "Birth Cost"
            , value = Energy.toInt config.birthCost
            , tagger = FoxConfigChanged FoxBirthCost << Energy.fromInt
            }
        , range
            { label = "Rabbit Nutrition"
            , value = Energy.toInt config.rabbitNutrition
            , tagger = FoxConfigChanged RabbitNutrition << Energy.fromInt
            }
        ]


rabbitConfigControls : RabbitConfig -> Html Msg
rabbitConfigControls config =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Rabbit Config" ]
        , range
            { label = "Metabolism"
            , value = Energy.toInt config.costOfLiving
            , tagger = RabbitConfigChanged RabbitCostOfLiving << Energy.fromInt
            }
        , range
            { label = "Birth Cost"
            , value = Energy.toInt config.birthCost
            , tagger = RabbitConfigChanged RabbitBirthCost << Energy.fromInt
            }
        , range
            { label = "Grass Nutrition"
            , value = Energy.toInt config.grassNutrition
            , tagger = RabbitConfigChanged GrassNutrition << Energy.fromInt
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


statsGraph : NonEmpty Grid.EnergyStat -> Html a
statsGraph history =
    let
        toSeries accessor hist =
            hist
                |> NonEmpty.toList
                |> List.reverse
                |> List.indexedMap (\idx entry -> { day = idx, value = accessor entry })
    in
    view2Area
        (toFloat << .day)
        (toFloat << Energy.toInt << .value)
        (toSeries .foxes history)
        (toSeries .rabbits history)


view2Area : (data -> Float) -> (data -> Float) -> List data -> List data -> Html msg
view2Area toX toY series1 series2 =
    LineChart.viewCustom
        (areaChartConfig toX toY)
        [ LineChart.line LineChart.Colors.rust LineChart.Dots.none "Foxes" series1
        , LineChart.line LineChart.Colors.grayLight LineChart.Dots.none "Rabbits" series2
        ]


areaChartConfig : (a -> Float) -> (a -> Float) -> LineChart.Config a msg
areaChartConfig toX toY =
    { y = LineChart.Axis.default 300 "Energy" toY
    , x = LineChart.Axis.default 1200 "Days" toX
    , container = LineChart.Container.default "line-chart-1"
    , interpolation = LineChart.Interpolation.default
    , intersection = LineChart.Axis.Intersection.default
    , legends = LineChart.Legends.default
    , events = LineChart.Events.default
    , junk = LineChart.Junk.default
    , grid = LineChart.Grid.default
    , area = LineChart.Area.stacked 1
    , line = LineChart.Line.default
    , dots = LineChart.Dots.default
    }


rules : Html a
rules =
    Html.fieldset []
        [ Html.legend [] [ Html.text "Rules" ]
        , Html.ul
            []
            [ Html.li [] [ Html.text "Animals consume their metabolism cost." ]
            , Html.li [] [ Html.text "Animals with zero energy die." ]
            , Html.li [] [ Html.text "Animals may give birth at the 'birth cost' if they have enough energy. This energy is transferred to the offspring." ]
            , Html.li [] [ Html.text "Foxes may consume a neighboring rabbit, gaining the 'rabbit nutrition' energy." ]
            , Html.li [] [ Html.text "Foxes will randomly move if they have nothing else to do." ]
            , Html.li [] [ Html.text "Rabbits will randomly move if they are adjancent to a fox" ]
            , Html.li [] [ Html.text "Rabbits will consume grass, gaining the 'grass nutrition' energy, if they have nothing else to do." ]
            ]
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
            hungerAwareValue fox.energy (Html.text "\u{1F98A}")

        RabbitCell rabbit ->
            hungerAwareValue rabbit.energy (Html.text "🐰")

        Empty ->
            Html.text ""


hungerAwareValue : Energy -> Html a -> Html a
hungerAwareValue energy content =
    Html.span [ Html.Attributes.class <| hungerClass energy ] [ content ]


hungerClass : Energy -> String
hungerClass energy =
    if Energy.toInt energy > 3 then
        "full"

    else if Energy.toInt energy > 1 then
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
