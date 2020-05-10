module Main exposing (main)

import Browser
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
    , grid = []
    }


type SimSpeed
    = Play
    | Pause
    | DoubleSpeed


type alias Grid =
    List Cell


width : Int
width =
    10


type Cell
    = Fox
    | Rabbit
    | Empty


gridGenerator : Generator Grid
gridGenerator =
    Random.list 100 cellGenerator


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
            ( model, Random.generate GridGenerated gridGenerator )

        SimSpeedChanged newSpeed ->
            ( { model | speed = newSpeed }, Cmd.none )



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
        [ Html.table [] <| List.map viewRow <| List.Extra.groupsOf width grid
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
