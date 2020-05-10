module Main exposing (main)

import Browser
import Html exposing (Html)
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
    ( [], Random.generate GridGenerated gridGenerator )



-- MODEL


type alias Model =
    Grid


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GridGenerated grid ->
            ( grid, Cmd.none )

        Tick ->
            ( model, Random.generate GridGenerated gridGenerator )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (\_ -> Tick)



-- VIEW


view : Model -> Html a
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Ecosystem" ]
        , Html.table [] <| List.map viewRow <| List.Extra.groupsOf width model
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
