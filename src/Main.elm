module Main exposing (main)

import Browser
import Html exposing (Html)
import List.Extra


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( [], Cmd.none )



-- MODEL


type alias Model =
    List Cell


width : Int
width =
    10


type Cell
    = Fox
    | Rabbit
    | Empty



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



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
