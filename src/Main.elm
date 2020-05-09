module Main exposing (main)

import Html exposing (Html)
import List.Extra


main : Html a
main =
    view initialModel


type alias Model =
    List Cell


initialModel : Model
initialModel =
    [ Fox
    , Rabbit
    , Empty
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    , Empty
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Empty
    , Fox
    , Empty
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Rabbit
    , Empty
    , Empty
    , Empty
    , Rabbit
    , Empty
    , Fox
    ]


width : Int
width =
    10


type Cell
    = Fox
    | Rabbit
    | Empty



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
