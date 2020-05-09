module Main exposing (main)

import Html exposing (Html)


main : Html a
main =
    view initialModel


type alias Model =
    List Cell


initialModel : Model
initialModel =
    [ Fox, Rabbit, Empty, Empty, Empty, Rabbit ]


type Cell
    = Fox
    | Rabbit
    | Empty



-- VIEW


view : Model -> Html a
view model =
    Html.text <| String.join "" <| List.map cellSymbol model


cellSymbol : Cell -> String
cellSymbol cell =
    case cell of
        Fox ->
            "\u{1F98A}"

        Rabbit ->
            "🐰"

        Empty ->
            ""
