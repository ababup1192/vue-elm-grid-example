module Main exposing (..)

import Html exposing (Html, text, div, table, thead, tbody, span, tr, th, td, input, form)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onInput, onClick)


---- MODEL ----


type Active
    = Name
    | Power


type alias Data =
    { name : String, power : Float }


type Order
    = Desc
    | Asc


switchOrder : Order -> Order
switchOrder order =
    case order of
        Desc ->
            Asc

        Asc ->
            Desc


type alias Model =
    { searchQuery : String
    , gridData : List Data
    , activeMaybe : Maybe Active
    , nameOrder : Order
    , powerOrder : Order
    }


infinity : Float
infinity =
    1 / 0


testGridData : List Data
testGridData =
    [ Data "Chuck Norris" infinity
    , Data "Bruce Lee" 9000
    , Data "Jackie Chan" 7000
    , Data "Jet Li" 8000
    ]


init : ( Model, Cmd Msg )
init =
    ( { searchQuery = ""
      , gridData = testGridData
      , activeMaybe = Nothing
      , nameOrder = Asc
      , powerOrder = Asc
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = InputSearchQuery String
    | SwitchOrder Active


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ nameOrder, powerOrder } as model) =
    case msg of
        InputSearchQuery q ->
            { model | searchQuery = q } ! []

        SwitchOrder act ->
            case act of
                Name ->
                    { model | nameOrder = switchOrder nameOrder, activeMaybe = Just act } ! []

                Power ->
                    { model | powerOrder = switchOrder powerOrder, activeMaybe = Just act } ! []



---- VIEW ----


view : Model -> Html Msg
view { gridData, activeMaybe, nameOrder, powerOrder, searchQuery } =
    let
        lwQuery =
            String.toLower searchQuery

        data2tr { name, power } =
            tr []
                [ td [] [ text name ]
                , td [] [ text <| toString power ]
                ]

        gridData2trList =
            List.map data2tr (gridData |> sortList activeMaybe nameOrder powerOrder |> filterList lwQuery)

        activeClass active =
            Maybe.withDefault "" <|
                Maybe.map
                    (\act ->
                        if active == act then
                            "active"
                        else
                            ""
                    )
                    activeMaybe

        arrowClass order =
            case order of
                Asc ->
                    "arrow asc"

                Desc ->
                    "arrow dsc"
    in
        div []
            [ form []
                [ text "Search"
                , input [ onInput InputSearchQuery ] []
                ]
            , table []
                [ thead []
                    [ tr []
                        [ th [ class <| activeClass Name, onClick <| SwitchOrder Name ]
                            [ text "Name"
                            , span [ class <| arrowClass nameOrder ] []
                            ]
                        , th [ class <| activeClass Power, onClick <| SwitchOrder Power ]
                            [ text "Power"
                            , span [ class <| arrowClass powerOrder ] []
                            ]
                        ]
                    ]
                , tbody []
                    gridData2trList
                ]
            ]


flippedComparison : comparable -> comparable -> Basics.Order
flippedComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


orderProduct : Order -> comparable -> comparable -> Basics.Order
orderProduct order a b =
    case order of
        Asc ->
            compare a b

        Desc ->
            flippedComparison a b


sortList : Maybe Active -> Order -> Order -> List Data -> List Data
sortList activeMaybe nameOrder powerOrder gridData =
    case activeMaybe of
        Just active ->
            (case active of
                Name ->
                    List.sortWith (\a b -> orderProduct nameOrder a.name b.name)

                Power ->
                    List.sortWith (\a b -> orderProduct powerOrder a.power b.power)
            )
                gridData

        Nothing ->
            gridData


filterList : String -> List Data -> List Data
filterList lwQuery gridData =
    List.filter
        (\{ name, power } ->
            String.contains lwQuery (String.toLower name)
                || String.contains lwQuery (String.toLower <| toString power)
        )
        gridData



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
