module Main exposing (..)

import Html exposing (Html, text, div, table, thead, tbody, span, tr, th, td, input, form)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onInput, onClick)


---- MODEL ----


type Number
    = Integer Int
    | Infinity


type Active
    = Name
    | Power


type alias Data =
    { name : String, power : Number }


number2string : Number -> String
number2string num =
    case num of
        Integer int ->
            toString int

        Infinity ->
            "Infinity"


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
    , active : Maybe Active
    , nameOrder : Order
    , powerOrder : Order
    }


init : ( Model, Cmd Msg )
init =
    ( { searchQuery = ""
      , gridData =
            [ Data "Chuck Norris" Infinity
            , Data "Bruce Lee" <| Integer 9000
            , Data "Jackie Chan" <| Integer 7000
            , Data "Jet Li" <| Integer 8000
            ]
      , active = Nothing
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
                    { model | nameOrder = switchOrder nameOrder, active = Just act } ! []

                Power ->
                    { model | powerOrder = switchOrder powerOrder, active = Just act } ! []



---- VIEW ----


view : Model -> Html Msg
view { gridData, active, nameOrder, powerOrder, searchQuery } =
    let
        lwQuery =
            String.toLower searchQuery

        flippedComparison a b =
            case compare a b of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT

        sortList =
            case active of
                Just act ->
                    case act of
                        Name ->
                            case nameOrder of
                                Desc ->
                                    List.sortWith
                                        (\a b ->
                                            case compare a.name b.name of
                                                LT ->
                                                    GT

                                                EQ ->
                                                    EQ

                                                GT ->
                                                    LT
                                        )
                                        gridData

                                Asc ->
                                    List.sortBy .name gridData

                        Power ->
                            case powerOrder of
                                Desc ->
                                    List.sortWith
                                        (\a b ->
                                            case ( a.power, b.power ) of
                                                ( Infinity, Integer _ ) ->
                                                    LT

                                                ( Integer _, Infinity ) ->
                                                    GT

                                                ( Integer aa, Integer bb ) ->
                                                    flippedComparison aa bb

                                                ( _, _ ) ->
                                                    EQ
                                        )
                                        gridData

                                Asc ->
                                    List.sortWith
                                        (\a b ->
                                            case ( a.power, b.power ) of
                                                ( Infinity, Integer _ ) ->
                                                    GT

                                                ( Integer _, Infinity ) ->
                                                    LT

                                                ( Integer aa, Integer bb ) ->
                                                    compare aa bb

                                                ( _, _ ) ->
                                                    EQ
                                        )
                                        gridData

                Nothing ->
                    gridData

        filterList sortedList =
            List.filter
                (\{ name, power } ->
                    String.contains lwQuery (String.toLower name)
                        || String.contains lwQuery (String.toLower <| number2string power)
                )
                sortedList

        data2tr { name, power } =
            tr []
                [ td [] [ text name ]
                , td [] [ text <| number2string power ]
                ]

        gridData2trList =
            List.map data2tr (sortList |> filterList)

        activeClass act =
            case active of
                Nothing ->
                    ""

                Just a ->
                    if a == act then
                        "active"
                    else
                        ""

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



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
