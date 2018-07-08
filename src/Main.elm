module Main exposing (..)

import Html exposing (Html, div, form, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick, onInput)


---- MODEL ----


type ColumnKey
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


order2string : Order -> String
order2string order =
    case order of
        Desc ->
            "dsc"

        Asc ->
            "asc"


type alias ItemOrder =
    Maybe ( ColumnKey, Order )


type alias Model =
    { searchQuery : String
    , gridData : List Data
    , itemOrder : ItemOrder
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
      , itemOrder = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = InputSearchQuery String
    | SwitchOrder ColumnKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ itemOrder } as model) =
    case msg of
        InputSearchQuery q ->
            { model | searchQuery = q } ! []

        SwitchOrder columnKey ->
            case itemOrder of
                Just ( _, order ) ->
                    { model | itemOrder = Just ( columnKey, switchOrder order ) } ! []

                Nothing ->
                    { model | itemOrder = Just ( columnKey, Asc ) } ! []



---- VIEW ----


type alias Column data =
    { key : ColumnKey
    , header : String
    , cell : data -> String
    , sorter : data -> data -> Basics.Order
    }


nameCol : Column Data
nameCol =
    { key = Name
    , header = "Name"
    , cell = .name
    , sorter = \d1 d2 -> compare d1.name d2.name
    }


powerCol : Column Data
powerCol =
    { key = Power
    , header = "Power"
    , cell = .power >> toString
    , sorter = \d1 d2 -> compare d1.power d2.power
    }


columns : List (Column Data)
columns =
    [ nameCol, powerCol ]


view : Model -> Html Msg
view { gridData, searchQuery, itemOrder } =
    let
        lwQuery =
            String.toLower searchQuery

        data2tr data =
            tr []
                (columns
                    |> List.map
                        (\column ->
                            td [] [ text (column.cell data) ]
                        )
                )

        gridData2trList =
            List.map data2tr
                (gridData
                    |> sortList columns itemOrder
                    |> filterList columns lwQuery
                )

        activeClass columnKey =
            Maybe.withDefault "" <|
                Maybe.map
                    (\( key, _ ) ->
                        if columnKey == key then
                            "active"
                        else
                            ""
                    )
                    itemOrder

        arrowClass columnKey =
            Maybe.withDefault "" <|
                Maybe.map
                    (\( key, order ) ->
                        if columnKey == key then
                            "arrow " ++ order2string order
                        else
                            ""
                    )
                    itemOrder
    in
        div []
            [ form []
                [ text "Search"
                , input [ onInput InputSearchQuery ] []
                ]
            , table []
                [ thead []
                    [ tr []
                        (columns
                            |> List.map
                                (\column ->
                                    th
                                        [ class <| activeClass column.key
                                        , onClick <| SwitchOrder column.key
                                        ]
                                        [ text column.header
                                        , span [ class <| arrowClass column.key ] []
                                        ]
                                )
                        )
                    ]
                , tbody []
                    gridData2trList
                ]
            ]


orderProduct : Order -> Basics.Order -> Basics.Order
orderProduct order o =
    case order of
        Asc ->
            o

        Desc ->
            case o of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT


sortList : List (Column data) -> ItemOrder -> List data -> List data
sortList columns itemOrder gridData =
    case itemOrder of
        Just ( columnKey, order ) ->
            columns
                |> List.filter (\column -> column.key == columnKey)
                |> List.head
                |> Maybe.map
                    (\column ->
                        List.sortWith
                            (\a b -> orderProduct order (column.sorter a b))
                            gridData
                    )
                |> Maybe.withDefault gridData

        Nothing ->
            gridData


filterList : List (Column data) -> String -> List data -> List data
filterList columns lwQuery gridData =
    List.filter
        (\data ->
            columns
                |> List.any
                    (\column ->
                        String.contains lwQuery (String.toLower (column.cell data))
                    )
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
