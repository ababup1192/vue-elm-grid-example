module Tests exposing (..)

import Test exposing (..)
import TestExp exposing (..)
import Main exposing (testGridData, sortList, filterList, Active(..), Order(..), Data, Number(..))


--  target modules


all : Test
all =
    describe "Grid layout module Test"
        [ "sort Not Active"
            => sortList Nothing Asc Asc testGridData
            === testGridData
        , "sort Name asc sort"
            => sortList (Just Name) Asc Asc testGridData
            === [ Data "Bruce Lee" <| Integer 9000
                , Data "Chuck Norris" Infinity
                , Data "Jackie Chan" <| Integer 7000
                , Data "Jet Li" <| Integer 8000
                ]
        , "sort Name desc sort"
            => sortList (Just Name) Desc Asc testGridData
            === [ Data "Jet Li" <| Integer 8000
                , Data "Jackie Chan" <| Integer 7000
                , Data "Chuck Norris" Infinity
                , Data "Bruce Lee" <| Integer 9000
                ]
        , "sort Power asc sort"
            => sortList (Just Power) Asc Asc testGridData
            === [ Data "Jackie Chan" <| Integer 7000
                , Data "Jet Li" <| Integer 8000
                , Data "Bruce Lee" <| Integer 9000
                , Data "Chuck Norris" Infinity
                ]
        , "sort Power desc sort"
            => sortList (Just Power) Asc Desc testGridData
            === [ Data "Chuck Norris" Infinity
                , Data "Bruce Lee" <| Integer 9000
                , Data "Jet Li" <| Integer 8000
                , Data "Jackie Chan" <| Integer 7000
                ]
        , "filter no query"
            => filterList "" testGridData
            === testGridData
        , "filter j"
            => filterList "j" testGridData
            === [ Data "Jackie Chan" <| Integer 7000
                , Data "Jet Li" <| Integer 8000
                ]
        , "filter i"
            => filterList "i" testGridData
            === [ Data "Chuck Norris" Infinity
                , Data "Jackie Chan" <| Integer 7000
                , Data "Jet Li" <| Integer 8000
                ]
        , "filter in"
            => filterList "in" testGridData
            === [ Data "Chuck Norris" Infinity ]
        , "filter 70"
            => filterList "70" testGridData
            === [ Data "Jackie Chan" <| Integer 7000 ]
        , "filter qawsderftgy"
            => filterList "qawsderftgy" testGridData
            === []
        ]
