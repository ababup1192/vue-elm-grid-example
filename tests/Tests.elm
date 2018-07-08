module Tests exposing (..)

import Main exposing (ColumnKey(..), Data, Order(..), columns, filterList, infinity, sortList, testGridData)
import Test exposing (..)
import TestExp exposing (..)



--  target modules


all : Test
all =
    describe "Grid layout module Test"
        [ "sort Not Active"
            => sortList columns Nothing testGridData
            === testGridData
        , "sort Name asc sort"
            => sortList columns (Just ( Name, Asc )) testGridData
            === [ Data "Bruce Lee" 9000
                , Data "Chuck Norris" infinity
                , Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                ]
        , "sort Name desc sort"
            => sortList columns (Just ( Name, Desc )) testGridData
            === [ Data "Jet Li" 8000
                , Data "Jackie Chan" 7000
                , Data "Chuck Norris" infinity
                , Data "Bruce Lee" 9000
                ]
        , "sort Power asc sort"
            => sortList columns (Just ( Power, Asc )) testGridData
            === [ Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                , Data "Bruce Lee" 9000
                , Data "Chuck Norris" infinity
                ]
        , "sort Power desc sort"
            => sortList columns (Just ( Power, Desc )) testGridData
            === [ Data "Chuck Norris" infinity
                , Data "Bruce Lee" 9000
                , Data "Jet Li" 8000
                , Data "Jackie Chan" 7000
                ]
        , "filter no query"
            => filterList columns "" testGridData
            === testGridData
        , "filter j"
            => filterList columns "j" testGridData
            === [ Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                ]
        , "filter i"
            => filterList columns "i" testGridData
            === [ Data "Chuck Norris" infinity
                , Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                ]
        , "filter in"
            => filterList columns "in" testGridData
            === [ Data "Chuck Norris" infinity ]
        , "filter 70"
            => filterList columns "70" testGridData
            === [ Data "Jackie Chan" 7000 ]
        , "filter qawsderftgy"
            => filterList columns "qawsderftgy" testGridData
            === []
        ]
