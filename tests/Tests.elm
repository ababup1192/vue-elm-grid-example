module Tests exposing (..)

import Test exposing (..)
import TestExp exposing (..)
import Main exposing (testGridData, infinity, sortList, filterList, Active(..), Order(..), Data)


--  target modules


all : Test
all =
    describe "Grid layout module Test"
        [ "sort Not Active"
            => sortList Nothing testGridData
            === testGridData
        , "sort Name asc sort"
            => sortList (Just ( Name, Asc )) testGridData
            === [ Data "Bruce Lee" 9000
                , Data "Chuck Norris" infinity
                , Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                ]
        , "sort Name desc sort"
            => sortList (Just ( Name, Desc )) testGridData
            === [ Data "Jet Li" 8000
                , Data "Jackie Chan" 7000
                , Data "Chuck Norris" infinity
                , Data "Bruce Lee" 9000
                ]
        , "sort Power asc sort"
            => sortList (Just ( Power, Asc )) testGridData
            === [ Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                , Data "Bruce Lee" 9000
                , Data "Chuck Norris" infinity
                ]
        , "sort Power desc sort"
            => sortList (Just ( Power, Desc )) testGridData
            === [ Data "Chuck Norris" infinity
                , Data "Bruce Lee" 9000
                , Data "Jet Li" 8000
                , Data "Jackie Chan" 7000
                ]
        , "filter no query"
            => filterList "" testGridData
            === testGridData
        , "filter j"
            => filterList "j" testGridData
            === [ Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                ]
        , "filter i"
            => filterList "i" testGridData
            === [ Data "Chuck Norris" infinity
                , Data "Jackie Chan" 7000
                , Data "Jet Li" 8000
                ]
        , "filter in"
            => filterList "in" testGridData
            === [ Data "Chuck Norris" infinity ]
        , "filter 70"
            => filterList "70" testGridData
            === [ Data "Jackie Chan" 7000 ]
        , "filter qawsderftgy"
            => filterList "qawsderftgy" testGridData
            === []
        ]
