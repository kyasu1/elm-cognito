module WarekiTest exposing (suite)

import Date
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time
import Wareki exposing (..)


suite : Test
suite =
    describe "The Wareki module"
        [ describe "Wareki.fromGregorian" <|
            List.map (\( date, string ) -> test (Debug.toString date) (\_ -> Expect.equal (Wareki.fromGregorian date |> Maybe.map Wareki.toShortString) string))
                [ ( Date.fromCalendarDate 1868 Time.Jan 25, Just "M01/01/25" )
                , ( Date.fromCalendarDate 1870 Time.Apr 15, Just "M03/04/15" )
                , ( Date.fromCalendarDate 1912 Time.Jul 30, Just "T01/07/30" )
                , ( Date.fromCalendarDate 1926 Time.Dec 25, Just "S01/12/25" )
                , ( Date.fromCalendarDate 1989 Time.Jan 8, Just "H01/01/08" )
                , ( Date.fromCalendarDate 2018 Time.Dec 15, Just "H30/12/15" )
                , ( Date.fromCalendarDate 2019 Time.Apr 30, Just "H31/04/30" )
                , ( Date.fromCalendarDate 2019 Time.May 1, Nothing )
                ]
        , describe "Wareki.fromCalendarDate"
            [ test "Create a wareki from a calendar date in Wareki" <|
                \_ ->
                    let
                        showa =
                            Wareki.fromCalendarDate Wareki.Showa 44 Time.Jan 11
                    in
                    Expect.equal (showa |> Maybe.map Wareki.toShortString) (Just "S44/01/11")
            ]
        , describe "Wareki.toGregorian"
            [ test "Convert Wareki to Gregorian date" <|
                \_ ->
                    let
                        showa =
                            Wareki.fromCalendarDate Wareki.Showa 44 Time.Jan 11
                    in
                    Expect.equal (showa |> Maybe.map Wareki.toGregorian) (Just (Date.fromCalendarDate 1969 Time.Jan 11))
            ]
        ]
