module JpnDate exposing (view, year)

import Element exposing (..)
import Element.Input as Input


view : Element msg
view =
    row []
        []


yToMeiji : Int -> String
yToMeiji y =
    "M" ++ (String.padLeft 2 '0' <| String.fromInt (y - 1867))


yToTaisho : Int -> String
yToTaisho y =
    "T" ++ (String.padLeft 2 '0' <| String.fromInt (y - 1911))


yToShowa : Int -> String
yToShowa y =
    "S" ++ (String.padLeft 2 '0' <| String.fromInt (y - 1925))


yToHeisei : Int -> String
yToHeisei y =
    "S" ++ (String.padLeft 2 '0' <| String.fromInt (y - 1988))


yToUnknown : Int -> String
yToUnknown y =
    "U" ++ (String.padLeft 2 '0' <| String.fromInt (y - 2018))


year : Int -> List String
year y =
    if y < 1867 then
        [ String.fromInt y ]

    else if y > 1867 && y < 1912 then
        [ String.fromInt y, yToMeiji y ]

    else if y == 1912 then
        [ "1912", "M45", "T01" ]

    else if y > 1912 && y < 1926 then
        [ String.fromInt y, yToTaisho y ]

    else if y == 1926 then
        [ "1926", yToTaisho y, yToShowa y ]

    else if y > 1926 && y < 1989 then
        [ String.fromInt y, yToShowa y ]

    else if y == 1989 then
        [ "1989", yToShowa y, yToHeisei y ]

    else if y > 1989 && y < 2019 then
        [ String.fromInt y, yToHeisei y ]

    else if y == 2019 then
        [ "2019", yToHeisei y, yToUnknown y ]

    else
        [ String.fromInt y, yToUnknown y ]
