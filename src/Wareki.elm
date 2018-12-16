module Wareki exposing (Gengoh(..), Wareki, fromCalendarDate, fromGregorian, toGregorian, toLongString, toShortString)

import Date exposing (Date)
import Element exposing (..)
import Element.Input as Input
import Time


type Gengoh
    = Meiji
    | Taisho
    | Showa
    | Heisei
    | Unknown


type Wareki
    = Wareki Gengoh Int Date.Month Int


toGregorian : Wareki -> Date
toGregorian (Wareki g y m d) =
    case g of
        Meiji ->
            Date.fromCalendarDate (Date.year meiji1 + y - 1) m d

        Taisho ->
            Date.fromCalendarDate (Date.year taisho1 + y - 1) m d

        Showa ->
            Date.fromCalendarDate (Date.year showa1 + y - 1) m d

        Heisei ->
            Date.fromCalendarDate (Date.year heisei1 + y - 1) m d

        Unknown ->
            Date.fromCalendarDate (Date.year unknown1 + y - 1) m d


fromGregorianHelper : Gengoh -> Date -> Date -> Wareki
fromGregorianHelper g date1 date =
    Wareki g (Date.diff Date.Years date1 date + 1) (Date.month date) (Date.day date)


fromGregorian : Date -> Maybe Wareki
fromGregorian date =
    if inMeiji date then
        Just <| fromGregorianHelper Meiji meiji1 date

    else if inTaisho date then
        Just <| fromGregorianHelper Taisho taisho1 date

    else if inShowa date then
        Just <| fromGregorianHelper Showa showa1 date

    else if inHeisei date then
        Just <| fromGregorianHelper Heisei heisei1 date

    else
        Nothing


fromCalendarDate : Gengoh -> Int -> Time.Month -> Int -> Maybe Wareki
fromCalendarDate g y m d =
    case g of
        Meiji ->
            if inMeiji <| Date.fromCalendarDate (Date.year meiji1 + y - 1) m d then
                Just (Wareki Meiji y m d)

            else
                Nothing

        Taisho ->
            if inTaisho <| Date.fromCalendarDate (Date.year taisho1 + y - 1) m d then
                Just (Wareki Taisho y m d)

            else
                Nothing

        Showa ->
            if inShowa <| Date.fromCalendarDate (Date.year showa1 + y - 1) m d then
                Just (Wareki Showa y m d)

            else
                Nothing

        Heisei ->
            if inHeisei <| Date.fromCalendarDate (Date.year heisei1 + y - 1) m d then
                Just (Wareki Heisei y m d)

            else
                Nothing

        _ ->
            Nothing



-- Wareki Conversion to String


padLeftZero2 =
    String.padLeft 2 '0' << String.fromInt


toMonthString : Time.Month -> String
toMonthString m =
    m |> Date.monthToNumber |> padLeftZero2


gengohToShort : Gengoh -> String
gengohToShort gengoh =
    case gengoh of
        Meiji ->
            "M"

        Taisho ->
            "T"

        Showa ->
            "S"

        Heisei ->
            "H"

        Unknown ->
            "U"


gengohToLong : Gengoh -> String
gengohToLong gengoh =
    case gengoh of
        Meiji ->
            "明治"

        Taisho ->
            "大正"

        Showa ->
            "昭和"

        Heisei ->
            "平成"

        Unknown ->
            "未定"


toShortString : Wareki -> String
toShortString (Wareki g y m d) =
    let
        gg =
            gengohToShort g

        yy =
            padLeftZero2 y

        mm =
            toMonthString m

        dd =
            padLeftZero2 d
    in
    String.concat [ gg, yy, "/", mm, "/", dd ]


toLongString : Wareki -> String
toLongString (Wareki g y m d) =
    let
        gg =
            gengohToLong g

        yy =
            padLeftZero2 y

        mm =
            toMonthString m

        dd =
            padLeftZero2 d
    in
    String.concat [ gg, yy, "年", mm, "月", dd, "日" ]



--


meiji1 : Date
meiji1 =
    Date.fromCalendarDate 1868 Time.Jan 25


taisho1 : Date
taisho1 =
    Date.fromCalendarDate 1912 Time.Jul 30


showa1 : Date
showa1 =
    Date.fromCalendarDate 1926 Time.Dec 25


heisei1 : Date
heisei1 =
    Date.fromCalendarDate 1989 Time.Jan 8


unknown1 : Date
unknown1 =
    Date.fromCalendarDate 2019 Time.May 1


inMeiji : Date -> Bool
inMeiji date =
    Date.isBetween meiji1 (Date.add Date.Days -1 taisho1) date


inTaisho : Date -> Bool
inTaisho date =
    Date.isBetween taisho1 (Date.add Date.Days -1 showa1) date


inShowa : Date -> Bool
inShowa date =
    Date.isBetween showa1 (Date.add Date.Days -1 heisei1) date


inHeisei : Date -> Bool
inHeisei date =
    Date.isBetween heisei1 (Date.add Date.Days -1 unknown1) date



--


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
