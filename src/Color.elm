module Color exposing
    ( base
    , black
    , dark
    , darker
    , darkest
    , grey
    , light
    , lighter
    , lightest
    , red
    , teal
    , white
    , yellow
    )

import Element exposing (Color, rgb255, rgba)



{--
Grey
--}


black =
    rgb255 34 41 47


white =
    rgb255 255 255 255


type Lightness
    = Darkest
    | Darker
    | Dark
    | Base
    | Light
    | Lighter
    | Lightest


darkest : (Lightness -> Color) -> Color
darkest hue =
    hue Darkest


darker : (Lightness -> Color) -> Color
darker hue =
    hue Darker


dark : (Lightness -> Color) -> Color
dark hue =
    hue Dark


base : (Lightness -> Color) -> Color
base hue =
    hue Base


lightest : (Lightness -> Color) -> Color
lightest hue =
    hue Lightest


lighter : (Lightness -> Color) -> Color
lighter hue =
    hue Lighter


light : (Lightness -> Color) -> Color
light hue =
    hue Light



--


grey : Lightness -> Color
grey lightness =
    case lightness of
        Darkest ->
            rgb255 61 72 82

        Darker ->
            rgb255 96 111 123

        Dark ->
            rgb255 135 149 161

        Base ->
            rgb255 210 194 204

        Light ->
            rgb255 218 225 231

        Lighter ->
            rgb255 241 245 248

        Lightest ->
            rgb255 248 250 252


red : Lightness -> Color
red lightness =
    case lightness of
        Darkest ->
            rgb255 59 13 12

        Darker ->
            rgb255 98 27 24

        Dark ->
            rgb255 204 31 26

        Base ->
            rgb255 227 52 47

        Light ->
            rgb255 239 87 83

        Lighter ->
            rgb255 249 172 170

        Lightest ->
            rgb255 252 235 234


yellow : Lightness -> Color
yellow lightness =
    case lightness of
        Darkest ->
            rgb255 69 52 17

        Darker ->
            rgb255 104 79 29

        Dark ->
            rgb255 242 208 36

        Base ->
            rgb255 255 237 74

        Light ->
            rgb255 255 243 130

        Lighter ->
            rgb255 255 249 194

        Lightest ->
            rgb255 252 251 235


teal : Lightness -> Color
teal lightness =
    case lightness of
        Darkest ->
            rgb255 13 51 49

        Darker ->
            rgb255 32 80 79

        Dark ->
            rgb255 56 168 157

        Base ->
            rgb255 77 192 181

        Light ->
            rgb255 100 213 202

        Lighter ->
            rgb255 160 240 237

        Lightest ->
            rgb255 232 255 254
