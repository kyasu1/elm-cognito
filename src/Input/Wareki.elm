module Input.Wareki exposing (viewBirthday)

import Date exposing (Date)
import Element exposing (..)
import Element.Input as Input
import Wareki exposing (Gengoh)


type State
    = NotSelected
    | GengohSelected (Maybe Gengoh)
    | YearSelected (Maybe Gengoh) Int
    | MonthSelected (Maybe Gengoh) Int Date.Month
    | DateSelected (Maybe Gengoh) Int Date.Month Int


init : State
init =
    NotSelected


initWithDate : Gengoh -> Int -> Date.Month -> Int -> State
initWithDate g y m d =
    DateSelected (Just g) y m d


type Msg
    = ChangedGengoh (Maybe Gengoh)
    | ChangedYear String
    | ChangedMonth String
    | ChangedDay String


update : Msg -> State -> State
update msg state =
    case msg of
        ChangedGengoh maybeGengoh ->
            GengohSelected maybeGengoh

        -- ChangedYear string ->
        --   case state of
        --     YearSelected maybeGengoh year ->
        --
        --   String.toInt string
        --   |> Maybe.map (\int -> )
        --
        -- state
        _ ->
            state


viewBirthday :
    { onChange : String -> msg
    , state : State
    , label : String
    }
    -> Element msg
viewBirthday { onChange, state, label } =
    case state of
        NotSelected ->
            viewGengoh Nothing

        _ ->
            text ""


viewGengoh : Maybe Gengoh -> Element Msg
viewGengoh maybeGengoh =
    Input.radio []
        { selected = Just maybeGengoh
        , onChange = ChangedGengoh
        , label = Input.labelAbove [] (text "生年月日")
        , options =
            [ Input.option Nothing (text "西暦")
            , Input.option (Just Wareki.Meiji) (text "明治")
            , Input.option (Just Wareki.Taisho) (text "大正")
            , Input.option (Just Wareki.Showa) (text "昭和")
            , Input.option (Just Wareki.Heisei) (text "平成")
            ]
        }


viewYear : Int -> Element Msg
viewYear year =
    Input.text []
        { onChange = ChangedYear
        , text = String.fromInt year
        , placeholder = Nothing
        , label = Input.labelRight [] (text "年")
        }
