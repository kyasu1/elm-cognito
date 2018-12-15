module Model exposing (Customer)

import Element exposing (..)
import Element.Input as Input


type GenderType
    = Male
    | Female
    | Unknown


type OccupationType
    = Student { college : Maybe String }


type Era
    = Meiji
    | Taisho
    | Showa
    | Heisei
    | Western


type alias BirthdayType =
    { era : Era
    , year : Int
    , month : Int
    , day : Int
    }


updateYear : BirthdayType -> String -> Maybe BirthdayType
updateYear birthday string =
    string
        |> String.toInt
        |> Maybe.andThen
            (\year ->
                case birthday.era of
                    Meiji ->
                        if year > 0 && year < 44 then
                            Just { birthday | year = year }

                        else
                            Nothing

                    Taisho ->
                        if year > 0 && year < 15 then
                            Just { birthday | year = year }

                        else
                            Nothing

                    Showa ->
                        if year > 0 && year < 64 then
                            Just { birthday | year = year }

                        else
                            Nothing

                    Heisei ->
                        if year > 0 && year < 30 then
                            Just { birthday | year = year }

                        else
                            Nothing

                    Western ->
                        if year > 0 && year < 2018 then
                            Just { birthday | year = year }

                        else
                            Nothing
            )


type alias Customer =
    { name : String
    , kana : String
    , gender : GenderType
    , birthday : BirthdayType
    , occupation : OccupationType

    -- , address : List Address
    -- , mobile : Digits
    }


type Field
    = Name String
    | Kana String
    | Gender GenderType
    | Birthday BirthdayType
    | Occupation OccupationType


updateField : Field -> Customer -> Customer
updateField field customer =
    case field of
        Name name ->
            { customer | name = name }

        Kana kana ->
            { customer | kana = kana }

        Gender gender ->
            { customer | gender = gender }

        Birthday birthday ->
            { customer | birthday = birthday }

        Occupation occupation ->
            { customer | occupation = occupation }


view : Customer -> Element Field
view customer =
    Element.column []
        [ Element.el [] <|
            Input.text
                []
                { onChange = Name
                , text = customer.name
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "名前")
                }
        , Element.row [] <|
            [ Input.text []
                { onChange = \s -> Birthday (updateYear customer.birthday s)
                , text = String.fromInt customer.birthday.year
                , placeholder = Nothing
                , label = Input.labelRight [] (text "年")
                }
            ]
        , Element.el [] <|
            case customer.occupation of
                Student { college } ->
                    Input.text
                        []
                        { onChange = \s -> Occupation <| Student { college = Just s }
                        , text = Maybe.withDefault "" college
                        , placeholder = Nothing
                        , label = Input.labelAbove [] (text "学校名")
                        }
        ]
