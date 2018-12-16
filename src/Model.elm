module Model exposing (Customer)

import Date exposing (Date)
import Element exposing (..)
import Element.Input as Input


type GenderType
    = Male
    | Female
    | Unknown


type OccupationType
    = Student { college : Maybe String }


type alias Customer =
    { name : String
    , kana : String
    , gender : GenderType
    , birthday : Date
    , occupation : OccupationType

    -- , address : List Address
    -- , mobile : Digits
    }


type Field
    = Name String
    | Kana String
    | Gender GenderType
    | BirthdayYear String
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

        BirthdayYear year ->
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
                { onChange = BirthdayYear
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
