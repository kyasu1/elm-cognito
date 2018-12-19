module Model exposing (Customer)

import Date exposing (Date)
import Element exposing (..)
import Element.Input as Input
import Validate as V exposing (Validator)


type GenderType
    = Male
    | Female
    | Unknown


type OccupationType
    = Student { college : Maybe String }


type Field a
    = Field
        { value : a
        , initial : a
        , focused : Bool
        }


updateField : a -> Field a -> Field a
updateField value (Field field) =
    Field { field | value = value }


initField : a -> Field a -> Field a
initField value (Field field) =
    Field { field | value = value, initial = value }


getValue : Field a -> a
getValue (Field { value }) =
    value


type alias Customer =
    { name : Field String
    , kana : Field String
    , gender : Field GenderType
    , birthday : Field Date
    , occupation : Field OccupationType

    -- , address : List Address
    -- , mobile : Digits
    }



-- customerValidator : Validator ( FieldType, String ) Customer
-- customerValidator =
--     V.all
--         [ V.ifBlank (\customer -> getValue customer.name) ( Name "", "名前を入力してください" )
--         , V.ifBlank (.name >> getValue) ( Kana "", "かなを入力してください" )
--         ]


type Form a
    = Form
        { model : a
        , errors : List String
        , validator : Validator ( Msg, String ) a
        }



-- type Field
--     = Name String
--     | Kana String
--     | Gender GenderType
--     | Birthday String
--     | Occupation OccupationType


type Msg
    = Name String
    | Kana String
    | Gender GenderType
    | Birthday String
    | Occupation OccupationType


validateName : String -> Result String String
validateName name =
    if name /= "" then
        Ok name

    else
        Err "Name must not be empty"


update : Msg -> Form Customer -> Form Customer
update msg (Form { model, errors, validator }) =
    case msg of
        Name name ->
            Form { model | name = updateField name model.name }

        Kana kana ->
            { model | kana = updateField kana model.kana }

        Gender gender ->
            { model | gender = updateField gender model.gender }

        Birthday birthday ->
            { model | birthday = updateField birthday model.birthday }

        Occupation occupation ->
            { model | occupation = updateField occupation model.occupation }


view : Customer -> Element Msg
view form =
    Element.column []
        [ Element.el [] <|
            Input.text
                []
                { onChange = Name
                , text = getValue form.name
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "名前")
                }
        , Element.row [] <|
            [ Input.text []
                { onChange = Birthday
                , text = Date.toIsoString <| getValue form.birthday
                , placeholder = Nothing
                , label = Input.labelRight [] (text "年")
                }
            ]
        , Element.el [] <|
            case getValue form.occupation of
                Student { college } ->
                    Input.text
                        []
                        { onChange = \s -> Occupation <| Student { college = Just s }
                        , text = Maybe.withDefault "" college
                        , placeholder = Nothing
                        , label = Input.labelAbove [] (text "学校名")
                        }
        ]
