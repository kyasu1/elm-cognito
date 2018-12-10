module Form exposing (Form)


type F
    = Username
    | Password
    | Age
    | Gender


type alias M =
    { form : Form F }


type Form a b
    = Form (List ( b, Value a ))



-- type FieldState
--     = FieldState
--         { value : Value a
--         , focused : Bool
--         }


type Value a
    = String String
    | Bool Bool
    | Int Int


fromString : String -> Value String
fromString string =
    String string


fromBool : Bool -> Value Bool
fromBool bool =
    Bool bool


fromInt : Int -> Value Int
fromInt int =
    Int int


toString : Value String -> Maybe String
toString value =
    case value of
        String string ->
            Just string

        _ ->
            Nothing


toBool : Value Bool -> Maybe Bool
toBool value =
    case value of
        Bool bool ->
            Just bool

        _ ->
            Nothing


toInt : Value Int -> Maybe Int
toInt value =
    case value of
        Int int ->
            Just int

        _ ->
            Nothing
