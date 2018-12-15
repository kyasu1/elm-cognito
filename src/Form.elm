module Form exposing
    ( Form(..)
    , Msg(..)
    , append
    , appendValidator
    , appendWithParser
    , empty
    , getErrors
    , getFieldAsBool
    , getFieldAsInt
    , getFieldAsString
    , hasFocused
    , isFocusing
    , isValid
    , remove
    , setErrors
    , setIntField
    , toList
    , update
    , validate
    , validateBoolField
    , validateStringField
    )

{-| -}

import Validator exposing (Validator, liftMap)


type FieldValue
    = String String
    | Bool Bool
    | Int Int


fromString : String -> FieldValue
fromString string =
    String string


fromBool : Bool -> FieldValue
fromBool bool =
    Bool bool


fromInt : Int -> FieldValue
fromInt int =
    Int int


toString : FieldValue -> Maybe String
toString value =
    case value of
        String string ->
            Just string

        _ ->
            Nothing


toBool : FieldValue -> Maybe Bool
toBool value =
    case value of
        Bool bool ->
            Just bool

        _ ->
            Nothing


toInt : FieldValue -> Maybe Int
toInt value =
    case value of
        Int int ->
            Just int

        _ ->
            Nothing


{-| -}
type Form a
    = Form (Model a)


type alias Model a =
    { fields : Fields a
    , isSubmitted : Bool
    , focus : Maybe a
    , errors : List ( a, String )
    , validator : Validator (Form a) ( a, String )
    }


type alias Fields a =
    List ( a, FieldState )


type alias FieldState =
    { value : FieldValue
    , focused : Bool
    , parser : FieldValue -> FieldValue
    }


{-|


# Form constructor

@doc emtpy, append

-}
empty : Form a
empty =
    Form
        { fields = []
        , isSubmitted = False
        , focus = Nothing
        , errors = []
        , validator = Validator.concat []
        }


addStringField : ( a, String ) -> Form a -> Form a
addStringField ( ident, string ) (Form model) =
    Form { model | fields = ( ident, FieldState (String string) False identity ) :: model.fields }


append : { ident : a, value : FieldValue } -> Form a -> Form a
append { ident, value } form =
    appendWithParser { ident = ident, value = value, parser = identity } form


appendWithParser : { ident : a, value : FieldValue, parser : FieldValue -> FieldValue } -> Form a -> Form a
appendWithParser { ident, value, parser } (Form model) =
    Form { model | fields = ( ident, FieldState value False parser ) :: model.fields }


remove : a -> Form a -> Form a
remove field (Form model) =
    Form { model | fields = model.fields |> List.filter (\( a, _ ) -> a /= field) }


appendValidator : Validator (Form a) ( a, String ) -> Form a -> Form a
appendValidator validator (Form model) =
    Form { model | validator = Validator.concat [ model.validator, validator ] }



--


validate : Form a -> Form a
validate (Form model) =
    Form { model | errors = Validator.errors model.validator (Form model) }


toList : Form a -> List ( a, FieldValue )
toList (Form model) =
    model.fields
        |> List.map (\( a, state ) -> ( a, state.value ))


{-| -}
getFieldAsString : a -> Form a -> Maybe String
getFieldAsString a (Form form) =
    getField a form.fields
        |> Maybe.map .value
        |> Maybe.andThen toString


getFieldAsBool : a -> Form a -> Maybe Bool
getFieldAsBool a (Form form) =
    getField a form.fields
        |> Maybe.map .value
        |> Maybe.andThen toBool


getFieldAsInt : a -> Form a -> Maybe Int
getFieldAsInt a (Form form) =
    getField a form.fields
        |> Maybe.map .value
        |> Maybe.andThen toInt


mapError : a -> e -> ( a, e )
mapError a e =
    ( a, e )



-- {-| -}
-- validateField : a -> Validator e (Maybe (FieldState a)) -> Validator ( a, e ) (Form a)
-- validateField a =
--     liftMap (mapError a) (getField a)


{-| -}
getErrors : a -> Form a -> List String
getErrors a (Form form) =
    let
        focused =
            getField a form.fields
                |> Maybe.map .focused
                |> Maybe.withDefault False
    in
    if focused then
        form.errors
            |> List.filter (\( f, m ) -> f == a)
            |> List.map Tuple.second

    else
        []


{-| -}
setErrors : List ( a, String ) -> Form a -> Form a
setErrors errors (Form form) =
    Form { form | errors = errors ++ form.errors }


{-| -}
isValid : Form a -> Bool
isValid (Form form) =
    form.errors == []


{-| -}
hasFocused : a -> Form a -> Bool
hasFocused field (Form form) =
    getField field form.fields
        |> Maybe.map .focused
        |> Maybe.withDefault False


{-| -}
isFocusing : a -> Form a -> Bool
isFocusing field (Form form) =
    form.focus
        |> Maybe.map (\f -> f == field)
        |> Maybe.withDefault False


{-| -}
setField : a -> FieldValue -> Form a -> Form a
setField field value (Form model) =
    Form { model | fields = updateFieldValue field value model.fields }


setStringField : a -> String -> Model a -> Model a
setStringField a value model =
    { model | fields = updateFieldValue a (fromString value) model.fields }


setIntField : Int -> a -> Form a -> Form a
setIntField value field (Form model) =
    Form { model | fields = updateFieldValue field (fromInt value) model.fields }


setBoolField : a -> Bool -> Model a -> Model a
setBoolField a value model =
    { model | fields = updateFieldValue a (fromBool value) model.fields }



-- internal functions


{-| -}
getField : a -> Fields a -> Maybe FieldState
getField a fields =
    fields
        |> List.filter (\( k, v ) -> k == a)
        |> List.head
        |> Maybe.map Tuple.second


updateFieldValue : a -> FieldValue -> Fields a -> Fields a
updateFieldValue a value fields =
    fields
        |> List.map
            (\( k, state ) ->
                if k == a then
                    ( k, { state | value = value } )

                else
                    ( k, state )
            )


updateFieldFocused : Bool -> a -> Fields a -> Fields a
updateFieldFocused flag a fields =
    fields
        |> List.map
            (\( k, state ) ->
                if k == a then
                    ( k, { state | focused = flag } )

                else
                    ( k, state )
            )


updateFieldFormat : a -> Fields a -> Fields a
updateFieldFormat a fields =
    fields
        |> List.map
            (\( k, state ) ->
                if k == a then
                    ( k, { state | value = state.parser state.value } )

                else
                    ( k, state )
            )



-- Update


type Msg a
    = HandleInput a String
    | HandleClick a String
    | HandleCheck a Bool
    | HandleFocus a
    | HandleBlur a
    | HandleChange a String
    | HandleSubmit


{-| -}
update : Msg a -> Form a -> Form a
update msg (Form model) =
    Form (updateInternal msg model)


updateInternal : Msg a -> Model a -> Model a
updateInternal msg model =
    case msg of
        HandleInput field value ->
            updateStringField field value model

        HandleClick field value ->
            updateStringField field value model

        HandleCheck field value ->
            let
                newModel =
                    setBoolField field value model

                errors =
                    Validator.errors model.validator (Form newModel)
            in
            { newModel | errors = errors }

        HandleChange field value ->
            updateStringField field value model

        HandleFocus a ->
            { model | focus = Just a }

        HandleBlur a ->
            let
                newModel =
                    { model | fields = model.fields |> updateFieldFocused True a |> updateFieldFormat a }
            in
            { newModel
                | errors = Validator.errors model.validator (Form newModel)
                , focus = Nothing
            }

        HandleSubmit ->
            { model
                | fields = List.map (\( k, state ) -> ( k, { state | focused = True } )) model.fields
                , focus = Nothing
                , errors = Validator.errors model.validator (Form model)
                , isSubmitted = True
            }


updateStringField : a -> String -> Model a -> Model a
updateStringField field value model =
    let
        newModel =
            setStringField field value model

        errors =
            Validator.errors model.validator (Form newModel)
    in
    { newModel | errors = errors }


validateHelper : a -> (a -> Form a -> b) -> Validator b String -> Validator (Form a) ( a, String )
validateHelper field getter v =
    Validator.liftMap (\e -> ( field, e )) (getter field) v


validateStringField : a -> Validator (Maybe String) String -> Validator (Form a) ( a, String )
validateStringField field validator =
    validateHelper field getFieldAsString validator


validateBoolField : a -> Validator (Maybe Bool) String -> Validator (Form a) ( a, String )
validateBoolField field validator =
    validateHelper field getFieldAsBool validator
