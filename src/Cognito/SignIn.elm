module Cognito.SignIn exposing (view)

import Element exposing (..)
import Element.Events as Events
import Element.Input as Input

type FormState
    = Valid String
    | Pristine String
    | Invalid String String

updateFormState : String -> FormState -> FormState
updateFormState string formState =
     caes formState of
       Valid s ->
         Valid string
        Pristine s ->
          Valid string
        Invalid s err ->
          Valid

type alias Model =
    { username : FormState
    , password : FormState
    }


type Msg
    = ChangedUsername String
    | ChangedPassword String
    | LoseFocusUsername String
    | ClickedSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUsername string ->
            ( { model | username = string }, Cmd.none )

        ChangedPassword string ->
            ( { model | password = string }, Cmd.none )

        LoseFocusUsername string ->
            ({ model | username = if string /= "" then Valid string else Invalid string "Must not be empty"})
        ClickedSubmit ->
            ( model, Cmd.none )


view :
    { username : String, password : String }
    ->
        { changedUsername : String -> msg
        , changedPassword : String -> msg
        , clickedSubmit : msg
        }
    -> Element msg
view { username, password } { changedUsername, changedPassword, clickedSubmit } =
    Element.column
        []
        [ Input.username [ Events.onLoseFocus LoseFocusUsername]
            { onChange = changedUsername
            , text = username
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "ユーザー名")
            }
        , Input.currentPassword []
            { onChange = changedPassword
            , text = password
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "パスワード")
            , show = False
            }
        , Input.button []
            { onPress = Just clickedSubmit
            , label = text "ログイン"
            }
        ]
