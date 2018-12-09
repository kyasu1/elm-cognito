module Cognito exposing
    ( Msg
    , State
    , initSignIn
    , initSignUp
    , subscriptions
    , update
    , view
    )

import Api
import Cognito.Session as Session
import Element exposing (Element, text)
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports


type alias SignInModel =
    { username : String
    , password : String
    }


type alias SignUpModel =
    { username : String
    , password : String
    , email : String
    }


type alias ConfirmSignUpModel =
    { username : String
    , code : String
    }


type State
    = SignIn SignInModel
    | SignUp SignUpModel
    | ConfirmSignUp ConfirmSignUpModel


initSignIn : State
initSignIn =
    SignIn { username = "", password = "" }


initSignUp : State
initSignUp =
    SignUp { username = "", password = "", email = "" }


type Msg
    = ChangedSignIn SignInModel
    | ClickedSignIn SignInModel
    | ChangedSignUp SignUpModel
    | ClickedSignUp SignUpModel
    | ChangedConfirmSignUp ConfirmSignUpModel
    | ClickedConfirmSignUp ConfirmSignUpModel
    | GotSignUp String (Result Api.Error Api.SignUpResponse)
    | GotConfirmSignUp (Result Api.Error String)
    | GotSignIn Value


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ChangedSignIn newModel ->
            ( SignIn newModel, Cmd.none )

        ClickedSignIn newModel ->
            ( state
            , Ports.elmToJs <|
                { tag = "SignIn"
                , value =
                    Encode.object
                        [ ( "username", Encode.string newModel.username )
                        , ( "password", Encode.string newModel.password )
                        ]
                }
            )

        GotSignIn value ->
            case Decode.decodeValue Api.decoderAuthenticationResult value |> Debug.log "GotSignIn" of
                Ok session ->
                    ( state, Cmd.none )

                Err e ->
                    ( state, Cmd.none )

        ChangedSignUp newModel ->
            ( SignUp newModel, Cmd.none )

        ClickedSignUp newModel ->
            ( state
            , Api.signUp
                { username = newModel.username
                , password = newModel.password
                , email = newModel.email
                }
                (GotSignUp newModel.username)
            )

        GotSignUp username (Ok response) ->
            ( ConfirmSignUp { username = username, code = "" }, Cmd.none )

        GotSignUp _ (Err e) ->
            let
                _ =
                    Debug.log "GotSignUp Error : " e
            in
            ( state, Cmd.none )

        ChangedConfirmSignUp newModel ->
            ( ConfirmSignUp newModel, Cmd.none )

        ClickedConfirmSignUp newModel ->
            ( state
            , Api.confirmSignUp
                { username = newModel.username
                , code = newModel.code
                }
                GotConfirmSignUp
            )

        GotConfirmSignUp (Ok response) ->
            ( state, Cmd.none )

        GotConfirmSignUp (Err e) ->
            ( state, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions state =
    Ports.signInResponse GotSignIn


view : State -> Element Msg
view state =
    case state of
        SignIn model ->
            viewSignIn model

        SignUp model ->
            viewSignUp model

        ConfirmSignUp model ->
            viewConfirmSignUp model


viewSignIn : SignInModel -> Element Msg
viewSignIn model =
    Element.column
        []
        [ Input.username []
            { onChange = \s -> ChangedSignIn { model | username = s }
            , text = model.username
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "ユーザー名")
            }
        , Input.currentPassword []
            { onChange = \s -> ChangedSignIn { model | password = s }
            , text = model.password
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "パスワード")
            , show = False
            }
        , Input.button []
            { onPress = Just (ClickedSignIn model)
            , label = text "ログイン"
            }
        ]


viewSignUp : SignUpModel -> Element Msg
viewSignUp model =
    Element.column
        []
        [ Input.username []
            { onChange = \s -> ChangedSignUp { model | username = s }
            , text = model.username
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "ユーザー名")
            }
        , Input.currentPassword []
            { onChange = \s -> ChangedSignUp { model | password = s }
            , text = model.password
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "パスワード")
            , show = False
            }
        , Input.email []
            { onChange = \s -> ChangedSignUp { model | email = s }
            , text = model.email
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "メールアドレス")
            }
        , Input.button []
            { onPress = Just (ClickedSignUp model)
            , label = text "サインアップ"
            }
        ]


viewConfirmSignUp : ConfirmSignUpModel -> Element Msg
viewConfirmSignUp model =
    Element.column
        []
        [ Element.column []
            [ text "ユーザー名"
            , text model.username
            ]
        , Input.text []
            { onChange = \s -> ChangedConfirmSignUp { model | code = s }
            , text = model.code
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "確認コード")
            }
        , Input.button []
            { onPress = Just (ClickedConfirmSignUp model)
            , label = text "コードの送信"
            }
        ]
