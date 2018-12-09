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
import Cognito.Session as Session exposing (Session)
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports
import Task


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
    | SignOut


update : (Session -> parentMsg) -> (Msg -> parentMsg) -> Msg -> State -> ( State, Cmd parentMsg )
update onSessionMsg toParentMsg msg state =
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
            case Decode.decodeValue Session.decoderSession value of
                Ok session ->
                    ( state, Task.succeed (onSessionMsg session) |> Task.perform identity )

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
                |> Cmd.map toParentMsg
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
                |> Cmd.map toParentMsg
            )

        GotConfirmSignUp (Ok response) ->
            -- ( state, Route.replaceUrl model.key Route.SignIn )
            ( state, Cmd.none )

        GotConfirmSignUp (Err e) ->
            ( state, Cmd.none )

        SignOut ->
            ( state
            , Ports.elmToJs <|
                { tag = "SignOut"
                , value = Encode.null
                }
            )


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
    column
        [ spacing 20 ]
        [ column [ spacing 10 ]
            [ Input.username
                [ Border.color <| Color.dark Color.red
                , Background.color <| Color.lighter Color.yellow
                ]
                { onChange = \s -> ChangedSignIn { model | username = s }
                , text = model.username
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.size 16 ] (text "ユーザー名")
                }
            , el
                [ Font.size 12
                , Font.color <| Color.dark Color.red
                ]
              <|
                text "ユーザー名を入力してください"
            ]
        , column [ spacing 10 ]
            [ Input.currentPassword []
                { onChange = \s -> ChangedSignIn { model | password = s }
                , text = model.password
                , placeholder = Nothing
                , label = Input.labelAbove [ Font.size 16 ] (text "パスワード")
                , show = False
                }
            , el
                [ Font.size 12
                , Font.color <| Color.dark Color.red
                ]
              <|
                text ""
            ]
        , Input.button
            [ centerX
            , Font.color <| Color.white

            -- , Border.solid
            -- , Border.width 2
            -- , Border.color <| Color.dark Color.teal
            , Border.rounded 4
            , Background.color <| Color.dark Color.teal
            , Font.size 16
            , padding 8
            , pointer
            , mouseOver
                [ Background.color <| Color.light Color.teal
                ]
            ]
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
        [ spacing 10 ]
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
