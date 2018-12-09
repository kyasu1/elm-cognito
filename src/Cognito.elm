module Cognito exposing (Msg, State, initSignIn, update, view)

import Cognito.SignIn as SignInForm
import Element exposing (Element)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports


type State
    = SignIn String String
    | SignUp String String


initSignIn : State
initSignIn =
    SignIn "" ""


type Msg
    = SignInUsername String
    | SignInPassword String
    | SignInClicked
    | JsToElm Ports.Data


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case state of
        SignIn username password ->
            case msg of
                SignInUsername string ->
                    ( SignIn string password, Cmd.none )

                SignInPassword string ->
                    ( SignIn username string, Cmd.none )

                SignInClicked ->
                    ( state
                    , Ports.elmToJs <|
                        { tag = "SignIn"
                        , value =
                            Encode.object
                                [ ( "username", Encode.string username )
                                , ( "password", Encode.string password )
                                ]
                        }
                    )
                JsToElm data ->
                  if data.tag == "SignIn" then
                    case Decode.decodeValue Session.sessionDecoder data.value of
                      Ok session ->


        SignUp username password ->
            ( SignUp username password, Cmd.none )


view : State -> Element Msg
view state =
    case state of
        SignIn username password ->
            SignInForm.view { username = username, password = password }
                { changedUsername = SignInUsername
                , changedPassword = SignInPassword
                , clickedSubmit = SignInClicked
                }

        _ ->
            Element.none
