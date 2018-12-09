module Main exposing (Msg(..), main, update, view)

import Browser
import Cognito
import Cognito.Session exposing (Session, sessionDecoder)
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as E exposing (Value)
import Ports


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Ports.jsToElm GotJsToElm
        }


init : Value -> ( Model, Cmd Msg )
init value =
    case Decoder.decodeValue (Decoder.field "session" sessionDecoder) value of
        Ok session ->
            ( { session = Just session, cognito = Cognito.initSignIn }, Cmd.none )

        _ ->
            ( { session = Nothing, cognito = Cognito.initSignIn }, Cmd.none )


type alias Model =
    { session : Maybe Session
    , cognito : Cognito.State
    }


type Msg
    = GotJsToElm Ports.Data
    | CognitoMsg Cognito.Msg
    | SessionUpdated Session


update msg model =
    case msg of
        CognitoMsg cognitoMsg ->
            let
                ( newModel, cmd ) =
                    Cognito.update cognitoMsg model.cognito
            in
            ( { model | cognito = newModel }, Cmd.map CognitoMsg cmd )
        GotJsToElm data ->

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "test"
    , body =
        [ layout []
            (column [ width fill, height fill, centerX, centerY ]
                [ case model.session of
                    Just session ->
                        column []
                            [ text "YOU ARE LOGGED IN"
                            , text <| Debug.toString session
                            ]

                    Nothing ->
                        Cognito.view model.cognito |> Element.map CognitoMsg
                ]
            )
        ]
    }
