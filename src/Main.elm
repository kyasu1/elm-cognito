module Main exposing (Msg(..), main, update, view)

import Api
import Base64
import Browser
import Browser.Navigation as Nav exposing (Key)
import Cognito
import Cognito.Session exposing (Session, decoderSession)
import Dashboard
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Page.Home as Home
import Ports
import Route exposing (Route)
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        sessionMaybe =
            Decode.decodeValue (Decode.field "session" decoderSession) value
                |> Result.toMaybe
    in
    changeRouteTo (Route.parseUrl url)
        { session = sessionMaybe, key = key, page = Loading }


type alias Model =
    { session : Maybe Session
    , key : Key
    , page : Page
    }


type Page
    = Cognito Cognito.State
    | Dashboard Dashboard.Model
    | Loading


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | GotJsToElm Ports.Data
    | CognitoMsg Cognito.Msg
    | DashboardMsg Dashboard.Msg
    | SessionUpdated Session
    | ClickedRefreshSession Session
    | GotRefreshSession Session (Result Api.Error Session)


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case model.session of
        Just session ->
            case route of
                Route.Home ->
                    let
                        ( initModel, cmd ) =
                            Dashboard.initHome session
                    in
                    ( { model | page = Dashboard initModel }, Cmd.map DashboardMsg cmd )

                Route.SignOut ->
                    ( { model | page = Loading, session = Nothing }
                    , Cmd.batch
                        [ Ports.elmToJs <|
                            { tag = "SignOut"
                            , value = Encode.null
                            }
                        , Route.replaceUrl model.key Route.SignIn
                        ]
                    )

                _ ->
                    let
                        ( initModel, cmd ) =
                            Dashboard.initHome session
                    in
                    ( { model | page = Dashboard initModel }, Cmd.map DashboardMsg cmd )

        Nothing ->
            case route of
                Route.SignUp ->
                    { model | page = Cognito Cognito.initSignUp } |> noCmd

                Route.SignIn ->
                    { model | page = Cognito Cognito.initSignIn } |> noCmd

                _ ->
                    ( model, Route.replaceUrl model.key Route.SignIn )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange url ->
            changeRouteTo (Route.parseUrl url) model

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        CognitoMsg cognitoMsg ->
            case model.page of
                Cognito stateCognito ->
                    let
                        ( newModel, cmd ) =
                            Cognito.update SessionUpdated CognitoMsg cognitoMsg stateCognito
                    in
                    ( { model | page = Cognito newModel }, cmd )

                _ ->
                    noCmd model

        SessionUpdated session ->
            ( { model | session = Just session }, Route.replaceUrl model.key Route.Home )

        ClickedRefreshSession session ->
            ( model, Api.refreshSession session (GotRefreshSession session) )

        GotRefreshSession oldSession (Ok session) ->
            let
                newSession =
                    { oldSession | accessToken = session.accessToken, idToken = session.idToken }
            in
            ( { model | session = Just newSession }, Cmd.none )

        GotRefreshSession oldSession (Err error) ->
            ( { model | session = Nothing }, Cmd.none )

        _ ->
            noCmd model


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Cognito cognitoModel ->
            Sub.map CognitoMsg (Cognito.subscriptions cognitoModel)

        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "AWS Cognito"
    , body =
        case model.page of
            Loading ->
                fullScreenLayout (Element.text "Loading")

            -- NotFound ->
            --   fullScreenLayout (Element.text "Not Found")
            Dashboard db ->
                Dashboard.view db ClickedRefreshSession

            Cognito cognito ->
                fullScreenLayout (Cognito.view cognito |> Element.map CognitoMsg)
    }


fullScreenLayout : Element Msg -> List (Html Msg)
fullScreenLayout content =
    [ layout []
        (column [ centerX, centerY ]
            [ content ]
        )
    ]
