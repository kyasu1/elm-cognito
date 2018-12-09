module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Cognito.Session exposing (Session)
import Element exposing (..)
import Ports
import Route


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Session -> Model -> Element Msg
view session model =
    text "HOME"
