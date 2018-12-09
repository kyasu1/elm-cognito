module Dashboard exposing (Model, Msg, initHome, update, view)

import Api
import Cognito.Session as Session exposing (Session)
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Page.Home as Home
import Route


type alias Model =
    { session : Session
    , page : Page
    }


initHome : Session -> ( Model, Cmd Msg )
initHome session =
    ( { session = session, page = Home }, Cmd.none )


type Page
    = Home


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



--
-- type alias Model =
--     {}
--
--
-- type Msg
--     = ClickedRefreshToken Session
--     | GotRefreshToken (Result Api.Error Session)
--
--
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         ClickedRefreshToken session ->
--             ( model, Api.refreshSession session GotRefreshToken )
--
--         GotRefreshToken (Ok session) ->
--             ( model, Cmd.none )
--
--         GotRefreshToken (Err error) ->
--             ( model, Cmd.none )


view : Model -> (Session -> msg) -> List (Html msg)
view model toMsg =
    [ layout []
        (column []
            [ row [ padding 10, spacing 10 ]
                [ text "HEADER"
                , Input.button [] { onPress = Just (toMsg model.session), label = text "更新" }
                , link []
                    { url = Route.pathFor Route.SignOut
                    , label = text "サインアウト"
                    }
                ]
            , row [] [ text "BODY" ]
            ]
        )
    ]
