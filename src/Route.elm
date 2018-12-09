module Route exposing
    ( Route(..)
    , href
    , parseUrl
    , parser
    , pathFor
    , replaceUrl
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = SignIn
    | SignUp
    | SignOut
    | Home
    | NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map SignUp (s "signup")
        , map SignIn (s "signin")
        , map SignOut (s "signout")
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse parser url of
        Just route ->
            route

        Nothing ->
            NotFound


pathFor : Route -> String
pathFor route =
    case route of
        SignIn ->
            "/signin"

        SignOut ->
            "/signout"

        SignUp ->
            "/signup"

        Home ->
            "/"

        NotFound ->
            "/"


href : Route -> Attribute msg
href targetRoute =
    Attributes.href (pathFor targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (pathFor route)
