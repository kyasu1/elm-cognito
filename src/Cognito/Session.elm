module Cognito.Session exposing
    ( Session
    , decoderAuthenticationResult
    , decoderSession
    , getRefreshToken
    , isValid
    )

import Base64
import Json.Decode as Decode exposing (Decoder, bool, field, int, map, map2, map4, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Task exposing (Task)
import Time


getAccessToken : Session -> String
getAccessToken session =
    session.accessToken.jwtToken


getRefreshToken : Session -> String
getRefreshToken session =
    session.refreshToken.token


isValid : Session -> Task x Bool
isValid session =
    Time.now
        |> Task.andThen
            (\time ->
                let
                    adjusted =
                        ((time |> Time.posixToMillis |> toFloat) / 1000 |> floor) - session.clockDrift
                in
                Task.succeed (adjusted < session.accessToken.payload.exp && adjusted < session.idToken.payload.exp)
            )



--


type alias CognitoAccessTokenPayload =
    { authTime : Int
    , clientId : String
    , eventId : String
    , exp : Int
    , iat : Int
    , iss : String
    , jti : String
    , scope : String
    , sub : String
    , tokenUse : String
    , username : String
    }


cognitoAccessTokenPayloadDecoder : Decoder CognitoAccessTokenPayload
cognitoAccessTokenPayloadDecoder =
    succeed CognitoAccessTokenPayload
        |> required "auth_time" int
        |> required "client_id" string
        |> required "event_id" string
        |> required "exp" int
        |> required "iat" int
        |> required "iss" string
        |> required "jti" string
        |> required "scope" string
        |> required "sub" string
        |> required "token_use" string
        |> required "username" string


type alias CognitoAccessToken =
    { jwtToken : String
    , payload : CognitoAccessTokenPayload
    }


cognitoAccessTokenDecoder : Decoder CognitoAccessToken
cognitoAccessTokenDecoder =
    map2 CognitoAccessToken
        (field "jwtToken" string)
        (field "payload" cognitoAccessTokenPayloadDecoder)


type alias CognitoIdTokenPayload =
    { aud : String
    , authTime : Int
    , cognitoUsername : String
    , email : String
    , emailVerified : Bool
    , eventId : String
    , exp : Int
    , iat : Int
    , iss : String
    , sub : String
    , token_use : String
    }


cognitoIdTokenPayloadDecoder : Decoder CognitoIdTokenPayload
cognitoIdTokenPayloadDecoder =
    succeed CognitoIdTokenPayload
        |> required "aud" string
        |> required "auth_time" int
        |> required "cognito:username" string
        |> required "email" string
        |> required "email_verified" bool
        |> required "event_id" string
        |> required "exp" int
        |> required "iat" int
        |> required "iss" string
        |> required "sub" string
        |> required "token_use" string


type alias CognitoIdToken =
    { jwtToken : String
    , payload : CognitoIdTokenPayload
    }


cognitoIdTokenDecoder : Decoder CognitoIdToken
cognitoIdTokenDecoder =
    map2 CognitoIdToken
        (field "jwtToken" string)
        (field "payload" cognitoIdTokenPayloadDecoder)


type alias CognitoRefreshToken =
    { token : String }


cognitoRefreshTokenDecoder : Decoder CognitoRefreshToken
cognitoRefreshTokenDecoder =
    map CognitoRefreshToken
        (field "token" string)


type alias Session =
    { accessToken : CognitoAccessToken
    , clockDrift : Int
    , idToken : CognitoIdToken
    , refreshToken : CognitoRefreshToken
    }


decoderSession : Decoder Session
decoderSession =
    map4 Session
        (field "accessToken" cognitoAccessTokenDecoder)
        (field "clockDrift" int)
        (field "idToken" cognitoIdTokenDecoder)
        (field "refreshToken" cognitoRefreshTokenDecoder)



--


type alias AuthenticationResult =
    { accessToken : String
    , expiresIn : Int
    , idToken : String
    , tokenType : String
    }


base64Decode : String -> String
base64Decode token =
    token
        |> String.split "."
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault ""
        |> Base64.decode
        |> Result.withDefault ""


decoderAuthenticationResult : Decoder Session
decoderAuthenticationResult =
    Decode.field "AuthenticationResult" <|
        Decode.map4 Session
            (Decode.field "AccessToken" Decode.string
                |> Decode.andThen
                    (\token ->
                        case Decode.decodeString cognitoAccessTokenPayloadDecoder (base64Decode token) of
                            Ok payload ->
                                Decode.succeed (CognitoAccessToken token payload)

                            Err e ->
                                Decode.fail "Failed to decode accessToken"
                    )
            )
            (Decode.succeed 0)
            (Decode.field "IdToken" Decode.string
                |> Decode.andThen
                    (\token ->
                        case Decode.decodeString cognitoIdTokenPayloadDecoder (base64Decode token) of
                            Ok payload ->
                                Decode.succeed (CognitoIdToken token payload)

                            Err e ->
                                Decode.fail "Failed to decode idToken"
                    )
            )
            (Decode.field "TokenType" Decode.string |> Decode.map CognitoRefreshToken)
