module Api exposing
    ( Error
    , SignUpResponse
    , clientId
    , confirmSignUp
    , graphql
    , refreshSession
    , signUp
    , userPoolId
    )

import Base64
import Cognito.Session as Session exposing (Session)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


graphql =
    "https://ghrraynmxnczvmqdeh66i2t5fm.appsync-api.ap-northeast-1.amazonaws.com/graphql"


userPoolId =
    "ap-northeast-1_5lHoc28av"


clientId =
    "4aajp1e5cuqj9hooej2689q0ju"


region =
    String.split "_" userPoolId
        |> List.head
        |> Maybe.withDefault ""


endpoint =
    "https://cognito-idp." ++ region ++ ".amazonaws.com"



-- Cognito Api


signUp :
    { username : String, password : String, email : String }
    -> (Result Error SignUpResponse -> msg)
    -> Cmd msg
signUp { username, password, email } tagger =
    let
        body =
            Encode.object
                [ ( "ClientId", Encode.string clientId )
                , ( "Username", Encode.string username )
                , ( "Password", Encode.string password )
                , ( "UserAttributes"
                  , Encode.list
                        (\( k, v ) ->
                            Encode.object
                                [ ( "Name", Encode.string k )
                                , ( "Value", Encode.string v )
                                ]
                        )
                        [ ( "email", email ) ]
                  )
                , ( "ValidationData", Encode.null )
                ]
                |> Encode.encode 0

        headers =
            [ Http.header "X-Amz-Target" "AWSCognitoIdentityProviderService.SignUp"
            , Http.header "X-Amz-User-Agent" "aws-amplify/0.1.x js"
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = endpoint
        , body = Http.stringBody "application/x-amz-json-1.1" body
        , expect = expectJson tagger decoderSignUpResponse
        , timeout = Nothing
        , tracker = Nothing
        }


confirmSignUp :
    { username : String, code : String }
    -> (Result Error String -> msg)
    -> Cmd msg
confirmSignUp { username, code } toMsg =
    let
        body =
            Encode.object
                [ ( "ClientId", Encode.string clientId )
                , ( "Username", Encode.string username )
                , ( "ConfirmationCode", Encode.string code )
                , ( "ForceAliasCreation", Encode.bool False )
                ]
                |> Encode.encode 0

        headers =
            [ Http.header "X-Amz-Target" "AWSCognitoIdentityProviderService.ConfirmSignUp"
            , Http.header "X-Amz-User-Agent" "aws-amplify/0.1.x js"
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = endpoint
        , body = Http.stringBody "application/x-amz-json-1.1" body
        , expect = expectJson toMsg Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }


refreshSession : Session -> (Result Error Session -> msg) -> Cmd msg
refreshSession session toMsg =
    let
        body =
            Encode.object
                [ ( "ClientId", Encode.string clientId )
                , ( "AuthFlow", Encode.string "REFRESH_TOKEN_AUTH" )
                , ( "AuthParameters"
                  , Encode.object
                        [ ( "REFRESH_TOKEN", Encode.string (Session.getRefreshToken session) )
                        ]
                  )
                ]
                |> Encode.encode 0

        headers =
            [ Http.header "X-Amz-Target" "AWSCognitoIdentityProviderService.InitiateAuth"
            , Http.header "X-Amz-User-Agent" "aws-amplify/0.1.x js"
            ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = endpoint
        , body = Http.stringBody "application/x-amz-json-1.1" body
        , expect = expectJson toMsg Session.decoderAuthenticationResult
        , timeout = Nothing
        , tracker = Nothing
        }



-- Decoder for SignUpResponse


type alias SignUpResponse =
    { codeDeliveryDetails : CodeDeliveryDetails
    , userConfirmed : Bool
    , userSub : String
    }


type alias CodeDeliveryDetails =
    { attributeName : String
    , deliveryMedium : String
    , destination : String
    }


decoderSignUpResponse : Decoder SignUpResponse
decoderSignUpResponse =
    Decode.map3 SignUpResponse
        (Decode.field "CodeDeliveryDetails" decoderCodeDeliveryDetails)
        (Decode.field "UserConfirmed" Decode.bool)
        (Decode.field "UserSub" Decode.string)


decoderCodeDeliveryDetails : Decoder CodeDeliveryDetails
decoderCodeDeliveryDetails =
    Decode.map3 CodeDeliveryDetails
        (Decode.field "AttributeName" Decode.string)
        (Decode.field "DeliveryMedium" Decode.string)
        (Decode.field "Destination" Decode.string)



-- Error Handling


type Error
    = HttpError Http.Error
    | CognitoError CognitoException


type CognitoException
    = InvalidPasswordException String
    | InvalidParameterException String
    | UsernameExistsException String


decoderException : Decoder CognitoException
decoderException =
    Decode.map2 Tuple.pair
        (Decode.field "__type" Decode.string)
        (Decode.field "message" Decode.string)
        |> Decode.andThen
            (\( string, message ) ->
                case string of
                    "InvalidPasswordException" ->
                        Decode.succeed (InvalidPasswordException message)

                    "InvalidParameterException" ->
                        Decode.succeed (InvalidParameterException message)

                    "UsernameExistsException" ->
                        Decode.succeed (UsernameExistsException message)

                    _ ->
                        Decode.fail "Invalid CognitoException"
            )


expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url |> HttpError)

                Http.Timeout_ ->
                    Err (Http.Timeout |> HttpError)

                Http.NetworkError_ ->
                    Err (Http.NetworkError |> HttpError)

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString decoderException body |> Debug.log "BadStatus : " of
                        Ok value ->
                            Err (CognitoError value)

                        Err err ->
                            Err (Http.BadStatus metadata.statusCode |> HttpError)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err) |> HttpError)
