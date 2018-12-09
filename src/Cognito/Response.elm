module Cognito.Response exposing (ErrorCode, Response(..), decode, errorDecoder)

import Json.Decode as Decode exposing (Decoder, field, map, map3, oneOf, string)
import Json.Encode as Encode exposing (Value)


type alias ErrorCode =
    { code : String
    , name : String
    , message : String
    }


type CognitoError
    = WithCode ErrorCode
    | Single String


type Response a
    = Error CognitoError
    | Success a


errorDecoder : Decoder CognitoError
errorDecoder =
    oneOf
        [ map3 ErrorCode (field "code" string) (field "name" string) (field "message" string)
            |> map WithCode
        , map Single string
        ]


responseDecoder : Decoder a -> Decoder (Response a)
responseDecoder decoder =
    Decode.oneOf
        [ decoder |> Decode.map Success
        , errorDecoder |> Decode.map Error
        ]


decode :
    Decoder a
    -> Value
    ->
        { success : a -> ( model, Cmd msg )
        , error : String -> ( model, Cmd msg )
        , failure : String -> ( model, Cmd msg )
        }
    -> ( model, Cmd msg )
decode decoder value { success, error, failure } =
    case Decode.decodeValue (responseDecoder decoder) value of
        Ok (Success response) ->
            success response

        Ok (Error (WithCode e)) ->
            error (codeToString e.code)

        Ok (Error (Single message)) ->
            error message

        Err e ->
            failure "aa"


codeToString : String -> String
codeToString string =
    case string of
        "NetworkError" ->
            "ネットワークエラー"

        "UserNotFoundException" ->
            "ユーザー名またはパスワードに誤りがあります"

        "NotAuthorizedException" ->
            "ユーザー名またはパスワードに誤りがあります"

        _ ->
            "不明なエラー"
