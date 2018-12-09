port module Ports exposing
    ( Data
    , elmToJs
    , encode
    , jsToElm
    )

import Json.Encode as Encode exposing (Value)


type alias Data =
    { tag : String
    , value : Value
    }


encode : Data -> Value
encode { tag, value } =
    Encode.object
        [ ( "tag", Encode.string tag )
        , ( "value", value )
        ]


port elmToJs : Data -> Cmd msg


port jsToElm : (Data -> msg) -> Sub msg
