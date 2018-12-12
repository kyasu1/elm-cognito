module Form exposing (Form)

import Validate exposing (..)

type alias Model =
    List (Field, FieldState a)

type alias FieldState a =
    { touched : Bool
    , initial : a
    , current : a
    }


type alias Form error model =
    { model : model
    , errors : List error
    , validator : Validator error model
    }
type Validator a b =
    Validator (State)


validate : Validate (form -> model) model

type alias State value =
  { touched : Bool
  , errors : List String
  , value : value }

type Validator a b =
    Validator (State a -> State b)

type Parser a b =
    Parser (State a -> List (State b))

nonEmpty : Validator String -> String
nonEmpty =

string : Parser (String -> a) a
string =
    Parser (State (String ->a) -> List (State b))
