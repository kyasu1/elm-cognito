module Model exposing (..)

type Gender
  = Male
  | Female
  | Unknown

type Occupation
  = Student { college : Maybe String}

type Era
  = Meiji
  | Taisho
  | Showa
  | Heisei
  | Western

hToTime : Int -> Int -> Int -> Maybe Time.Posix
hToTime year month day =

type alias Birthday =
  { era : Era
  , year : Int
  , month : Int
  , day : Int
  }

type alias Customer =
  { name : Stringgt6vgv
  , kana : String
  , gender : Gender
  , birthday : Birthday
  , occupation : Occupation
  , address : List Address
  , mobile : Digits
  }
