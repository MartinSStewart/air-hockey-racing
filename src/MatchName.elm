module MatchName exposing
    ( Error(..)
    , MatchName(..)
    , empty
    , fromString
    , toString
    )


type MatchName
    = MatchName String


type Error
    = MatchNameTooLong


empty : MatchName
empty =
    MatchName ""


maxLength : number
maxLength =
    40


fromString : String -> Result Error MatchName
fromString text =
    let
        trimmed =
            String.trim text
    in
    if String.length trimmed > maxLength then
        Err MatchNameTooLong

    else
        Ok (MatchName trimmed)


toString : MatchName -> String
toString (MatchName groupName) =
    groupName
