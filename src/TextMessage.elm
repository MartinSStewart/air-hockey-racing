module TextMessage exposing
    ( Error(..)
    , TextMessage(..)
    , fromString
    , toString
    )

import String.Nonempty exposing (NonemptyString)


type TextMessage
    = TextMessage NonemptyString


type Error
    = MessageEmpty
    | MessageTooLong


maxLength : number
maxLength =
    400


fromString : String -> Result Error TextMessage
fromString text =
    let
        trimmed =
            String.trim text
    in
    if String.length trimmed > maxLength then
        Err MessageTooLong

    else
        case String.Nonempty.fromString text of
            Just nonempty ->
                Ok (TextMessage nonempty)

            Nothing ->
                Err MessageEmpty


toString : TextMessage -> String
toString (TextMessage message) =
    String.Nonempty.toString message
