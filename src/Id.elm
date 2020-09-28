module Id exposing (Id, fromInt, toInt)

import Dict


type Id idType
    = Id Int


fromInt : Int -> Id idType
fromInt =
    Id


toInt : Id idType -> Int
toInt (Id id) =
    id
