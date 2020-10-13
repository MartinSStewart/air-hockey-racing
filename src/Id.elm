module Id exposing (Id, fromInt, increment, toInt)


type Id idType
    = Id Int


fromInt : Int -> Id idType
fromInt =
    Id


toInt : Id idType -> Int
toInt (Id id) =
    id


increment : Id idType -> Id idType
increment (Id id) =
    Id (id + 1)
