module Id exposing (Id, decrement, fromInt, toInt)


type Id idType
    = Id Int


fromInt : Int -> Id idType
fromInt =
    Id


toInt : Id idType -> Int
toInt (Id id) =
    id


decrement : Id idType -> Id idType
decrement (Id id) =
    id - 1 |> Id
