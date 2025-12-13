module Id exposing (Id, decrement, fromInt, increment, toInt, toString)


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
    id + 1 |> Id


decrement : Id idType -> Id idType
decrement (Id id) =
    id - 1 |> Id


toString : Id idType -> String
toString (Id id) =
    String.fromInt id
