module IdDict exposing
    ( IdDict
    , diff
    , empty
    , filter
    , fromList
    , get
    , insert
    , isEmpty
    , keys
    , map
    , member
    , remove
    , singleton
    , size
    , toList
    , update
    )

import Dict exposing (Dict)
import Id exposing (Id)


type IdDict idType value
    = IdDict (Dict Int value)


diff : IdDict idType a -> IdDict idType b -> IdDict idType a
diff (IdDict dict0) (IdDict dict1) =
    Dict.diff dict0 dict1 |> IdDict


empty : IdDict idType v
empty =
    IdDict Dict.empty


filter : (Id idType -> value -> Bool) -> IdDict idType value -> IdDict idType value
filter isGood (IdDict dict) =
    Dict.filter (\key value -> isGood (Id.fromInt key) value) dict |> IdDict


fromList : List ( Id idType, value ) -> IdDict idType value
fromList =
    List.map (Tuple.mapFirst Id.toInt) >> Dict.fromList >> IdDict


get : Id idType -> IdDict idType value -> Maybe value
get key (IdDict dict) =
    Dict.get (Id.toInt key) dict


insert : Id idType -> value -> IdDict idType value -> IdDict idType value
insert key value (IdDict dict) =
    Dict.insert (Id.toInt key) value dict |> IdDict


isEmpty : IdDict idType value -> Bool
isEmpty (IdDict dict) =
    Dict.isEmpty dict


map : (Id idType -> a -> b) -> IdDict idType a -> IdDict idType b
map mapFunc (IdDict dict) =
    Dict.map (\key value -> mapFunc (Id.fromInt key) value) dict |> IdDict


member : Id idType -> IdDict idType value -> Bool
member key (IdDict dict) =
    Dict.member (Id.toInt key) dict


remove : Id idType -> IdDict idType value -> IdDict idType value
remove key (IdDict dict) =
    Dict.remove (Id.toInt key) dict |> IdDict


singleton : Id idType -> value -> IdDict idType value
singleton key value =
    Dict.singleton (Id.toInt key) value |> IdDict


size : IdDict idType value -> Int
size (IdDict dict) =
    Dict.size dict


toList : IdDict idType b -> List ( Id idType, b )
toList (IdDict dict) =
    Dict.toList dict |> List.map (Tuple.mapFirst Id.fromInt)


update : Id idType -> (Maybe value -> Maybe value) -> IdDict idType value -> IdDict idType value
update key mapFunc (IdDict dict) =
    Dict.update (Id.toInt key) mapFunc dict |> IdDict


keys : IdDict idType a -> List (Id idType)
keys (IdDict dict) =
    Dict.keys dict |> List.map Id.fromInt
