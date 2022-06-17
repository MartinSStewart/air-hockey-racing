module Timeline exposing (Error(..), FrameId, Timeline, TimelineCache, addInput, getStateAt, init, maxCacheSize)

import AssocSet as Set exposing (Set)
import Id exposing (Id)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))


type FrameId
    = FrameId Never


type alias TimelineCache state =
    { cache : Nonempty ( Id FrameId, state ) }


type alias Timeline input =
    Set ( Id FrameId, input )


init : state -> TimelineCache state
init initialState =
    { cache = Nonempty ( Id.fromInt -1, initialState ) [] }


type Error
    = InputTooOld


addInput : Id FrameId -> TimelineCache state -> Result Error (TimelineCache state)
addInput frame timelineCache =
    let
        newCache =
            List.Nonempty.toList timelineCache.cache
                |> List.filter (\( cacheFrame, _ ) -> cacheFrame |> isAfter frame |> not)
                |> List.Nonempty.fromList
    in
    case newCache of
        Just nonemptyCache ->
            Ok { cache = nonemptyCache }

        Nothing ->
            Err InputTooOld


isAfter : Id FrameId -> Id FrameId -> Bool
isAfter a b =
    Id.toInt b > Id.toInt a


getStateAt :
    (Id FrameId -> List input -> state -> state)
    -> Id FrameId
    -> TimelineCache state
    -> Timeline input
    -> Result Error ( TimelineCache state, state )
getStateAt updateFunc frame timelineCache timeline =
    case
        List.Nonempty.toList timelineCache.cache
            |> List.filter (\( cacheFrame, _ ) -> cacheFrame |> isAfter frame |> not)
            |> List.maximumBy (Tuple.first >> Id.toInt)
    of
        Just ( startFrame, startState ) ->
            let
                ( newCache, finalState ) =
                    List.range (Id.toInt startFrame) (Id.toInt frame - 1)
                        |> List.map
                            (\frameId ->
                                ( frameId
                                , Set.toList timeline
                                    |> List.filter (Tuple.first >> Id.toInt >> (==) frameId)
                                    |> List.map Tuple.second
                                )
                            )
                        |> List.foldl
                            (\( frameId, input ) ( cache, state ) ->
                                let
                                    newState : state
                                    newState =
                                        updateFunc (Id.fromInt frameId) input state
                                in
                                ( if modBy 1 frameId == 0 then
                                    ( Id.fromInt (frameId + 1), newState ) :: cache

                                  else
                                    cache
                                , newState
                                )
                            )
                            ( [], startState )
            in
            ( { cache =
                    newCache
                        ++ List.Nonempty.toList timelineCache.cache
                        |> List.sortBy (Tuple.first >> Id.toInt >> negate)
                        |> List.take maxCacheSize
                        |> List.Nonempty.fromList
                        |> Maybe.withDefault timelineCache.cache
              }
            , finalState
            )
                |> Ok

        Nothing ->
            Err InputTooOld


maxCacheSize =
    180
