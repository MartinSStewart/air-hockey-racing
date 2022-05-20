module Timeline exposing (FrameId, Timeline, TimelineCache, addInput, addInput_, getStateAt, init)

import AssocSet as Set exposing (Set)
import Id exposing (Id)
import List.Extra as List


type FrameId
    = FrameId Never


type alias TimelineCache state =
    { cache : List ( Id FrameId, state )
    , initialState : state
    }


type alias Timeline input =
    Set ( Id FrameId, input )


init : state -> TimelineCache state
init initialState =
    { cache = []
    , initialState = initialState
    }


addInput : Id FrameId -> input -> TimelineCache state -> Timeline input -> ( TimelineCache state, Timeline input )
addInput frame input timelineCache timeline =
    if Id.toInt frame < 0 then
        ( timelineCache, timeline )

    else
        ( { cache =
                List.filter
                    (\( cacheFrame, _ ) -> cacheFrame |> isAfter frame |> not)
                    timelineCache.cache
          , initialState = timelineCache.initialState
          }
        , Set.insert ( frame, input ) timeline
        )


addInput_ : Id FrameId -> input -> Timeline input -> Timeline input
addInput_ frame input timeline =
    if Id.toInt frame < 0 then
        timeline

    else
        Set.insert ( frame, input ) timeline


isBefore : Id FrameId -> Id FrameId -> Bool
isBefore a b =
    Id.toInt b < Id.toInt a


isAfter : Id FrameId -> Id FrameId -> Bool
isAfter a b =
    Id.toInt b > Id.toInt a


getStateAt :
    (List input -> state -> state)
    -> Id FrameId
    -> TimelineCache state
    -> Timeline input
    -> ( TimelineCache state, state )
getStateAt updateFunc frame timelineCache timeline =
    let
        ( startFrame, startState ) =
            timelineCache.cache
                |> List.filter (\( cacheFrame, _ ) -> cacheFrame |> isAfter frame |> not)
                |> List.maximumBy (Tuple.first >> Id.toInt)
                |> Maybe.withDefault ( Id.fromInt 0, timelineCache.initialState )

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
                            newState =
                                updateFunc input state
                        in
                        ( ( Id.fromInt (frameId + 1), newState ) :: cache, newState )
                    )
                    ( [], startState )
    in
    ( { timelineCache
        | cache =
            newCache
                ++ timelineCache.cache
                |> List.sortBy (Tuple.first >> Id.toInt >> negate)
                |> List.take 60
      }
    , finalState
    )
