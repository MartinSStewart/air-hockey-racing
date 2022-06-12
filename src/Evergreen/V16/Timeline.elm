module Evergreen.V16.Timeline exposing (..)

import AssocSet
import Evergreen.V16.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V16.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V16.Id.Id FrameId, state )
    , initialState : state
    }
