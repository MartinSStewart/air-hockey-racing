module Evergreen.V4.Timeline exposing (..)

import AssocSet
import Evergreen.V4.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V4.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V4.Id.Id FrameId, state )
    , initialState : state
    }
