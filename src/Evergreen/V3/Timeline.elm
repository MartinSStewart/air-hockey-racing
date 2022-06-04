module Evergreen.V3.Timeline exposing (..)

import AssocSet
import Evergreen.V3.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V3.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V3.Id.Id FrameId, state )
    , initialState : state
    }
