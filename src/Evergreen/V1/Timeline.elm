module Evergreen.V1.Timeline exposing (..)

import AssocSet
import Evergreen.V1.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V1.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V1.Id.Id FrameId, state )
    , initialState : state
    }
