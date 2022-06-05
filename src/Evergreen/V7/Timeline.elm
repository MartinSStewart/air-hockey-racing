module Evergreen.V7.Timeline exposing (..)

import AssocSet
import Evergreen.V7.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V7.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V7.Id.Id FrameId, state )
    , initialState : state
    }
