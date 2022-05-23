module Evergreen.V6.Timeline exposing (..)

import AssocSet
import Evergreen.V6.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V6.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V6.Id.Id FrameId, state )
    , initialState : state
    }
