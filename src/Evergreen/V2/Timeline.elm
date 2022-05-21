module Evergreen.V2.Timeline exposing (..)

import AssocSet
import Evergreen.V2.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V2.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V2.Id.Id FrameId, state )
    , initialState : state
    }
