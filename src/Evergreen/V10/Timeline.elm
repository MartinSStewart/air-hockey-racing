module Evergreen.V10.Timeline exposing (..)

import AssocSet
import Evergreen.V10.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V10.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V10.Id.Id FrameId, state )
    , initialState : state
    }
