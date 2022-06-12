module Evergreen.V15.Timeline exposing (..)

import AssocSet
import Evergreen.V15.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V15.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V15.Id.Id FrameId, state )
    , initialState : state
    }
