module Evergreen.V11.Timeline exposing (..)

import AssocSet
import Evergreen.V11.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V11.Id.Id FrameId, input )


type alias TimelineCache state =
    { cache : List ( Evergreen.V11.Id.Id FrameId, state )
    , initialState : state
    }
