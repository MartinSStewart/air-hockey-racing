module Evergreen.V25.Timeline exposing (..)

import AssocSet
import Evergreen.V25.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V25.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List ( Evergreen.V25.Id.Id FrameId, state )
    , initialState : state
    }
