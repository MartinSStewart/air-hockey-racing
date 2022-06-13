module Evergreen.V26.Timeline exposing (..)

import AssocSet
import Evergreen.V26.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V26.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List ( Evergreen.V26.Id.Id FrameId, state )
    , initialState : state
    }
