module Evergreen.V28.Timeline exposing (..)

import AssocSet
import Evergreen.V28.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V28.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List ( Evergreen.V28.Id.Id FrameId, state )
    , initialState : state
    }
