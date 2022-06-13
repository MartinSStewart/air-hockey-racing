module Evergreen.V27.Timeline exposing (..)

import AssocSet
import Evergreen.V27.Id


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V27.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List ( Evergreen.V27.Id.Id FrameId, state )
    , initialState : state
    }
