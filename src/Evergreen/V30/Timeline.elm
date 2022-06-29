module Evergreen.V30.Timeline exposing (..)

import AssocSet
import Evergreen.V30.Id
import List.Nonempty


type FrameId
    = FrameId Never


type alias Timeline input =
    AssocSet.Set ( Evergreen.V30.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List.Nonempty.Nonempty ( Evergreen.V30.Id.Id FrameId, state )
    }
