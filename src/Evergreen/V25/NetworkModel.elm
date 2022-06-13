module Evergreen.V25.NetworkModel exposing (..)

import Evergreen.V25.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V25.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V25.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
