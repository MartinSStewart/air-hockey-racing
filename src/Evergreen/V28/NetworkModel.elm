module Evergreen.V28.NetworkModel exposing (..)

import Evergreen.V28.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V28.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V28.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
