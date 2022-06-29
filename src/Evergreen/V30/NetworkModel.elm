module Evergreen.V30.NetworkModel exposing (..)

import Evergreen.V30.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V30.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V30.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
