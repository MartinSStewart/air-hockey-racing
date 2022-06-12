module Evergreen.V15.NetworkModel exposing (..)

import Evergreen.V15.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V15.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V15.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
