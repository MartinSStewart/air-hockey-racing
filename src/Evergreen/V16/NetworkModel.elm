module Evergreen.V16.NetworkModel exposing (..)

import Evergreen.V16.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V16.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V16.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
