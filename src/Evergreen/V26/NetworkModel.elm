module Evergreen.V26.NetworkModel exposing (..)

import Evergreen.V26.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V26.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V26.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
