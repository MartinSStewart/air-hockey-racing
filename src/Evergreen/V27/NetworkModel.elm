module Evergreen.V27.NetworkModel exposing (..)

import Evergreen.V27.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V27.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V27.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
