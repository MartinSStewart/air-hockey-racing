module Evergreen.V12.NetworkModel exposing (..)

import Evergreen.V12.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V12.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V12.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
