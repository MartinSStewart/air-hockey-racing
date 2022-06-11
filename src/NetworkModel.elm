module NetworkModel exposing (EventId, NetworkModel, init, localState, updateFromBackend, updateFromUser)

import Id exposing (Id)


type alias NetworkModel msg model =
    { idCounter : Id EventId
    , localMsgs : List { id : Id EventId, msg : msg }
    , serverState : model
    }


type EventId
    = EventId Never


init : model -> NetworkModel msg model
init model =
    { idCounter = Id.fromInt 0, localMsgs = [], serverState = model }


updateFromUser : msg -> NetworkModel msg model -> { eventId : Id EventId, newNetworkModel : NetworkModel msg model }
updateFromUser msg localModel =
    { eventId = localModel.idCounter
    , newNetworkModel =
        { localMsgs = localModel.localMsgs ++ [ { id = localModel.idCounter, msg = msg } ]
        , serverState = localModel.serverState
        , idCounter = Id.increment localModel.idCounter
        }
    }


localState : (msg -> model -> model) -> NetworkModel msg model -> model
localState updateFunc localModel =
    List.foldl (\{ msg } model -> updateFunc msg model) localModel.serverState localModel.localMsgs


updateFromBackend : (msg -> model -> model) -> Maybe (Id EventId) -> msg -> NetworkModel msg model -> NetworkModel msg model
updateFromBackend updateFunc maybeEventId msg localModel =
    { localMsgs =
        case maybeEventId of
            Just eventId ->
                List.filter (\{ id } -> id /= eventId) localModel.localMsgs

            Nothing ->
                localModel.localMsgs
    , serverState = updateFunc msg localModel.serverState
    , idCounter = localModel.idCounter
    }
