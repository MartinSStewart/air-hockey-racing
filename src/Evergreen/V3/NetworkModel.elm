module Evergreen.V3.NetworkModel exposing (..)


type alias NetworkModel msg model =
    { localMsgs : List msg
    , serverState : model
    }
