module Evergreen.V11.NetworkModel exposing (..)


type alias NetworkModel msg model =
    { localMsgs : List msg
    , serverState : model
    }
