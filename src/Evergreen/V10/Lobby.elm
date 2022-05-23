module Evergreen.V10.Lobby exposing (..)

import AssocSet
import Evergreen.V10.Id
import Evergreen.V10.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
    , users : AssocSet.Set (Evergreen.V10.Id.Id Evergreen.V10.User.UserId)
    }


type Lobby
    = Lobby LobbyData
