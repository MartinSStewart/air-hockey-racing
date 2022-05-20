module Evergreen.V1.Lobby exposing (..)

import AssocSet
import Evergreen.V1.Id
import Evergreen.V1.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , users : AssocSet.Set (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
    }


type Lobby
    = Lobby LobbyData
