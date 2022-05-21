module Evergreen.V3.Lobby exposing (..)

import AssocSet
import Evergreen.V3.Id
import Evergreen.V3.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , users : AssocSet.Set (Evergreen.V3.Id.Id Evergreen.V3.User.UserId)
    }


type Lobby
    = Lobby LobbyData
