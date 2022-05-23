module Evergreen.V6.Lobby exposing (..)

import AssocSet
import Evergreen.V6.Id
import Evergreen.V6.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V6.Id.Id Evergreen.V6.User.UserId
    , users : AssocSet.Set (Evergreen.V6.Id.Id Evergreen.V6.User.UserId)
    }


type Lobby
    = Lobby LobbyData
