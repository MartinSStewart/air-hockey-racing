module Evergreen.V7.Lobby exposing (..)

import AssocSet
import Evergreen.V7.Id
import Evergreen.V7.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , users : AssocSet.Set (Evergreen.V7.Id.Id Evergreen.V7.User.UserId)
    }


type Lobby
    = Lobby LobbyData
