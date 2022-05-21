module Evergreen.V4.Lobby exposing (..)

import AssocSet
import Evergreen.V4.Id
import Evergreen.V4.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V4.Id.Id Evergreen.V4.User.UserId
    , users : AssocSet.Set (Evergreen.V4.Id.Id Evergreen.V4.User.UserId)
    }


type Lobby
    = Lobby LobbyData
