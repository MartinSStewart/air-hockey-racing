module Evergreen.V2.Lobby exposing (..)

import AssocSet
import Evergreen.V2.Id
import Evergreen.V2.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type alias LobbyData =
    { name : String
    , owner : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , users : AssocSet.Set (Evergreen.V2.Id.Id Evergreen.V2.User.UserId)
    }


type Lobby
    = Lobby LobbyData
