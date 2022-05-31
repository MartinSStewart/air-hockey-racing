module Evergreen.V1.MatchSetup exposing (..)

import AssocList
import Evergreen.V1.ColorIndex
import Evergreen.V1.Decal
import Evergreen.V1.Id
import Evergreen.V1.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V1.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V1.ColorIndex.ColorIndex
    | SetDecal Evergreen.V1.Decal.Decal


type alias PlayerData =
    { primaryColor : Evergreen.V1.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V1.ColorIndex.ColorIndex
    , decal : Evergreen.V1.Decal.Decal
    }


type alias MatchSetupData =
    { name : String
    , owner : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) PlayerData
    }


type MatchSetup
    = MatchSetup MatchSetupData
