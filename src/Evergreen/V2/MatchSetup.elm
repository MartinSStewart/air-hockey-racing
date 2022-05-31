module Evergreen.V2.MatchSetup exposing (..)

import AssocList
import Evergreen.V2.ColorIndex
import Evergreen.V2.Decal
import Evergreen.V2.Id
import Evergreen.V2.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V2.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V2.ColorIndex.ColorIndex
    | SetDecal Evergreen.V2.Decal.Decal


type alias PlayerData =
    { primaryColor : Evergreen.V2.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V2.ColorIndex.ColorIndex
    , decal : Evergreen.V2.Decal.Decal
    }


type alias MatchSetupData =
    { name : String
    , owner : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) PlayerData
    }


type MatchSetup
    = MatchSetup MatchSetupData
