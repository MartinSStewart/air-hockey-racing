module Evergreen.V12.MatchSetup exposing (..)

import AssocList
import Evergreen.V12.ColorIndex
import Evergreen.V12.Decal
import Evergreen.V12.Id
import Evergreen.V12.User


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V12.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V12.ColorIndex.ColorIndex
    | SetDecal Evergreen.V12.Decal.Decal


type alias PlayerData =
    { primaryColor : Evergreen.V12.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V12.ColorIndex.ColorIndex
    , decal : Evergreen.V12.Decal.Decal
    }


type alias MatchSetupData =
    { name : String
    , owner : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) PlayerData
    }


type MatchSetup
    = MatchSetup MatchSetupData
