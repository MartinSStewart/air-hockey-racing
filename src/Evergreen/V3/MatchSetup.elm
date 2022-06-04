module Evergreen.V3.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V3.ColorIndex
import Evergreen.V3.Decal
import Evergreen.V3.Direction2d
import Evergreen.V3.Id
import Evergreen.V3.Point2d
import Evergreen.V3.Timeline
import Evergreen.V3.User
import Evergreen.V3.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : String
    , userCount : Int
    }


type WorldCoordinate
    = WorldCoordinate Never


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V3.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V3.ColorIndex.ColorIndex
    | SetDecal Evergreen.V3.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch Time.Posix
    | MatchInputRequest (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId) (Maybe (Evergreen.V3.Direction2d.Direction2d WorldCoordinate))


type alias PlayerData =
    { primaryColor : Evergreen.V3.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V3.ColorIndex.ColorIndex
    , decal : Evergreen.V3.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , input : Maybe (Evergreen.V3.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : Time.Posix
    , timeline : Evergreen.V3.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : String
    , owner : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) PlayerData
    , match : Maybe Match
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V3.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V3.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Maybe (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
    , lastCollision : Maybe (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) Player
    }
