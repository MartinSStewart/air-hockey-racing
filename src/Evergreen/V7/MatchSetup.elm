module Evergreen.V7.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V7.ColorIndex
import Evergreen.V7.Decal
import Evergreen.V7.Direction2d
import Evergreen.V7.Id
import Evergreen.V7.MatchName
import Evergreen.V7.Point2d
import Evergreen.V7.Timeline
import Evergreen.V7.User
import Evergreen.V7.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V7.MatchName.MatchName
    , userCount : Int
    }


type WorldCoordinate
    = WorldCoordinate Never


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V7.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V7.ColorIndex.ColorIndex
    | SetDecal Evergreen.V7.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch Time.Posix
    | MatchInputRequest (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId) (Maybe (Evergreen.V7.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V7.MatchName.MatchName


type alias PlayerData =
    { primaryColor : Evergreen.V7.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V7.ColorIndex.ColorIndex
    , decal : Evergreen.V7.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , input : Maybe (Evergreen.V7.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : Time.Posix
    , timeline : Evergreen.V7.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V7.MatchName.MatchName
    , owner : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) PlayerData
    , match : Maybe Match
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V7.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V7.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Maybe (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
    , lastCollision : Maybe (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) Player
    }
