module Evergreen.V10.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V10.ColorIndex
import Evergreen.V10.Decal
import Evergreen.V10.Direction2d
import Evergreen.V10.Id
import Evergreen.V10.MatchName
import Evergreen.V10.Point2d
import Evergreen.V10.TextMessage
import Evergreen.V10.Timeline
import Evergreen.V10.User
import Evergreen.V10.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V10.MatchName.MatchName
    , userCount : Int
    }


type WorldCoordinate
    = WorldCoordinate Never


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V10.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V10.ColorIndex.ColorIndex
    | SetDecal Evergreen.V10.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch Time.Posix
    | MatchInputRequest (Evergreen.V10.Id.Id Evergreen.V10.Timeline.FrameId) (Maybe (Evergreen.V10.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V10.MatchName.MatchName
    | SendTextMessage Evergreen.V10.TextMessage.TextMessage


type alias PlayerData =
    { primaryColor : Evergreen.V10.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V10.ColorIndex.ColorIndex
    , decal : Evergreen.V10.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
    , input : Maybe (Evergreen.V10.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : Time.Posix
    , timeline : Evergreen.V10.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V10.MatchName.MatchName
    , owner : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V10.Id.Id Evergreen.V10.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
            , message : Evergreen.V10.TextMessage.TextMessage
            }
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V10.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V10.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V10.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Maybe (Evergreen.V10.Id.Id Evergreen.V10.Timeline.FrameId)
    , lastCollision : Maybe (Evergreen.V10.Id.Id Evergreen.V10.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V10.Id.Id Evergreen.V10.User.UserId) Player
    }
